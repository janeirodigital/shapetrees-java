package com.janeirodigital.shapetrees;

import com.janeirodigital.shapetrees.client.impl.ShapeTreeClientConfiguration;
import com.janeirodigital.shapetrees.client.impl.ShapeTreeHttpClientHolder;
import com.janeirodigital.shapetrees.enums.HttpHeaders;
import com.janeirodigital.shapetrees.enums.LinkRelations;
import com.janeirodigital.shapetrees.helper.GraphHelper;
import com.janeirodigital.shapetrees.helper.HttpHeaderHelper;
import lombok.extern.slf4j.Slf4j;
import okhttp3.*;
import org.apache.jena.graph.Graph;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.io.StringWriter;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Slf4j
public class RemoteResource {

    private static final String REL_TYPE = "type";
    private static final String LDP_CONTAINER = "http://www.w3.org/ns/ldp#Container";
    private static final String LDP_BASIC_CONTAINER = "http://www.w3.org/ns/ldp#BasicContainer";

    private final URI URI;
    private String authorizationHeaderValue;
    private Boolean invalidated = false;
    private Boolean exists;
    private Map<String, List<String>> responseHeaders;
    private Map<String, List<String>> parsedLinkHeaders;
    private Graph parsedGraph;
    private String rawBody;
    private final ShapeTreeClientConfiguration clientConfiguration = new ShapeTreeClientConfiguration(null, false, false);

    public RemoteResource(String uriString, String authorizationHeaderValue) throws IOException {
        URI requestUri;
        try {
            requestUri = new URI(uriString);
        } catch (URISyntaxException ex) {
            throw new IOException("Request URI is not a value URI");
        }
        this.URI = requestUri;
        this.authorizationHeaderValue = authorizationHeaderValue;
        dereferenceURI();
    }

    public RemoteResource(URI uri, String authorizationHeaderValue) throws IOException {
        this.URI = uri;
        this.authorizationHeaderValue = authorizationHeaderValue;
        dereferenceURI();
    }

    public RemoteResource(Response response) throws IOException, URISyntaxException {
        this.parseResponseToRemoteResource(response);
        this.URI = new URI(getFirstHeaderByName("Location"));
    }

    public URI getURI() throws IOException {
        if (this.invalidated) {
            dereferenceURI();
        }
        return this.URI;
    }

    public Boolean exists() {
        return this.exists;
    }

    public String getBody() throws IOException {
        if (!this.exists) return null;

        if (this.invalidated) {
            log.debug("RemoteResource#getBody({}) - Resource Invalidated - Refreshing", this.URI);
            dereferenceURI();
        }

        return this.rawBody;
    }

    // Lazy-load graph when requested
    public Graph getGraph(URI baseURI) throws IOException {
        if (!this.exists) return null;

        if (this.invalidated) {
            log.debug("RemoteResource#getGraph({}) - Resource Invalidated - Refreshing", this.URI);
            dereferenceURI();
        }

        if (this.parsedGraph == null) {
            this.parsedGraph = GraphHelper.readStringIntoGraph(this.rawBody, baseURI, getFirstHeaderByName(HttpHeaders.CONTENT_TYPE.getValue()));
        }
        return this.parsedGraph;
    }

    public Boolean isContainer() {
        if (!this.exists()) {
            // If this is a framented URI, remove the fragment to handle cases where it is a container
            // with a container graph
            String uri = this.URI.toString();
            if (uri.contains("#")) {
                uri = uri.substring(0, uri.indexOf("#"));
            }

            return uri.endsWith("/");
        }

        if (this.parsedLinkHeaders != null && this.parsedLinkHeaders.get(REL_TYPE) != null) {
            return this.parsedLinkHeaders.get(REL_TYPE).contains(LDP_CONTAINER) ||
                    this.parsedLinkHeaders.get(REL_TYPE).contains(LDP_BASIC_CONTAINER);
        }
        return false;
    }

    public Map<String, List<String>> getLinkHeaders() {
        return this.parsedLinkHeaders;
    }

    public String getFirstHeaderByName(String headerName) throws IOException {
        if (this.invalidated) {
            log.debug("RemoteResource#getFirstHeaderByName({}) - Resource Invalidated - Refreshing", this.URI);
            dereferenceURI();
        }

        List<String> headerValues = responseHeaders.get(headerName);
        if (headerValues == null) {
            return null;
        }

        return headerValues.get(0);
    }

    public void updateGraph(Graph updatedGraph, Boolean refreshResourceAfterUpdate, String authorizationHeaderValue) throws IOException {
        log.debug("RemoteResource#updateGraph({})", this.URI);

        if (this.invalidated) {
            throw new ShapeTreeException(500, "Cannot call 'updateGraph' on an invalidated RemoteResource - ");
        }

        StringWriter sw = new StringWriter();
        RDFDataMgr.write(sw, updatedGraph, Lang.TURTLE);

        OkHttpClient httpClient = ShapeTreeHttpClientHolder.getForConfig(this.clientConfiguration);
        Request.Builder requestBuilder = new Request.Builder()
                .url(this.URI.toURL())
                .addHeader(HttpHeaders.CONTENT_TYPE.getValue(), "text/turtle")
                .put(RequestBody.create(sw.toString(), MediaType.get("text/turtle")));

        if (authorizationHeaderValue != null) {
            requestBuilder.addHeader(HttpHeaders.AUTHORIZATION.getValue(), authorizationHeaderValue);
        }

        httpClient.newCall(requestBuilder.build()).execute();

        if (refreshResourceAfterUpdate) {
            dereferenceURI();
        } else {
            this.invalidated = true;
            log.debug("RemoteResource#updateGraph({}) - Invalidating Resource", this.URI);
        }
    }

    public RemoteResource getMetadataResource(String authorizationHeaderValue) throws IOException {
        return new RemoteResource(this.getMetadataURI(), authorizationHeaderValue);
    }

    @NotNull
    public String getMetadataURI() throws IOException {
        if (!this.parsedLinkHeaders.containsKey(LinkRelations.SHAPETREE.getValue())) {
            log.error("The resource {} does not contain a link header of {}", this.getURI(), LinkRelations.SHAPETREE.getValue());
            throw new ShapeTreeException(500, "No Link header with relation of " + LinkRelations.SHAPETREE.getValue() + " found");
        }

        String metaDataURIString = this.parsedLinkHeaders.get(LinkRelations.SHAPETREE.getValue()).stream().findFirst().orElse(null);

        if (metaDataURIString == null) {
            throw new ShapeTreeException(500, "No Link header with relation of " + LinkRelations.SHAPETREE.getValue() + " found");
        }

        if (metaDataURIString.startsWith("/")) {
            // If the header value doesn't include scheme/host, prefix it with the scheme & host from container
            URI shapeTreeContainerURI = this.getURI();
            String portFragment;
            if (shapeTreeContainerURI.getPort() > 0) {
                portFragment = ":" + shapeTreeContainerURI.getPort();
            } else {
                portFragment = "";
            }
            metaDataURIString = shapeTreeContainerURI.getScheme() + "://" + shapeTreeContainerURI.getHost() + portFragment + metaDataURIString;
        }

        // Check to see if the primary resource (this) has a scheme mismatch (https / http), and upgrade
        // to https if so. This addresses issues where the solid server is behind an SSL proxy, using non-https
        // links internally.
        if (metaDataURIString.startsWith("http") && this.getURI().getScheme().startsWith("https")) {
            log.debug("Upgrading metadata URI {} from http to https due to mismatch with primary resource URI {}",
                    metaDataURIString, this.getURI().toString());
            metaDataURIString = metaDataURIString.replaceFirst("http", "https");
        }

        return metaDataURIString;
    }

    private void dereferenceURI() throws IOException {
        log.debug("RemoteResource#dereferencingURI({})", this.URI);

        OkHttpClient httpClient = ShapeTreeHttpClientHolder.getForConfig(this.clientConfiguration);
        Request.Builder requestBuilder = new Request.Builder()
                .url(this.URI.toURL());

        if (this.authorizationHeaderValue != null) {
            requestBuilder.addHeader(HttpHeaders.AUTHORIZATION.getValue(), this.authorizationHeaderValue);
        }

        Request request = requestBuilder.build();

        try (Response response = httpClient.newCall(request).execute()) {
            parseResponseToRemoteResource(response);
            this.invalidated = false;
        } catch (Exception e) {
            log.error("Error dereferencing URI", e);
        }
    }

    private void parseResponseToRemoteResource(Response response) throws IOException {
        this.exists = response.code() < 400;

        // Parse the headers for ease of use later
        this.responseHeaders = response.headers().toMultimap();

        // We especially care about Link headers which require extra parsing of the rel values
        if (this.responseHeaders.get(HttpHeaders.LINK.getValue()) != null) {
            this.parsedLinkHeaders = HttpHeaderHelper.parseLinkHeadersToMap(response.headers(HttpHeaders.LINK.getValue()));
        } else {
            this.parsedLinkHeaders = new HashMap<>();
        }

        // Save raw body
        try (ResponseBody body = response.body()) {
            if (body != null) {
                this.rawBody = body.string();
            }
        }
    }
}
