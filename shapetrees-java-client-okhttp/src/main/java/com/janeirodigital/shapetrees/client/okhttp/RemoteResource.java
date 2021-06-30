package com.janeirodigital.shapetrees.client.okhttp;

import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.core.enums.LinkRelations;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.helpers.GraphHelper;
import com.janeirodigital.shapetrees.core.helpers.HttpHeaderHelper;
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

/**
 * Convenience class that encapsulates an OkHttp-based http client to quickly retrieve
 * web resources with convenient methods to access headers, link headers and the body as a graph
 */
@Slf4j
public class RemoteResource {

    private final URI uri;
    private final String authorizationHeaderValue;
    private Boolean invalidated = false;
    private Boolean exists;
    private Map<String, List<String>> responseHeaders;
    private Map<String, List<String>> parsedLinkHeaders;
    private Graph parsedGraph;
    private String rawBody;
    private final ShapeTreeClientConfiguration clientConfiguration = new ShapeTreeClientConfiguration(false, false);

    public RemoteResource(String uriString, String authorizationHeaderValue) throws IOException {
        URI requestUri;
        try {
            requestUri = new URI(uriString);
        } catch (URISyntaxException ex) {
            throw new IOException("Request URI is not a value URI");
        }
        this.uri = requestUri;
        this.authorizationHeaderValue = authorizationHeaderValue;
        dereferenceURI();
    }

    public RemoteResource(URI uri, String authorizationHeaderValue) throws IOException {
        this.uri = uri;
        this.authorizationHeaderValue = authorizationHeaderValue;
        dereferenceURI();
    }

    public URI getUri() throws IOException {
        if (Boolean.TRUE.equals(this.invalidated)) {
            dereferenceURI();
        }
        return this.uri;
    }

    public Boolean exists() {
        return this.exists;
    }

    public String getBody() throws IOException {
        if (Boolean.FALSE.equals(this.exists)) return null;

        if (Boolean.TRUE.equals(this.invalidated)) {
            log.debug("RemoteResource#getBody({}) - Resource Invalidated - Refreshing", this.uri);
            dereferenceURI();
        }

        return this.rawBody;
    }

    // Lazy-load graph when requested
    public Graph getGraph(URI baseURI) throws IOException {
        if (Boolean.FALSE.equals(this.exists)) return null;

        if (Boolean.TRUE.equals(this.invalidated)) {
            log.debug("RemoteResource#getGraph({}) - Resource Invalidated - Refreshing", this.uri);
            dereferenceURI();
        }

        if (this.parsedGraph == null) {
            this.parsedGraph = GraphHelper.readStringIntoGraph(baseURI, this.rawBody, getFirstHeaderByName(HttpHeaders.CONTENT_TYPE.getValue()));
        }
        return this.parsedGraph;
    }

    public Boolean isContainer() {
        String uriPath = this.uri.toString();
        if (uriPath.contains("#")) {
            uriPath = uriPath.substring(0, uriPath.indexOf("#"));
        }

        return uriPath.endsWith("/");
    }

    public Map<String, List<String>> getResponseHeaders() { return this.responseHeaders; }

    public Map<String, List<String>> getLinkHeaders() {
        return this.parsedLinkHeaders;
    }

    public String getFirstHeaderByName(String headerName) throws IOException {
        if (Boolean.TRUE.equals(this.invalidated)) {
            log.debug("RemoteResource#getFirstHeaderByName({}) - Resource Invalidated - Refreshing", this.uri);
            dereferenceURI();
        }

        List<String> headerValues = responseHeaders.get(headerName);
        if (headerValues == null) {
            return null;
        }

        return headerValues.get(0);
    }

    public void updateGraph(Graph updatedGraph, Boolean refreshResourceAfterUpdate, String authorizationHeaderValue) throws IOException {
        log.debug("RemoteResource#updateGraph({})", this.uri);

        if (Boolean.TRUE.equals(this.invalidated)) {
            throw new ShapeTreeException(500, "Cannot call 'updateGraph' on an invalidated RemoteResource - ");
        }

        StringWriter sw = new StringWriter();
        RDFDataMgr.write(sw, updatedGraph, Lang.TURTLE);

        OkHttpClient httpClient = ShapeTreeHttpClientHolder.getForConfig(this.clientConfiguration);
        Request.Builder requestBuilder = new Request.Builder()
                .url(this.uri.toURL())
                .addHeader(HttpHeaders.CONTENT_TYPE.getValue(), "text/turtle")
                .put(RequestBody.create(sw.toString(), MediaType.get("text/turtle")));

        if (authorizationHeaderValue != null) {
            requestBuilder.addHeader(HttpHeaders.AUTHORIZATION.getValue(), authorizationHeaderValue);
        }

        httpClient.newCall(requestBuilder.build()).execute();

        if (Boolean.TRUE.equals(refreshResourceAfterUpdate)) {
            dereferenceURI();
        } else {
            this.invalidated = true;
            log.debug("RemoteResource#updateGraph({}) - Invalidating Resource", this.uri);
        }
    }

    public RemoteResource getMetadataResource(String authorizationHeaderValue) throws IOException {
        return new RemoteResource(this.getMetadataURI(), authorizationHeaderValue);
    }

    @NotNull
    public String getMetadataURI() throws IOException {
        if (!this.parsedLinkHeaders.containsKey(LinkRelations.SHAPETREE_LOCATOR.getValue())) {
            log.error("The resource {} does not contain a link header of {}", this.getUri(), LinkRelations.SHAPETREE_LOCATOR.getValue());
            // TODO: Should this really be a 500? What if shape trees aren't supported by server?
            throw new ShapeTreeException(500, "No Link header with relation of " + LinkRelations.SHAPETREE_LOCATOR.getValue() + " found");
        }
        String metaDataURIString = this.parsedLinkHeaders.get(LinkRelations.SHAPETREE_LOCATOR.getValue()).stream().findFirst().orElse(null);
        if (metaDataURIString != null && metaDataURIString.startsWith("/")) {
            // If the header value doesn't include scheme/host, prefix it with the scheme & host from container
            URI shapeTreeContainerURI = this.getUri();
            String portFragment;
            if (shapeTreeContainerURI.getPort() > 0) {
                portFragment = ":" + shapeTreeContainerURI.getPort();
            } else {
                portFragment = "";
            }
            metaDataURIString = shapeTreeContainerURI.getScheme() + "://" + shapeTreeContainerURI.getHost() + portFragment + metaDataURIString;
        }

        if (metaDataURIString == null) {
            // TODO: Is a 500 necessary when shape trees aren't supported?
            throw new ShapeTreeException(500, "No Link header with relation of " + LinkRelations.SHAPETREE_LOCATOR.getValue() + " found");
        }

        return metaDataURIString;
    }

    private void dereferenceURI() throws IOException {
        log.debug("RemoteResource#dereferencingURI({})", this.uri);

        OkHttpClient httpClient = ShapeTreeHttpClientHolder.getForConfig(this.clientConfiguration);
        Request.Builder requestBuilder = new Request.Builder()
                .url(this.uri.toURL());

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
