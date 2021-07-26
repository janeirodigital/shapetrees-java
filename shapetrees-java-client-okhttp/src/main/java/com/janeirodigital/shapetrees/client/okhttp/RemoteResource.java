package com.janeirodigital.shapetrees.client.okhttp;

import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.core.enums.LinkRelations;
import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;
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
import java.util.Set;

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
    protected final Set<String> supportedRDFContentTypes = Set.of("text/turtle", "application/rdf+xml", "application/n-triples", "application/ld+json");

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

    public String getName() {

        String path = this.uri.getPath();

        if (this.uri.getPath().equals("/")) return "/";

        // if this is a container, trim the trailing slash
        if (this.uri.getPath().endsWith("/")) {
            path = path.substring(0, path.length() - 1);
        }

        int pathIndex = path.lastIndexOf('/');

        // No slashes in the path
        if (pathIndex == -1) {
            return path;
        }

        return path.substring(path.lastIndexOf('/') + 1);

    }

    public ShapeTreeResourceType getResourceType() throws IOException {

        if (isContainer()) {
            return ShapeTreeResourceType.CONTAINER;
        }

        if (isRdfResource()) {
            return ShapeTreeResourceType.RESOURCE;
        } else {
            return ShapeTreeResourceType.NON_RDF;
        }

    }

    public Boolean isRdfResource() throws IOException {
        String contentType = this.getFirstHeaderByName(HttpHeaders.CONTENT_TYPE.getValue().toLowerCase());
        if (contentType != null) {
            return this.supportedRDFContentTypes.contains(contentType);
        } else {
            return false;
        }
    }

    public Boolean isNonRdfSource() throws IOException {
        return isRdfResource() ? false : true;
    }

    public Boolean isContainer() {
        String uriPath = this.uri.getPath();

        return uriPath.endsWith("/");
    }

    public Boolean isMetadata() throws IOException {
        // If the resource has an HTTP Link header of type of https://www.w3.org/ns/shapetrees#ShapeTreeLocator
        // with a metadata target, it is not a metadata resource (because it is pointing to one)
        if (this.exists() && this.parsedLinkHeaders != null && this.parsedLinkHeaders.containsKey(LinkRelations.SHAPETREE_LOCATOR.getValue())) {
            return false;
        }
        // If the resource doesn't exist, currently we need to do some inference based on the URI
        if (this.getUri().getPath() != null && this.getUri().getPath().matches(".*\\.shapetree$")) { return true; }
        if (this.getUri().getQuery() != null && this.getUri().getQuery().matches(".*ext\\=shapetree$")) { return true; }

        return false;
    }

    public Boolean isManaged() throws IOException {

        // If there is a metadata resource that
        if (this.getMetadataResource(this.authorizationHeaderValue).exists()) {
            return true;
        }
        return false;

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

    // Return the resource URI directly associated with a given resource
    public URI getAssociatedURI() throws IOException {
        // If metadata - it is primary uri
        // If not metadata - it is metadata uri
        if (this.isMetadata()) {

            // If this implementation uses a dot notation for meta, trim it from the path
            String basePath = this.getUri().getPath().replaceAll("\\.shapetree$", "");

            // Rebuild without the query string in case that was employed
            String associatedString = this.getUri().getScheme() + "://" + this.getUri().getAuthority() + basePath;

            return URI.create(associatedString);

        } else {
            return URI.create(getMetadataURI());
        }
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
