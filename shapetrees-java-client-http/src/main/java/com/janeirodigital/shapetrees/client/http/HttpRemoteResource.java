package com.janeirodigital.shapetrees.client.http;

import com.janeirodigital.shapetrees.core.ResourceAttributes;
import com.janeirodigital.shapetrees.core.enums.LinkRelations;
import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.helpers.GraphHelper;
import lombok.extern.slf4j.Slf4j;
import org.apache.jena.graph.Graph;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.io.StringWriter;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Set;

/**
 * Convenience class that encapsulates an OkHttp-based http client to quickly retrieve
 * web resources with convenient methods to access headers, link headers and the body as a graph
 */
@Slf4j
public class HttpRemoteResource {

    private static final String TEXT_TURTLE = "text/turtle";
    private static final String APP_RDF_XML = "application/rdf+xml";
    private static final String APP_N3 = "application/n-triples";
    private static final String APP_LD_JSON = "application/ld+json";

    private final URI uri;
    private final String authorizationHeaderValue;
    private Boolean invalidated = false;
    private Boolean exists;
    private ResourceAttributes responseHeaders;
    private ResourceAttributes parsedLinkHeaders;
    private Graph parsedGraph;
    private String rawBody;
    protected final Set<String> supportedRDFContentTypes = Set.of(TEXT_TURTLE, APP_RDF_XML, APP_N3, APP_LD_JSON);

    public HttpRemoteResource(String uriString, String authorizationHeaderValue) throws IOException {
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

    public HttpRemoteResource(URI uri, String authorizationHeaderValue) throws IOException {
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
            log.debug("HttpRemoteResource#getBody({}) - Resource Invalidated - Refreshing", this.uri);
            dereferenceURI();
        }

        return this.rawBody;
    }

    // Lazy-load graph when requested
    public Graph getGraph(URI baseURI) throws IOException {
        if (Boolean.FALSE.equals(this.exists)) return null;

        if (Boolean.TRUE.equals(this.invalidated)) {
            log.debug("HttpRemoteResource#getGraph({}) - Resource Invalidated - Refreshing", this.uri);
            dereferenceURI();
        }

        if (this.parsedGraph == null) {
            this.parsedGraph = GraphHelper.readStringIntoGraph(baseURI, this.rawBody, getFirstHeaderByName(com.janeirodigital.shapetrees.core.enums.HttpHeaders.CONTENT_TYPE.getValue()));
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

        if (Boolean.TRUE.equals(isContainer())) {
            return ShapeTreeResourceType.CONTAINER;
        }

        if (Boolean.TRUE.equals(isRdfResource())) {
            return ShapeTreeResourceType.RESOURCE;
        } else {
            return ShapeTreeResourceType.NON_RDF;
        }

    }

    public Boolean isRdfResource() throws IOException {
        String contentType = this.getFirstHeaderByName(com.janeirodigital.shapetrees.core.enums.HttpHeaders.CONTENT_TYPE.getValue().toLowerCase());
        if (contentType != null) {
            return this.supportedRDFContentTypes.contains(contentType);
        } else {
            return false;
        }
    }

    public Boolean isNonRdfSource() throws IOException {
        return !isRdfResource();
    }

    public Boolean isContainer() {
        String uriPath = this.uri.getPath();
        // Some implementations use the query string to denote auxiliary metadata for containers or resources
        // In the event a query string is present on a container, ensure that the auxiliary resource doesn't
        // get mistaken as a container when shortened to the URI path
        return (this.uri.getQuery() == null && uriPath.endsWith("/"));
    }

    public Boolean isMetadata() throws IOException {
        // If the resource has an HTTP Link header of type of https://www.w3.org/ns/shapetrees#ShapeTreeLocator
        // with a metadata target, it is not a metadata resource (because it is pointing to one)
        if (Boolean.TRUE.equals(this.exists()) && this.parsedLinkHeaders != null && !this.parsedLinkHeaders.firstValue(LinkRelations.SHAPETREE_LOCATOR.getValue()).isEmpty()) {
            return false;
        }
        // If the resource doesn't exist, currently we need to do some inference based on the URI
        if (this.getUri().getPath() != null && this.getUri().getPath().matches(".*\\.shapetree$")) { return true; }
        if (this.getUri().getQuery() != null && this.getUri().getQuery().matches(".*ext\\=shapetree$")) { return true; }

        return false;
    }

    public Boolean isManaged() throws IOException {

        if (Boolean.TRUE.equals(this.isMetadata())) { return false; }

        if (Boolean.TRUE.equals(this.getMetadataResource(this.authorizationHeaderValue).exists())) { return true; }

        return false;

    }

    public ResourceAttributes getResponseHeaders() { return this.responseHeaders; }

    public ResourceAttributes getLinkHeaders() {
        return this.parsedLinkHeaders;
    }

    public String getFirstHeaderByName(String headerName) throws IOException {
        if (Boolean.TRUE.equals(this.invalidated)) {
            log.debug("HttpRemoteResource#getFirstHeaderByName({}) - Resource Invalidated - Refreshing", this.uri);
            dereferenceURI();
        }

        return responseHeaders.firstValue(headerName).orElse(null);
    }

    public void updateGraph(Graph updatedGraph, Boolean refreshResourceAfterUpdate, String authorizationHeaderValue) throws IOException {
        log.debug("HttpRemoteResource#updateGraph({})", this.uri);

        if (Boolean.TRUE.equals(this.invalidated)) {
            throw new ShapeTreeException(500, "Cannot call 'updateGraph' on an invalidated HttpRemoteResource - ");
        }

        StringWriter sw = new StringWriter();
        RDFDataMgr.write(sw, updatedGraph, Lang.TURTLE);

        HttpClient fetcher = AbstractHttpClientFactory.getFactory().get(false);
        ResourceAttributes headers = new ResourceAttributes(com.janeirodigital.shapetrees.core.enums.HttpHeaders.AUTHORIZATION.getValue(), authorizationHeaderValue);
        fetcher.fetchShapeTreeResponse(new HttpRequest("PUT", this.uri, headers, sw.toString(), TEXT_TURTLE));

        if (Boolean.TRUE.equals(refreshResourceAfterUpdate)) {
            dereferenceURI();
        } else {
            this.invalidated = true;
            log.debug("HttpRemoteResource#updateGraph({}) - Invalidating Resource", this.uri);
        }
    }

    public HttpRemoteResource getMetadataResource(String authorizationHeaderValue) throws IOException {
        return new HttpRemoteResource(this.getMetadataURI(), authorizationHeaderValue);
    }

    // Return the resource URI directly associated with a given resource
    public URI getAssociatedURI() throws IOException {
        // If metadata - it is primary uri
        // If not metadata - it is metadata uri
        if (Boolean.TRUE.equals(this.isMetadata())) {

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
        if (this.parsedLinkHeaders.firstValue(LinkRelations.SHAPETREE_LOCATOR.getValue()).isEmpty()) {
            log.error("The resource {} does not contain a link header of {}", this.getUri(), LinkRelations.SHAPETREE_LOCATOR.getValue());
            // TODO: Should this be gracefully handled by the client?
            throw new ShapeTreeException(500, "No Link header with relation of " + LinkRelations.SHAPETREE_LOCATOR.getValue() + " found");
        }
        String metaDataURIString = this.parsedLinkHeaders.firstValue(LinkRelations.SHAPETREE_LOCATOR.getValue()).orElse(null);
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
            throw new ShapeTreeException(500, "No Link header with relation of " + LinkRelations.SHAPETREE_LOCATOR.getValue() + " found");
        }

        return metaDataURIString;
    }

    private void dereferenceURI() throws IOException {
        log.debug("HttpRemoteResource#dereferencingURI({})", this.uri);

        try {
            HttpClient fetcher = AbstractHttpClientFactory.getFactory().get(false);
            ResourceAttributes headers = new ResourceAttributes();
            headers.maybeSet(com.janeirodigital.shapetrees.core.enums.HttpHeaders.AUTHORIZATION.getValue(), authorizationHeaderValue);
            fetcher.fetchIntoRemoteResource(new HttpRequest("GET", this.uri, headers, null, null), this);
            this.invalidated = false;
        } catch (Exception e) {
            log.error("Error dereferencing URI", e);
        }
    }

    // Promiscuous hack for Fetcher.fetchIntoRemoteResource: Only HttpClient.fetchIntoRemoteResource needs to call these functions.
    // Is it possible to simulate a "friend" per https://stackoverflow.com/a/18634125/1243605 ?
    public void setExists(boolean exists) { this.exists = exists; }
    public void setResponseHeaders(ResourceAttributes responseHeaders) { this.responseHeaders = responseHeaders; }
    public void setParsedLinkHeaders(ResourceAttributes parsedLinkHeaders) { this.parsedLinkHeaders = parsedLinkHeaders; }
    public void setRawBody(String rawBody) { this.rawBody = rawBody; }
}
