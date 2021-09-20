package com.janeirodigital.shapetrees.client.http;

import com.janeirodigital.shapetrees.core.DocumentResponse;
import com.janeirodigital.shapetrees.core.ResourceAttributes;
import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
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
import java.util.*;

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
    private Boolean exists;
    private ResourceAttributes responseHeaders;
    private ResourceAttributes parsedLinkHeaders;
    private final Optional<Graph> parsedGraph;
    private String rawBody;
    protected final Set<String> supportedRDFContentTypes = Set.of(TEXT_TURTLE, APP_RDF_XML, APP_N3, APP_LD_JSON);

    public HttpRemoteResource(URI uri, String authorizationHeaderValue) throws ShapeTreeException {
        this.uri = uri;
        this.authorizationHeaderValue = authorizationHeaderValue;
        dereferenceURI();
        if (this.exists() && isRdfResource()) {
            try {
                this.parsedGraph = Optional.of(GraphHelper.readStringIntoGraph(uri, this.rawBody, getFirstHeaderByName(HttpHeaders.CONTENT_TYPE.getValue())));
            } catch (ShapeTreeException e) {
                throw new ShapeTreeException(500, "Unable to parse graph at " + uri.toString());
            }
        } else {
            this.parsedGraph = Optional.empty();
        }
    }

    public URI getUri() {
        return this.uri;
    }

    public Boolean exists() {
        return this.exists;
    }

    public String getBody() {
        if (Boolean.FALSE.equals(this.exists)) return null; // TODO: this means we can get get an error message back to a user.

        return this.rawBody;
    }

    // Lazy-load graph when requested
    public Optional<Graph> getGraph() {

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

    public ShapeTreeResourceType getResourceType() {

        if (Boolean.TRUE.equals(isContainer())) {
            return ShapeTreeResourceType.CONTAINER;
        }

        if (Boolean.TRUE.equals(isRdfResource())) {
            return ShapeTreeResourceType.RESOURCE;
        } else {
            return ShapeTreeResourceType.NON_RDF;
        }

    }

    public Boolean isRdfResource() {
        String contentType = this.getFirstHeaderByName(HttpHeaders.CONTENT_TYPE.getValue().toLowerCase());
        if (contentType != null) {
            return this.supportedRDFContentTypes.contains(contentType);
        } else {
            return false;
        }
    }

    public Boolean isContainer() {
        String uriPath = this.uri.getPath();
        // Some implementations use the query string to denote auxiliary metadata for containers or resources
        // In the event a query string is present on a container, ensure that the auxiliary resource doesn't
        // get mistaken as a container when shortened to the URI path
        return (this.uri.getQuery() == null && uriPath.endsWith("/"));
    }

    public Boolean isMetadata() {
        // If the resource has an HTTP Link header of type of https://www.w3.org/ns/shapetrees#ShapeTreeLocator
        // with a metadata target, it is not a metadata resource (because it is pointing to one)
        if (Boolean.TRUE.equals(this.exists()) && this.parsedLinkHeaders != null && this.parsedLinkHeaders.firstValue(LinkRelations.SHAPETREE_LOCATOR.getValue()).isPresent()) {
            return false;
        }
        // If the resource doesn't exist, currently we need to do some inference based on the URI
        if (this.getUri().getPath() != null && this.getUri().getPath().matches(".*\\.shapetree$")) { return true; }
        return this.getUri().getQuery() != null && this.getUri().getQuery().matches(".*ext\\=shapetree$");
    }

    public Boolean isManaged() throws ShapeTreeException {
        if (Boolean.TRUE.equals(this.isMetadata())) { return false; }
        Optional<URI> metadataUri = this.getMetadataURI();
        if (metadataUri.isEmpty()) { return false; }
        return Boolean.TRUE.equals(new HttpRemoteResource(metadataUri.get(), this.authorizationHeaderValue).exists());
    }

    public ResourceAttributes getResponseHeaders() { return this.responseHeaders; }

    public ResourceAttributes getLinkHeaders() {
        return this.parsedLinkHeaders;
    }

    public String getFirstHeaderByName(String headerName) {
        return this.responseHeaders.firstValue(headerName).orElse(null);
    }

    // TODO: only referenced in HttpRemoteResourceTests; !remove
    public void updateGraph(Graph updatedGraph, Boolean refreshResourceAfterUpdate, String authorizationHeaderValue) throws ShapeTreeException {
        log.debug("HttpRemoteResource#updateGraph({})", this.uri);

        StringWriter sw = new StringWriter();
        RDFDataMgr.write(sw, updatedGraph, Lang.TURTLE);

        HttpClient fetcher = AbstractHttpClientFactory.getFactory().get(false);
        ResourceAttributes headers = new ResourceAttributes(HttpHeaders.AUTHORIZATION.getValue(), authorizationHeaderValue);
        fetcher.fetchShapeTreeResponse(new HttpRequest("PUT", this.uri, headers, sw.toString(), TEXT_TURTLE));

        if (Boolean.TRUE.equals(refreshResourceAfterUpdate)) {
            dereferenceURI();
        } else {
            log.debug("HttpRemoteResource#updateGraph({}) - Invalidating Resource feature removed", this.uri);
        }
    }

    // Return the resource URI directly associated with a given resource
    public Optional<URI> getAssociatedURI() {
        // If metadata - it is primary uri
        // If not metadata - it is metadata uri
        if (Boolean.TRUE.equals(this.isMetadata())) {

            // If this implementation uses a dot notation for meta, trim it from the path
            String basePath = this.getUri().getPath().replaceAll("\\.shapetree$", "");

            // Rebuild without the query string in case that was employed
            String associatedString = this.getUri().getScheme() + "://" + this.getUri().getAuthority() + basePath;

            return Optional.of(URI.create(associatedString));

        } else {
            return getMetadataURI();
        }
    }

    @NotNull
    public Optional<URI> getMetadataURI() {
        final Optional<String> optLocatorString = this.parsedLinkHeaders.firstValue(LinkRelations.SHAPETREE_LOCATOR.getValue());
        if (optLocatorString.isEmpty()) {
            log.info("The resource {} does not contain a link header of {}", this.getUri(), LinkRelations.SHAPETREE_LOCATOR.getValue());
            return Optional.empty();
        }
        String metaDataURIString = optLocatorString.get();
        if (metaDataURIString.startsWith("/")) {
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

        return Optional.of(URI.create(metaDataURIString));
    }

    private void dereferenceURI() { // TODO: swallows STE instead of throwing it
        log.debug("HttpRemoteResource#dereferencingURI({})", this.uri);

        try {
            HttpClient fetcher = AbstractHttpClientFactory.getFactory().get(false);
            ResourceAttributes headers = new ResourceAttributes();
            headers.maybeSet(HttpHeaders.AUTHORIZATION.getValue(), this.authorizationHeaderValue);
            HttpRequest req = new HttpRequest("GET", this.uri, headers, null, null);
            DocumentResponse resp = fetcher.fetchShapeTreeResponse(req);
            this.exists = resp.exists();
            ResourceAttributes allHeaders = resp.getResourceAttributes();
            this.responseHeaders = new ResourceAttributes(allHeaders.toMultimap());
            final List<String> linkHeaders = allHeaders.allValues(HttpHeaders.LINK.getValue());
            if (linkHeaders.size() != 0) {
                this.parsedLinkHeaders = ResourceAttributes.parseLinkHeaders(linkHeaders);
            } else {
                this.parsedLinkHeaders = new ResourceAttributes();
            }
            this.rawBody = Objects.requireNonNull(resp.getBody()); // @@ is requireNull useful here?
        } catch (Exception e) {
            log.error("Error dereferencing URI", e);
        }
    }

    @Override
    public String toString() {
        return "HttpRemoteResource{" +
                "uri=" + this.uri +
                ", authorizationHeaderValue='" + this.authorizationHeaderValue + '\'' +
                ", exists=" + this.exists +
                ", responseHeaders=" + this.responseHeaders +
                ", parsedLinkHeaders=" + this.parsedLinkHeaders +
                ", parsedGraph=" + this.parsedGraph +
                ", rawBody='" + this.rawBody + '\'' +
                ", supportedRDFContentTypes=" + this.supportedRDFContentTypes +
                '}';
    }
}
