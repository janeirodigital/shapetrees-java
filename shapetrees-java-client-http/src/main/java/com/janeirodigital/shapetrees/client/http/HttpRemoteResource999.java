package com.janeirodigital.shapetrees.client.http;

import com.janeirodigital.shapetrees.core.DocumentResponse;
import com.janeirodigital.shapetrees.core.ResourceAttributes;
import com.janeirodigital.shapetrees.core.ShapeTreeResource999;
import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.core.enums.LinkRelations;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.helpers.GraphHelper;
import lombok.extern.slf4j.Slf4j;
import org.apache.jena.graph.Graph;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;

import java.io.StringWriter;
import java.net.URI;
import java.util.*;

/**
 * Convenience class that encapsulates an OkHttp-based http client to quickly retrieve
 * web resources with convenient methods to access headers, link headers and the body as a graph
 */
@Slf4j
public class HttpRemoteResource999 extends ShapeTreeResource999 /*implements ResourceAccessor*/ {
    protected static final String TEXT_TURTLE = "text/turtle";
    protected static final String APP_RDF_XML = "application/rdf+xml";
    protected static final String APP_N3 = "application/n-triples";
    protected static final String APP_LD_JSON = "application/ld+json";
    protected static final Set<String> supportedRDFContentTypes = Set.of(TEXT_TURTLE, APP_RDF_XML, APP_N3, APP_LD_JSON);

    protected final String authorizationHeaderValue;

    public HttpRemoteResource999(URI uri, String authorizationHeaderValue) throws ShapeTreeException {
        this.uri = uri;
        this.authorizationHeaderValue = authorizationHeaderValue;
        dereferenceURI();
        if (this.isExists() && isRdfResource(this.attributes)) {
            try {
                this.graph = Optional.of(GraphHelper.readStringIntoGraph(uri, this.body, this.attributes.firstValue(HttpHeaders.CONTENT_TYPE.getValue()).orElse(null)));
            } catch (ShapeTreeException e) {
                throw new ShapeTreeException(500, "Unable to parse graph at " + uri.toString());
            }
        } else {
            this.graph = Optional.empty();
        }
    }

    public static Boolean isRdfResource(ResourceAttributes attributes) {
        Optional<String> contentType = attributes.firstValue(HttpHeaders.CONTENT_TYPE.getValue().toLowerCase());
        if (contentType.isPresent()) {
            return HttpRemoteResource999.supportedRDFContentTypes.contains(contentType.get());
        } else {
            return false;
        }
    }

    // TODO: Test: only referenced in HttpRemoteResourceTests; !remove
    public void updateGraph(Graph updatedGraph, Boolean refreshResourceAfterUpdate, String authorizationHeaderValue) throws ShapeTreeException {
        log.debug("HttpRemoteResource999#updateGraph({})", this.uri);

        StringWriter sw = new StringWriter();
        RDFDataMgr.write(sw, updatedGraph, Lang.TURTLE);

        HttpClient fetcher = AbstractHttpClientFactory.getFactory().get(false);
        ResourceAttributes headers = new ResourceAttributes(HttpHeaders.AUTHORIZATION.getValue(), authorizationHeaderValue);
        fetcher.fetchShapeTreeResponse(new HttpRequest("PUT", this.uri, headers, sw.toString(), TEXT_TURTLE));

        if (Boolean.TRUE.equals(refreshResourceAfterUpdate)) {
            dereferenceURI();
        } else {
            log.debug("HttpRemoteResource999#updateGraph({}) - Invalidating Resource feature removed", this.uri);
        }
    }

    private void dereferenceURI() throws ShapeTreeException {
        log.debug("HttpRemoteResource999#dereferencingURI({})", this.uri);

        ResourceAttributes headers = new ResourceAttributes();
        headers.maybeSet(HttpHeaders.AUTHORIZATION.getValue(), this.authorizationHeaderValue);
        HttpClient fetcher = AbstractHttpClientFactory.getFactory().get(false);
        HttpRequest req = new HttpRequest("GET", this.uri, headers, null, null);

        DocumentResponse response = fetcher.fetchShapeTreeResponse(req);
        Optional<String> location = response.getResourceAttributes().firstValue(HttpHeaders.LOCATION.getValue());
        if (location.isPresent()) { this.uri = URI.create(location.get()); }
        // this.exists = response.exists(); !!
        this.exists = response.getStatusCode()/100 == 2;
        this.container = isContainerFromHeaders(response.getResourceAttributes(), this.uri);
        this.attributes = response.getResourceAttributes();
        this.resourceType = getResourceTypeFromHeaders(response.getResourceAttributes());

        this.body = response.getBody();
        if (response.getBody() == null) {
            log.error("Exception retrieving body string");
        }

        final List<String> linkHeaders = this.attributes.allValues(HttpHeaders.LINK.getValue());
        ResourceAttributes parsedLinkHeaders = linkHeaders.size() == 0 // !!
                ? new ResourceAttributes()
                : ResourceAttributes.parseLinkHeaders(linkHeaders);
        Optional<URI> metadataUri = HttpRemoteResource999.calculateMetadataURI(this.uri, parsedLinkHeaders);
        this.metadata = HttpRemoteResource999.calculateIsMetadata(this.uri, this.exists, parsedLinkHeaders);;
        this.managed = true;
        if (this.metadata) { this.managed = false; }
        if (metadataUri.isEmpty()) { this.managed = false; }
        if (!this.exists) { this.managed = false; }
//        HttpRemoteResourceAccessor.isManaged(metadata, metadataUri);
        this.associatedUri = metadataUri;
        // If metadata - it is primary uri
        // If not metadata - it is metadata uri
        if (Boolean.TRUE.equals(this.metadata)) {

            // If this implementation uses a dot notation for meta, trim it from the path
            String basePath = this.uri.getPath().replaceAll("\\.shapetree$", "");

            // Rebuild without the query string in case that was employed
            String associatedString = this.uri.getScheme() + "://" + this.uri.getAuthority() + basePath;
            // @see https://github.com/xformativ/shapetrees-java/issues/86
            this.associatedUri = Optional.of(URI.create(associatedString));

        }
        this.body = response.getBody();
        this.name = calculateName(this.uri);
    }

    static Optional<URI> calculateMetadataURI(URI uri, ResourceAttributes parsedLinkHeaders) {
        final Optional<String> optLocatorString = parsedLinkHeaders.firstValue(LinkRelations.SHAPETREE_LOCATOR.getValue());
        if (optLocatorString.isEmpty()) {
            log.info("The resource {} does not contain a link header of {}", uri, LinkRelations.SHAPETREE_LOCATOR.getValue());
            return Optional.empty();
        }
        String metaDataURIString = optLocatorString.get();
        if (metaDataURIString.startsWith("/")) {
            // If the header value doesn't include scheme/host, prefix it with the scheme & host from container
            URI shapeTreeContainerURI = uri;
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

    static Boolean calculateIsMetadata(URI uri, boolean exists, ResourceAttributes parsedLinkHeaders) {
        // If the resource has an HTTP Link header of type of https://www.w3.org/ns/shapetrees#ShapeTreeLocator
        // with a metadata target, it is not a metadata resource (because it is pointing to one)
        if (Boolean.TRUE.equals(exists) && parsedLinkHeaders != null && parsedLinkHeaders.firstValue(LinkRelations.SHAPETREE_LOCATOR.getValue()).isPresent()) {
            return false;
        }
        // If the resource doesn't exist, currently we need to do some inference based on the URI
        if (uri.getPath() != null && uri.getPath().matches(".*\\.shapetree$")) { return true; }
        return uri.getQuery() != null && uri.getQuery().matches(".*ext\\=shapetree$");
    }

    static String calculateName(URI uri) {
        String path = uri.getPath();

        if (path.equals("/")) return "/";

        // if this is a container, trim the trailing slash
        if (path.endsWith("/")) {
            path = path.substring(0, path.length() - 1);
        }

        int pathIndex = path.lastIndexOf('/');

        // No slashes in the path
        if (pathIndex == -1) {
            return path;
        }

        return path.substring(path.lastIndexOf('/') + 1);
    }
}
