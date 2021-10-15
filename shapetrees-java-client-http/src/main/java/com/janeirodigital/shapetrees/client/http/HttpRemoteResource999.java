package com.janeirodigital.shapetrees.client.http;

import com.janeirodigital.shapetrees.core.DocumentResponse;
import com.janeirodigital.shapetrees.core.ResourceAttributes;
import com.janeirodigital.shapetrees.core.ShapeTreeResource999;
import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;
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
        this.container = HttpRemoteResourceAccessor.isContainerFromHeaders(response.getResourceAttributes(), this.uri);
        this.attributes = response.getResourceAttributes();
        this.resourceType = ShapeTreeResourceType.NON_RDF; // getResourceTypeFromHeaders(response.getResourceAttributes());

        this.body = response.getBody();
        if (response.getBody() == null) {
            log.error("Exception retrieving body string");
        }

        final List<String> linkHeaders = this.attributes.allValues(HttpHeaders.LINK.getValue());
        ResourceAttributes parsedLinkHeaders = linkHeaders.size() == 0 // !!
                ? new ResourceAttributes()
                : ResourceAttributes.parseLinkHeaders(linkHeaders);
        Optional<URI> metadataUri = HttpRemoteResourceAccessor.calculateMetadataURI(this.uri, parsedLinkHeaders);
        this.metadata = HttpRemoteResourceAccessor.calculateIsMetadata(this.uri, this.exists, parsedLinkHeaders);;
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
        this.name = HttpRemoteResourceAccessor.calculateName(this.uri);
    }

}
