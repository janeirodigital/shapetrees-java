package com.janeirodigital.shapetrees.client.http;

import com.janeirodigital.shapetrees.core.*;
import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.helpers.GraphHelper;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;
import com.janeirodigital.shapetrees.core.vocabularies.LdpVocabulary;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.jena.graph.Graph;
import org.apache.jena.graph.Node;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Triple;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

@NoArgsConstructor
@Slf4j
public class HttpRemoteResourceAccessor implements ResourceAccessor {

    private static final String POST = "POST";
    private static final String PUT = "PUT";
    private static final String PATCH = "PATCH";

    @Override
    public ShapeTreeResource.Fork getResource(ShapeTreeContext context, URI uri) throws ShapeTreeException {
        log.debug("HttpRemoteResourceAccessor#getResource({})", uri);
        ResourceAttributes headers = new ResourceAttributes();
        headers.maybeSet(HttpHeaders.AUTHORIZATION.getValue(), context.getAuthorizationHeaderValue());
        HttpClient fetcher = AbstractHttpClientFactory.getFactory().get(false);
        HttpRequest req = new HttpRequest("GET", uri, headers, null, null);

        DocumentResponse response = fetcher.fetchShapeTreeResponse(req);
        return makeAFork(uri, response);
    }

    @Override
    public ShapeTreeResource.Fork createResource(ShapeTreeContext context, String method, URI uri, ResourceAttributes headers, String body, String contentType) throws ShapeTreeException {
        log.debug("createResource via {}: URI [{}], headers [{}]", method, uri, headers.toString());

        HttpClient fetcher = AbstractHttpClientFactory.getFactory().get(false);
        ResourceAttributes allHeaders = headers.maybePlus(HttpHeaders.AUTHORIZATION.getValue(), context.getAuthorizationHeaderValue());
        DocumentResponse response = fetcher.fetchShapeTreeResponse(new HttpRequest(method, uri, allHeaders, body, contentType));
        if (!response.exists()) {
            throw new ShapeTreeException(500, "Unable to create pre-existing resource <" + uri + ">");
        }
        return makeAFork(uri, response);
    }

    protected ShapeTreeResource.Fork makeAFork(URI uri, DocumentResponse response) throws ShapeTreeException {
        Optional<String> location = response.getResourceAttributes().firstValue(HttpHeaders.LOCATION.getValue());
        if (location.isPresent()) { uri = URI.create(location.get()); }
        // this.exists = response.exists(); !!
        final boolean exists = response.getStatusCode()/100 == 2;
        final boolean container = ShapeTreeResource999.isContainerFromHeaders(response.getResourceAttributes(), uri);
        final ResourceAttributes attributes = response.getResourceAttributes();
        final ShapeTreeResourceType resourceType = ShapeTreeResource999.getResourceTypeFromHeaders(response.getResourceAttributes());

        final String name = HttpRemoteResource999.calculateName(uri);
        final String body = response.getBody();
        if (response.getBody() == null) {
            log.error("Exception retrieving body string");
        }

        final List<String> linkHeaders = attributes.allValues(HttpHeaders.LINK.getValue());
        ResourceAttributes parsedLinkHeaders = linkHeaders.size() == 0 // !!
                ? new ResourceAttributes()
                : ResourceAttributes.parseLinkHeaders(linkHeaders);
        final Optional<URI> metadataUri = HttpRemoteResource999.calculateMetadataURI(uri, parsedLinkHeaders);
        final boolean metadata = HttpRemoteResource999.calculateIsMetadata(uri, exists, parsedLinkHeaders);;

        if (Boolean.TRUE.equals(metadata)) {
            // If this implementation uses a dot notation for meta, trim it from the path
            final String basePath = uri.getPath().replaceAll("\\.shapetree$", "");

            // Rebuild without the query string in case that was employed
            final String associatedString = uri.getScheme() + "://" + uri.getAuthority() + basePath;
            // @see https://github.com/xformativ/shapetrees-java/issues/86
            final URI associatedUri = URI.create(associatedString);

            final Optional<String> contentType = attributes.firstValue(HttpHeaders.CONTENT_TYPE.getValue().toLowerCase());
            Optional<Graph> graph;
            if (exists && !contentType.isEmpty()) {
                if (!HttpRemoteResource999.isRdfResource(attributes)) {
                    throw new IllegalStateException("<" + uri + "> is a metadata resource with a non-RDF Content-Type: " + contentType); }
                try {
                    graph = Optional.of(GraphHelper.readStringIntoGraph(uri, body, attributes.firstValue(HttpHeaders.CONTENT_TYPE.getValue()).orElse(null)));
                } catch (ShapeTreeException e) {
                    throw new ShapeTreeException(500, "Unable to parse graph at " + uri.toString());
                }
            } else {
                // throw new IllegalStateException("<" + uri + "> is a metadata resource but doesn't exist");
                graph = Optional.empty();
            }

            return new ShapeTreeResource.Metadata(uri, resourceType, attributes, body, name, exists, Optional.of(associatedUri), graph);
        } else {
            final boolean managed = exists && !metadataUri.isEmpty();
            final List<String> linkHeaderValues = attributes.allValues(HttpHeaders.LINK.getValue());
            return new ShapeTreeResource.UserOwned(uri, resourceType, attributes, body, name, exists, metadataUri, ResourceAttributes.parseLinkHeaders(linkHeaderValues), managed, container);
        }
    }

    @Override
    public List<ShapeTreeResource> getContainedResources(ShapeTreeContext context, URI containerResourceURI) throws ShapeTreeException {
        try {
            ShapeTreeResource.Fork rf = this.getResource(context, containerResourceURI);
            if (!(rf instanceof ShapeTreeResource.UserOwned)) {
                throw new ShapeTreeException(500, "Cannot get contained resources for a metadata resource <" + containerResourceURI + ">");
            }
            ShapeTreeResource.UserOwned containerResource = (ShapeTreeResource.UserOwned) rf;

            if (Boolean.FALSE.equals(containerResource.isContainer())) {
                throw new ShapeTreeException(500, "Cannot get contained resources for a resource that is not a Container <" + containerResourceURI + ">");
            }

            Graph containerGraph = GraphHelper.readStringIntoGraph(containerResourceURI, containerResource.getBody(), containerResource.getAttributes().firstValue(HttpHeaders.CONTENT_TYPE.getValue()).orElse(null));

            if (containerGraph.isEmpty()) { return Collections.emptyList(); }

            List<Triple> containerTriples = containerGraph.find(NodeFactory.createURI(containerResourceURI.toString()),
                    NodeFactory.createURI(LdpVocabulary.CONTAINS),
                    Node.ANY).toList();

            if (containerTriples.isEmpty()) { return Collections.emptyList(); }

            ArrayList<ShapeTreeResource> containedResources = new ArrayList<>();

            for (Triple containerTriple : containerTriples) {
                ShapeTreeResource containedResource = new ShapeTreeResource(URI.create(containerTriple.getObject().getURI()), this, context); // getResource(context,URI.create(containerTriple.getObject().getURI()));
                containedResources.add(containedResource);
            }

            return containedResources;
        } catch (Exception ex) {
            throw new ShapeTreeException(500, ex.getMessage());
        }
    }

    @Override
    public DocumentResponse updateResource(ShapeTreeContext context, String method, ShapeTreeResource.Fork updatedResource, String body) throws ShapeTreeException { // TODO: @@ could be Metadata
        log.debug("updateResource: URI [{}]", updatedResource.getUri());

        String contentType = updatedResource.getAttributes().firstValue(HttpHeaders.CONTENT_TYPE.getValue()).orElse(null);
        // [careful] updatedResource attributes may contain illegal client headers (connection, content-length, date, expect, from, host, upgrade, via, warning)
        ResourceAttributes allHeaders = updatedResource.getAttributes().maybePlus(HttpHeaders.AUTHORIZATION.getValue(), context.getAuthorizationHeaderValue());
        HttpClient fetcher = AbstractHttpClientFactory.getFactory().get(false);
        DocumentResponse response = fetcher.fetchShapeTreeResponse(new HttpRequest(method, updatedResource.getUri(), allHeaders, body, contentType));
        return response;
    }

    @Override
    public DocumentResponse deleteResource(ShapeTreeContext context, ShapeTreeResource.Metadata deletedResource) throws ShapeTreeException {
        log.debug("deleteResource: URI [{}]", deletedResource.getUri());

        HttpClient fetcher = AbstractHttpClientFactory.getFactory().get(false);
        ResourceAttributes allHeaders = deletedResource.getAttributes().maybePlus(HttpHeaders.AUTHORIZATION.getValue(), context.getAuthorizationHeaderValue());
        DocumentResponse response = fetcher.fetchShapeTreeResponse(new HttpRequest("DELETE", deletedResource.getUri(), allHeaders, null, null));
        int respCode = response.getStatusCode();
        if (respCode < 200 || respCode >= 400) {
            log.error("Error deleting resource {}, Status {}", deletedResource.getUri(), respCode);
        }
        return response;
    }
}
