package com.janeirodigital.shapetrees.client.http;

import com.janeirodigital.shapetrees.core.*;
import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
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
    public ShapeTreeResource getResource(ShapeTreeContext context, URI resourceURI) throws ShapeTreeException {
        return new HttpRemoteResource(resourceURI, context.getAuthorizationHeaderValue());
    }

    @Override
    public List<ResourceConstellation> getContainedResources(ShapeTreeContext context, URI containerResourceURI) throws ShapeTreeException {
        try {
            HttpRemoteResource containerResource = new HttpRemoteResource(containerResourceURI, context.getAuthorizationHeaderValue());

            if (Boolean.FALSE.equals(containerResource.isContainer())) {
                throw new ShapeTreeException(500, "Cannot get contained resources for a resource that is not a Container");
            }

            Optional<Graph> containerGraph = containerResource.getGraph();

            if (containerGraph.isEmpty()) { return Collections.emptyList(); }

            List<Triple> containerTriples = containerGraph.get().find(NodeFactory.createURI(containerResourceURI.toString()),
                                                                                      NodeFactory.createURI(LdpVocabulary.CONTAINS),
                                                                                      Node.ANY).toList();

            if (containerTriples.isEmpty()) { return Collections.emptyList(); }

            ArrayList<ResourceConstellation> containedResources = new ArrayList<>();

            for (Triple containerTriple : containerTriples) {
                ResourceConstellation containedResource = new ResourceConstellation(URI.create(containerTriple.getObject().getURI()), this, context); // getResource(context,URI.create(containerTriple.getObject().getURI()));
                containedResources.add(containedResource);
            }

            return containedResources;

        } catch (Exception ex) {
            throw new ShapeTreeException(500, ex.getMessage());
        }
    }

    @Override
    public ShapeTreeResource createResource(ShapeTreeContext context, String method, URI resourceURI, ResourceAttributes headers, String body, String contentType) throws ShapeTreeException {
        log.debug("createResource via {}: URI [{}], headers [{}]", method, resourceURI, headers.toString());

        HttpClient fetcher = AbstractHttpClientFactory.getFactory().get(false);
        ResourceAttributes allHeaders = headers.maybePlus(HttpHeaders.AUTHORIZATION.getValue(), context.getAuthorizationHeaderValue());
        DocumentResponse response = fetcher.fetchShapeTreeResponse(new HttpRequest(method, resourceURI, allHeaders, body, contentType));
        if (!response.exists()) {
            throw new ShapeTreeException(500, "Unable to create pre-existing resource <" + resourceURI + ">");
        }
        return new ShapeTreeResource(resourceURI, response);
    }

    @Override
    public ShapeTreeResource updateResource(ShapeTreeContext context, String method, ShapeTreeResource updatedResource) throws ShapeTreeException {
        log.debug("updateResource: URI [{}]", updatedResource.getUri());

        String contentType = updatedResource.getAttributes().firstValue(HttpHeaders.CONTENT_TYPE.getValue()).orElse(null);
        // [careful] updatedResource attributes may contain illegal client headers (connection, content-length, date, expect, from, host, upgrade, via, warning)
        ResourceAttributes allHeaders = updatedResource.getAttributes().maybePlus(HttpHeaders.AUTHORIZATION.getValue(), context.getAuthorizationHeaderValue());
        HttpClient fetcher = AbstractHttpClientFactory.getFactory().get(false);
        DocumentResponse response = fetcher.fetchShapeTreeResponse(new HttpRequest(method, updatedResource.getUri(), allHeaders, updatedResource.getBody(), contentType));
        return new ShapeTreeResource(updatedResource.getUri(), response);
    }

    @Override
    public DocumentResponse deleteResource(ShapeTreeContext context, ResourceConstellation.MetadataResource deletedResource) throws ShapeTreeException {
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
