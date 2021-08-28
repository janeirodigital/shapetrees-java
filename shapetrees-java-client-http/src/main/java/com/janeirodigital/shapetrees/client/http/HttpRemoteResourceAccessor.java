package com.janeirodigital.shapetrees.client.http;

import com.janeirodigital.shapetrees.core.HttpClientHeaders;
import com.janeirodigital.shapetrees.core.ResourceAccessor;
import com.janeirodigital.shapetrees.core.ShapeTreeResource;
import com.janeirodigital.shapetrees.core.ShapeTreeResponse;
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

import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

@NoArgsConstructor
@Slf4j
public class HttpRemoteResourceAccessor implements ResourceAccessor {

    private static final String POST = "POST";
    private static final String PUT = "PUT";
    private static final String PATCH = "PATCH";

    @Override
    public ShapeTreeResource getResource(ShapeTreeContext context, URI resourceURI) throws ShapeTreeException {
        try {
            return mapRemoteResourceToShapeTreeResource(new HttpRemoteResource(resourceURI, context.getAuthorizationHeaderValue()));
        } catch (Exception ex) {
            throw new ShapeTreeException(500, ex.getMessage());
        }
    }

    @Override
    public List<ShapeTreeResource> getContainedResources(ShapeTreeContext context, URI containerResourceURI) throws ShapeTreeException {
        try {
            HttpRemoteResource containerResource = new HttpRemoteResource(containerResourceURI, context.getAuthorizationHeaderValue());

            if (Boolean.FALSE.equals(containerResource.isContainer())) {
                throw new ShapeTreeException(500, "Cannot get contained resources for a resource that is not a Container");
            }

            Graph containerGraph = containerResource.getGraph(containerResourceURI);

            if (containerGraph == null) { return Collections.emptyList(); }

            List<Triple> containerTriples = containerGraph.find(NodeFactory.createURI(containerResourceURI.toString()),
                                                                                      NodeFactory.createURI(LdpVocabulary.CONTAINS),
                                                                                      Node.ANY).toList();

            if (containerTriples.isEmpty()) { return Collections.emptyList(); }

            ArrayList<ShapeTreeResource> containedResources = new ArrayList<>();

            for (Triple containerTriple : containerTriples) {
                ShapeTreeResource containedResource = getResource(context,URI.create(containerTriple.getObject().getURI()));
                containedResources.add(containedResource);
            }

            return containedResources;

        } catch (Exception ex) {
            throw new ShapeTreeException(500, ex.getMessage());
        }
    }

    @Override
    public ShapeTreeResource createResource(ShapeTreeContext context, String method, URI resourceURI, HttpClientHeaders headers, String body, String contentType) throws ShapeTreeException {
        log.debug("createResource via {}: URI [{}], headers [{}]", method, resourceURI, headers.toString());

        HttpClient fetcher = AbstractHttpClientFactory.getFactory().get(false);
        HttpClientHeaders allHeaders = headers.maybePlus(HttpHeaders.AUTHORIZATION.getValue(), context.getAuthorizationHeaderValue());
        return fetcher.fetchShapeTreeResource(method, resourceURI, allHeaders, body, contentType);
    }

    @Override
    public ShapeTreeResource updateResource(ShapeTreeContext context, String method, ShapeTreeResource updatedResource) throws ShapeTreeException {
        log.debug("updateResource: URI [{}]", updatedResource.getUri());

        String contentType = updatedResource.getFirstAttributeValue(HttpHeaders.CONTENT_TYPE.getValue());
        // [careful] updatedResource attributes may contain illegal client headers (connection, content-length, date, expect, from, host, upgrade, via, warning)
        HttpClientHeaders allHeaders = updatedResource.getAttributes().maybePlus(HttpHeaders.AUTHORIZATION.getValue(), context.getAuthorizationHeaderValue());
        HttpClient fetcher = AbstractHttpClientFactory.getFactory().get(false);
        return fetcher.fetchShapeTreeResource(method, updatedResource.getUri(), allHeaders, updatedResource.getBody(), contentType);
    }

    @Override
    public ShapeTreeResponse deleteResource(ShapeTreeContext context, ShapeTreeResource deletedResource) throws ShapeTreeException {
        log.debug("deleteResource: URI [{}]", deletedResource.getUri());

        HttpClient fetcher = AbstractHttpClientFactory.getFactory().get(false);
        HttpClientHeaders allHeaders = deletedResource.getAttributes().maybePlus(HttpHeaders.AUTHORIZATION.getValue(), context.getAuthorizationHeaderValue());
        ShapeTreeResponse response = fetcher.fetchShapeTreeResponse("DELETE", deletedResource.getUri(), allHeaders, null, null);
        int respCode = response.getStatusCode();
        if (respCode < 200 || respCode >= 400) {
            log.error("Error deleting resource {}, Status {}", deletedResource.getUri(), respCode);
        }
        return response;

    }

    private ShapeTreeResource mapRemoteResourceToShapeTreeResource(HttpRemoteResource remoteResource) throws IOException {
        ShapeTreeResource shapeTreeResource = new ShapeTreeResource();
        try {
            shapeTreeResource.setUri(remoteResource.getUri());
        } catch (IOException ex) {
            throw new ShapeTreeException(500, "Error resolving URI");
        }

        shapeTreeResource.setExists(remoteResource.exists());

        shapeTreeResource.setName(remoteResource.getName());

        shapeTreeResource.setMetadata(remoteResource.isMetadata());

        shapeTreeResource.setContainer(remoteResource.isContainer());

        shapeTreeResource.setAttributes(remoteResource.getResponseHeaders());

        try {
            shapeTreeResource.setAssociatedUri(remoteResource.getAssociatedURI());
        } catch (IOException iex) {
            shapeTreeResource.setAssociatedUri(null);
        }

        if (shapeTreeResource.isExists()) {
            try {
                shapeTreeResource.setType(remoteResource.getResourceType());
            } catch (IOException iex) {
                shapeTreeResource.setType(null);
            }

            try {
                shapeTreeResource.setManaged(remoteResource.isManaged());
            } catch (IOException iex) {
                shapeTreeResource.setManaged(false);
            }

            try {
                shapeTreeResource.setBody(remoteResource.getBody());
            } catch (IOException iex) {
                shapeTreeResource.setBody(null);
            }
        }

        return shapeTreeResource;
    }
}
