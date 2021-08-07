package com.janeirodigital.shapetrees.client.fetch;

import com.janeirodigital.shapetrees.core.ResourceAccessor;
import com.janeirodigital.shapetrees.core.ShapeTreeResource;
import com.janeirodigital.shapetrees.core.ShapeTreeResponse;
import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;
import com.janeirodigital.shapetrees.core.vocabularies.LdpVocabulary;
import lombok.extern.slf4j.Slf4j;
// import okhttp3.OkHttpClient;
// import okhttp3.Request;
// import okhttp3.RequestBody;
// import okhttp3.MediaType;
// import okhttp3.Response;
import org.apache.http.Header;
import org.apache.jena.graph.Graph;
import org.apache.jena.graph.Node;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Triple;

import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

@Slf4j
public class FetchRemoteResourceAccessor implements ResourceAccessor {

    private static final String POST = "POST";
    private static final String PUT = "PUT";
    private static final String PATCH = "PATCH";

    @Override
    public ShapeTreeResource getResource(ShapeTreeContext context, URI resourceURI) throws ShapeTreeException {
        try {
            return mapRemoteResourceToShapeTreeResource(new RemoteResource(resourceURI, context.getAuthorizationHeaderValue()));
        } catch (Exception ex) {
            throw new ShapeTreeException(500, ex.getMessage());
        }
    }

    @Override
    public List<ShapeTreeResource> getContainedResources(ShapeTreeContext context, URI containerResourceURI) throws ShapeTreeException {
        try {
            RemoteResource containerResource = new RemoteResource(containerResourceURI, context.getAuthorizationHeaderValue());

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
    public ShapeTreeResource createResource(ShapeTreeContext context, String method, URI resourceURI, Map<String, List<String>> headers, String body, String contentType) throws ShapeTreeException {
        log.debug("createResource via {}: URI [{}], headers [{}]", method, resourceURI, writeHeaders(headers));

        OkHttpFetcher fetcher = OkHttpFetcher.getFetcher999(new ShapeTreeClientConfiguration(false, false));
        okhttp3.Response response = fetcher.fetch(method, resourceURI, headers, context.getAuthorizationHeaderValue(), body, contentType);
        return FetchHelper.mapFetchResponseToShapeTreeResource(response, resourceURI, headers);
    }

    @Override
    public ShapeTreeResource updateResource(ShapeTreeContext context, String method, ShapeTreeResource updatedResource) throws ShapeTreeException {
        log.debug("updateResource: URI [{}]", updatedResource.getUri());

        String contentType = updatedResource.getFirstAttributeValue(HttpHeaders.CONTENT_TYPE.getValue());
        OkHttpFetcher fetcher = OkHttpFetcher.getFetcher999(new ShapeTreeClientConfiguration(false, false));
        okhttp3.Response response = fetcher.fetch(method, updatedResource.getUri(), updatedResource.getAttributes(), context.getAuthorizationHeaderValue(), updatedResource.getBody(), contentType);
        return FetchHelper.mapFetchResponseToShapeTreeResource(response, updatedResource.getUri(), updatedResource.getAttributes());

    }

    @Override
    public ShapeTreeResponse deleteResource(ShapeTreeContext context, ShapeTreeResource deletedResource) throws ShapeTreeException {
        log.debug("deleteResource: URI [{}]", deletedResource.getUri());

        OkHttpFetcher fetcher = OkHttpFetcher.getFetcher999(new ShapeTreeClientConfiguration(false, false));
        okhttp3.Response response = fetcher.fetch("DELETE", deletedResource.getUri(), deletedResource.getAttributes(), context.getAuthorizationHeaderValue(), null, null);
        if (!response.isSuccessful()) {
            log.error("Error deleting resource {}, Status {} Message {}", deletedResource.getUri(), response.code(), response.message());
        }
        return FetchHelper.mapFetchResponseToShapeTreeResponse(response);

    }

    private String writeHeaders(Map<String, List<String>> headers) {
        StringBuilder sb = new StringBuilder();
        for (Map.Entry<String, List<String>> entry : headers.entrySet()) {
            for (String value : entry.getValue()) {
                if (sb.length() != 0) {
                    sb.append(",");
                }
                sb.append(entry.getKey()).append("=").append(value);
            }
        }

        return sb.toString();
    }

    private ShapeTreeResource mapRemoteResourceToShapeTreeResource(RemoteResource remoteResource) throws IOException {
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
