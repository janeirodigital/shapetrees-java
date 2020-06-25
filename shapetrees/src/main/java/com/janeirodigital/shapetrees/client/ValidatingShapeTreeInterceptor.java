package com.janeirodigital.shapetrees.client;

import com.janeirodigital.shapetrees.*;
import com.janeirodigital.shapetrees.enums.HttpHeaders;
import com.janeirodigital.shapetrees.enums.Namespaces;
import com.janeirodigital.shapetrees.helper.GraphHelper;
import com.janeirodigital.shapetrees.helper.HttpHeaderHelper;
import com.janeirodigital.shapetrees.model.ShapeTreeStep;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.*;
import org.apache.http.client.HttpResponseException;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.protocol.HttpContext;
import org.apache.jena.graph.*;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.*;

@Slf4j
public class ValidatingShapeTreeInterceptor implements HttpRequestInterceptor {

    public static final String REL_SHAPE_TREE = "ShapeTree";
    public static final String REL_DESCRIBEDBY = "acl"; // "describedby"; TODO: This is not working in ESS, using acl for testing
    public static final String POST = "POST";
    public static final String REL_TYPE_CONTAINER = "<http://www.w3.org/ns/ldp#Container>; rel=\"type\"";


    private final ShapeTreeEcosystem ecosystem;

    public ValidatingShapeTreeInterceptor(ShapeTreeEcosystem ecosystem) {
        this.ecosystem = ecosystem;
    }

    @SneakyThrows
    public void process(HttpRequest request, HttpContext context) {

        RemoteResource requestRemoteResource = new RemoteResource(request.getRequestLine().getUri());
        Map<String, List<String>> incomingRequestHeaders = HttpHeaderHelper.parseHeadersToMap(request.getAllHeaders());
        Map<String, List<String>> incomingRequestLinkHeaders = HttpHeaderHelper.parseLinkHeadersToMap(request.getHeaders(HttpHeaders.LINK.getValue()));

        switch (request.getRequestLine().getMethod()) {
            case POST: {
                // For a POST the requestRemoteResource is the parent container
                // If it doesn't exist, return a 404
                if (!requestRemoteResource.exists()) throw new HttpResponseException(404, "Parent Container not found");

                String slugHeaderValue = incomingRequestHeaders.get(HttpHeaders.SLUG.getValue()).stream().findFirst().orElse(null);
                // TODO this needs to be defaulted when Slug header is missing
                String requestedName = slugHeaderValue + (requestRemoteResource.isContainer() ? "/" : "");

                StringEntity interceptedRequestBody = null;
                if (request instanceof HttpEntityEnclosingRequest) {
                    interceptedRequestBody = (StringEntity)((HttpEntityEnclosingRequest) request).getEntity();
                }

                String incomingRequestContentType = incomingRequestHeaders.get(HttpHeaders.CONTENT_TYPE.getValue()).stream().findFirst().orElse(null);
                String incomingRequestShapeTreeUri = incomingRequestLinkHeaders.get(REL_SHAPE_TREE).stream().findFirst().orElse(null);

                if (incomingRequestShapeTreeUri != null) {
                    // This means we're Planting a new Shape Tree
                    Graph incomingRequestBodyGraph = GraphHelper.readStreamIntoGraph(interceptedRequestBody.getContent(), incomingRequestContentType);

                    ShapeTreeStep shapeTreeStep = null;
                    try {
                        shapeTreeStep = ShapeTreeFactory.getShapeTreeStep(new URI(incomingRequestShapeTreeUri));
                    } catch (URISyntaxException e) {
                        throw new HttpResponseException(400, "Value of 'ShapeTree' link header is not a value URI");
                    }

                    Boolean alreadyPlanted = ecosystem.containerIsShapeTree(requestRemoteResource.getURI(), shapeTreeStep.getURI());
                    if (!alreadyPlanted) {
                        URI plantedURI = plantShapeTree(requestRemoteResource, shapeTreeStep, requestedName, incomingRequestBodyGraph, ".", 0);
                        ecosystem.indexShapeTree(requestRemoteResource.getURI(), shapeTreeStep.getURI(), plantedURI, incomingRequestBodyGraph);
                    }
                }
            }

            // TODO Patch -- handle a local SPARQL update and do validation before passing it on
        }
    }

    // TODO
    // TODO
    // TODO in shape-tree.js#plantShapeTreeInstance payloadGraph is passed in but appears to only be used for logging?
    // TODO
    // TODO

    @SneakyThrows
    private URI plantShapeTree(RemoteResource parentContainer, ShapeTreeStep shapeTreeStep, String requestedName, Graph bodyGraph, String shapeTreePath, int depth) {
        log.debug("plantShapeTree: parent [{}], step [{}], slug [{}], path [{}], depth [{}]", parentContainer.getURI(), shapeTreeStep.getId(), requestedName, shapeTreePath, depth);

        // Create new container with the Slug/Requested Name
        RemoteResource shapeTreeContainer = createContainer(parentContainer.getURI(), requestedName, bodyGraph);

        String metaDataURIString = shapeTreeContainer.getFirstLinkHeaderValueByRel(REL_DESCRIBEDBY);
        RemoteResource shapeTreeContainerMetadataResource = new RemoteResource(metaDataURIString);

        // Get the existing graph
        Graph shapeTreeContainerMetadataGraph = shapeTreeContainerMetadataResource.getGraph();

        // Remove any previous triples for the shapetree planting metadata
        // TODO: What are some cases where this could already exist?
        GraphUtil.remove(shapeTreeContainerMetadataGraph, NodeFactory.createURI(metaDataURIString), NodeFactory.createURI(Namespaces.SHAPETREE_NAMESPACE.getValue() + "shapeTreeRoot"), null);
        GraphUtil.remove(shapeTreeContainerMetadataGraph, NodeFactory.createURI(metaDataURIString), NodeFactory.createURI(Namespaces.SHAPETREE_NAMESPACE.getValue() + "shapeTreeInstancePath"), null);
        GraphUtil.remove(shapeTreeContainerMetadataGraph, NodeFactory.createURI(metaDataURIString), NodeFactory.createURI(Namespaces.SHAPETREE_NAMESPACE.getValue() + "shapeTreeInstanceRoot"), null);

        List<Triple> triplesToAdd = new ArrayList<>();
        triplesToAdd.add(new Triple(NodeFactory.createURI(metaDataURIString), NodeFactory.createURI(Namespaces.SHAPETREE_NAMESPACE.getValue() + "shapeTreeRoot"), NodeFactory.createURI(shapeTreeStep.getId())));
        // TODO Need to review this in concert with the recursive call below
        triplesToAdd.add(new Triple(NodeFactory.createURI(metaDataURIString), NodeFactory.createURI(Namespaces.SHAPETREE_NAMESPACE.getValue() + "shapeTreeInstancePath"), NodeFactory.createLiteral(shapeTreePath)));
        // TODO Need to review this in concert with the recursive call below
        String relativePath = (depth==0) ? "./" : StringUtils.repeat("../", depth);
        triplesToAdd.add(new Triple(NodeFactory.createURI(metaDataURIString), NodeFactory.createURI(Namespaces.SHAPETREE_NAMESPACE.getValue() + "shapeTreeInstanceRoot"), NodeFactory.createURI(relativePath)));
        GraphUtil.add(shapeTreeContainerMetadataGraph, triplesToAdd);
        // Write the updates back to the resource
        shapeTreeContainerMetadataResource.updateGraph(shapeTreeContainerMetadataGraph,false);

        // Recursively call plantShapeTree for any container contents -- resources are ignored
        for (URI contentStepURI : shapeTreeStep.getContents()) {
            ShapeTreeStep contentStep = ShapeTreeFactory.getShapeTreeStep(contentStepURI);
            if (contentStep.isContainer()) {
                // the return URI is discarded for recursive calls
                plantShapeTree(shapeTreeContainer, contentStep, contentStep.getLabel(), null, shapeTreePath +"/" + contentStep.getLabel(), ++depth);
            }
        }

        return shapeTreeContainer.getURI();
    }

    @SneakyThrows
    private RemoteResource createContainer(URI parentURI, String requestedName, Graph bodyGraph) {
        log.debug("createContainer: parent [{}], slug [{}]", parentURI, requestedName);
        CloseableHttpClient httpClient = HttpClients.createDefault();
        HttpPost createContainerPost = new HttpPost();
        createContainerPost.setHeader(HttpHeaders.SLUG.getValue(), requestedName);
        createContainerPost.setHeader(HttpHeaders.LINK.getValue(), REL_TYPE_CONTAINER);
        createContainerPost.setURI(parentURI);

        CloseableHttpResponse response = httpClient.execute(createContainerPost);
        return new RemoteResource(response);
    }

}
