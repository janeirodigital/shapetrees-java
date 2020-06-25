package com.janeirodigital.shapetrees.client;

import com.janeirodigital.shapetrees.*;
import com.janeirodigital.shapetrees.enums.HttpHeaders;
import com.janeirodigital.shapetrees.enums.Namespaces;
import com.janeirodigital.shapetrees.helper.GraphHelper;
import com.janeirodigital.shapetrees.helper.HttpClientHelper;
import com.janeirodigital.shapetrees.helper.HttpHeaderHelper;
import com.janeirodigital.shapetrees.model.ShapeTreeStep;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.Header;
import org.apache.http.HttpEntityEnclosingRequest;
import org.apache.http.HttpRequest;
import org.apache.http.HttpRequestInterceptor;
import org.apache.http.client.HttpResponseException;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpRequestWrapper;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.message.BasicHeader;
import org.apache.http.protocol.HttpContext;
import org.apache.jena.graph.*;

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
    @Override
    public void process(HttpRequest request, HttpContext context) {

        String authorizationHeaderValue = request.getFirstHeader("Authorization").getValue();
        HttpRequestWrapper requestWrapper = (HttpRequestWrapper)request;
        RemoteResource requestRemoteResource = new RemoteResource(requestWrapper.getTarget().toURI() + request.getRequestLine().getUri(), authorizationHeaderValue);
        Map<String, List<String>> incomingRequestHeaders = HttpHeaderHelper.parseHeadersToMap(request.getAllHeaders());
        Map<String, List<String>> incomingRequestLinkHeaders = HttpHeaderHelper.parseLinkHeadersToMap(request.getHeaders(HttpHeaders.LINK.getValue()));

        switch (request.getRequestLine().getMethod()) {
            case POST: {
                // For a POST the requestRemoteResource is the parent container
                // If it doesn't exist, return a 404
                if (!requestRemoteResource.exists()) throw new HttpResponseException(404, "Parent Container not found");

                String slugHeaderValue = null;
                if (incomingRequestHeaders.containsKey(HttpHeaders.SLUG.getValue())) {
                    slugHeaderValue = incomingRequestHeaders.get(HttpHeaders.SLUG.getValue()).stream().findFirst().orElse(null);
                } else {
                    slugHeaderValue = "Container";
                }

                // TODO - why did the JS code append a slash at the end, just to remove it later?
                // TODO - Slash is kept for certain operations
                String requestedName = slugHeaderValue;

                StringEntity interceptedRequestBody = null;
                if (request instanceof HttpEntityEnclosingRequest) {
                    interceptedRequestBody = (StringEntity)((HttpEntityEnclosingRequest) request).getEntity();
                }

                String incomingRequestContentType = null;
                if (incomingRequestHeaders.containsKey(HttpHeaders.CONTENT_TYPE.getValue())) {
                    incomingRequestContentType = incomingRequestHeaders.get(HttpHeaders.CONTENT_TYPE.getValue()).stream().findFirst().orElse(null);
                }
                String incomingRequestShapeTreeUri = null;
                if (incomingRequestLinkHeaders.containsKey(REL_SHAPE_TREE)) {
                    incomingRequestShapeTreeUri = incomingRequestLinkHeaders.get(REL_SHAPE_TREE).stream().findFirst().orElse(null);
                }


                if (incomingRequestShapeTreeUri != null) {
                    // This means we're Planting a new Shape Tree
                    Graph incomingRequestBodyGraph = null;
                    if (interceptedRequestBody != null) {
                        incomingRequestBodyGraph = GraphHelper.readStreamIntoGraph(interceptedRequestBody.getContent(), incomingRequestContentType);
                    }

                    ShapeTreeStep shapeTreeStep = null;
                    try {
                        shapeTreeStep = ShapeTreeFactory.getShapeTreeStep(new URI(incomingRequestShapeTreeUri));
                    } catch (URISyntaxException e) {
                        throw new HttpResponseException(400, "Value of 'ShapeTree' link header is not a value URI");
                    }

                    Boolean alreadyPlanted = ecosystem.containerIsShapeTree(requestRemoteResource.getURI(), shapeTreeStep.getURI());
                    if (!alreadyPlanted) {
                        URI plantedURI = plantShapeTree(authorizationHeaderValue, requestRemoteResource, shapeTreeStep, requestedName, ".", 0);
                        //ecosystem.indexShapeTree(requestRemoteResource.getURI(), shapeTreeStep.getURI(), plantedURI, incomingRequestBodyGraph);
                    }
                }
            }

            // TODO Patch -- handle a local SPARQL update and do validation before passing it on
        }
    }

    @SneakyThrows
    private URI plantShapeTree(String authorizationHeaderValue, RemoteResource parentContainer, ShapeTreeStep shapeTreeStep, String requestedName, String shapeTreePath, int depth) {
        log.debug("plantShapeTree: parent [{}], step [{}], slug [{}], path [{}], depth [{}]", parentContainer.getURI(), shapeTreeStep.getId(), requestedName, shapeTreePath, depth);

        // Create new container with the Slug/Requested Name
        RemoteResource shapeTreeContainer = createContainer(authorizationHeaderValue, parentContainer.getURI(), requestedName);

        String metaDataURIString = shapeTreeContainer.getFirstLinkHeaderValueByRel(REL_DESCRIBEDBY);
        RemoteResource shapeTreeContainerMetadataResource = new RemoteResource(metaDataURIString, authorizationHeaderValue);

        // Get the existing graph
        Graph shapeTreeContainerMetadataGraph = shapeTreeContainerMetadataResource.getGraph();

        // Remove any previous triples for the shapetree planting metadata
        // TODO: What are some cases where this could already exist?
        GraphUtil.remove(shapeTreeContainerMetadataGraph, NodeFactory.createURI(metaDataURIString), NodeFactory.createURI(Namespaces.SHAPETREE_NAMESPACE.getValue() + "shapeTreeRoot"), null);
        GraphUtil.remove(shapeTreeContainerMetadataGraph, NodeFactory.createURI(metaDataURIString), NodeFactory.createURI(Namespaces.SHAPETREE_NAMESPACE.getValue() + "shapeTreeInstancePath"), null);
        GraphUtil.remove(shapeTreeContainerMetadataGraph, NodeFactory.createURI(metaDataURIString), NodeFactory.createURI(Namespaces.SHAPETREE_NAMESPACE.getValue() + "shapeTreeInstanceRoot"), null);

        List<Triple> triplesToAdd = new ArrayList<>();
        triplesToAdd.add(new Triple(NodeFactory.createURI(metaDataURIString), NodeFactory.createURI(Namespaces.SHAPETREE_NAMESPACE.getValue() + "shapeTreeRoot"), NodeFactory.createURI(shapeTreeStep.getId())));
        triplesToAdd.add(new Triple(NodeFactory.createURI(metaDataURIString), NodeFactory.createURI(Namespaces.SHAPETREE_NAMESPACE.getValue() + "shapeTreeInstancePath"), NodeFactory.createLiteral(shapeTreePath)));
        String relativePath = (depth==0) ? "./" : StringUtils.repeat("../", depth);
        triplesToAdd.add(new Triple(NodeFactory.createURI(metaDataURIString), NodeFactory.createURI(Namespaces.SHAPETREE_NAMESPACE.getValue() + "shapeTreeInstanceRoot"), NodeFactory.createURI(relativePath)));
        GraphUtil.add(shapeTreeContainerMetadataGraph, triplesToAdd);
        // Write the updates back to the resource
        shapeTreeContainerMetadataResource.updateGraph(shapeTreeContainerMetadataGraph,false);

        // Recursively call plantShapeTree for any static, nested container contents -- resources and dynamically named containers are ignored
        for (URI contentStepURI : shapeTreeStep.getContents()) {
            ShapeTreeStep contentStep = ShapeTreeFactory.getShapeTreeStep(contentStepURI);
            if (contentStep.getLabel() != null) {
                // the return URI is discarded for recursive calls
                plantShapeTree(authorizationHeaderValue, shapeTreeContainer, contentStep, contentStep.getLabel(), shapeTreePath +"/" + contentStep.getLabel(), ++depth);
            }
        }

        return shapeTreeContainer.getURI();
    }

    @SneakyThrows
    private RemoteResource createContainer(String authorizationHeaderValue, URI parentURI, String requestedName) {
        log.debug("createContainer: parent [{}], slug [{}]", parentURI, requestedName);
        CloseableHttpClient httpClient = HttpClientHelper.getClient(true);
        HttpPost createContainerPost = new HttpPost();
        List<Header> headers = new ArrayList<>();
        headers.add(new BasicHeader(HttpHeaders.SLUG.getValue(), requestedName));
        headers.add(new BasicHeader(HttpHeaders.LINK.getValue(), REL_TYPE_CONTAINER));
        headers.add(new BasicHeader(HttpHeaders.CONTENT_TYPE.getValue(), "text/turtle"));
        headers.add(new BasicHeader(HttpHeaders.AUTHORIZATION.getValue(), authorizationHeaderValue));
        createContainerPost.setHeaders(headers.toArray(new Header[0]));
        createContainerPost.setURI(parentURI);

        CloseableHttpResponse response = httpClient.execute(createContainerPost);
        return new RemoteResource(response);
    }

}
