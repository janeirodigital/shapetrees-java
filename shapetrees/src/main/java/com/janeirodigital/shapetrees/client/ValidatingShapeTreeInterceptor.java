package com.janeirodigital.shapetrees.client;

import com.janeirodigital.shapetrees.*;
import com.janeirodigital.shapetrees.enums.HttpHeaders;
import com.janeirodigital.shapetrees.enums.Namespaces;
import com.janeirodigital.shapetrees.helper.GraphHelper;
import com.janeirodigital.shapetrees.helper.HttpClientHelper;
import com.janeirodigital.shapetrees.helper.HttpHeaderHelper;
import com.janeirodigital.shapetrees.model.ShapeTreePlantResult;
import com.janeirodigital.shapetrees.model.ShapeTreeStep;
import com.janeirodigital.shapetrees.model.ValidationResult;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import okhttp3.*;
import okio.Buffer;
import org.apache.commons.lang3.StringUtils;
import org.apache.jena.graph.*;
import org.apache.jena.rdf.model.ModelFactory;
import org.jetbrains.annotations.NotNull;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.*;

@Slf4j
public class ValidatingShapeTreeInterceptor implements Interceptor {

    private static final String REL_SHAPE_TREE = "ShapeTree";
    private static final String REL_DESCRIBEDBY = "acl"; // "describedby"; TODO: This is not working in ESS, using acl for testing
    private static final String POST = "POST";
    private static final String PUT = "PUT";
    private static final String PATCH = "PATCH";
    private static final String DELETE = "DELETE";
    private static final String REL_TYPE = "type";
    private static final String LDP_CONTAINER = "http://www.w3.org/ns/ldp#Container";
    private static final String REL_TYPE_CONTAINER = "<" + LDP_CONTAINER + ">; rel=\"" + REL_TYPE + "\"";
    private static final String SHAPE_TREE_ROOT_PREDICATE = Namespaces.SHAPETREE_NAMESPACE.getValue() + "shapeTreeRoot";
    private static final String SHAPE_TREE_INSTANCE_PATH_PREDICATE = Namespaces.SHAPETREE_NAMESPACE.getValue() + "shapeTreeInstancePath";
    private static final String SHAPE_TREE_INSTANCE_ROOT_PREDICATE = Namespaces.SHAPETREE_NAMESPACE.getValue() + "shapeTreeInstanceRoot";


    private final ShapeTreeEcosystem ecosystem;

    public ValidatingShapeTreeInterceptor(ShapeTreeEcosystem ecosystem) {
        this.ecosystem = ecosystem;
    }


    @NotNull
    @SneakyThrows
    @Override
    public Response intercept(Chain chain) {

        Request request = chain.request();

        // The authorization header is saved so it can be used on other requests
        String authorizationHeaderValue = request.header(HttpHeaders.AUTHORIZATION.getValue());
        RemoteResource requestRemoteResource = new RemoteResource(request.url().uri(), authorizationHeaderValue);
        Map<String, List<String>> incomingRequestHeaders = request.headers().toMultimap();
        Map<String, List<String>> incomingRequestLinkHeaders = HttpHeaderHelper.parseLinkHeadersToMap(request.headers(HttpHeaders.LINK.getValue()));

        Integer responseCode = null;
        String responseMessage = null;
        Response chainedResponse = null;

        switch (request.method()) {
            case POST: {
                // For a POST the requestRemoteResource is the parent container
                // If it doesn't exist, return a 404
                if (!requestRemoteResource.exists()) {
                    responseCode = 404;
                    responseMessage = "Parent Container not found";
                    break;
                }

                String slugHeaderValue;
                if (incomingRequestHeaders.containsKey(HttpHeaders.SLUG.getValue())) {
                    slugHeaderValue = incomingRequestHeaders.get(HttpHeaders.SLUG.getValue()).stream().findFirst().orElse(null);
                } else {
                    slugHeaderValue = "Container";
                }

                String requestedName = slugHeaderValue;

                String incomingRequestContentType = null;
                if (incomingRequestHeaders.containsKey(HttpHeaders.CONTENT_TYPE.getValue())) {
                    incomingRequestContentType = incomingRequestHeaders.get(HttpHeaders.CONTENT_TYPE.getValue()).stream().findFirst().orElse(null);
                }
                String incomingRequestShapeTreeUri = null;
                if (incomingRequestLinkHeaders.containsKey(REL_SHAPE_TREE)) {
                    incomingRequestShapeTreeUri = incomingRequestLinkHeaders.get(REL_SHAPE_TREE).stream().findFirst().orElse(null);
                }

                Buffer buffer = new Buffer();
                request.body().writeTo(buffer);
                String incomingRequestBody = buffer.readUtf8();
                Graph incomingRequestBodyGraph = null;
                if (incomingRequestBody != null && incomingRequestBody.length() > 0) {
                    incomingRequestBodyGraph = GraphHelper.readStringIntoGraph(incomingRequestBody, incomingRequestContentType);
                }

                if (incomingRequestShapeTreeUri != null) {
                    // This means we're Planting a new Shape Tree

                    ShapeTreeStep shapeTreeStep = null;
                    try {
                        shapeTreeStep = ShapeTreeFactory.getShapeTreeStep(new URI(incomingRequestShapeTreeUri));
                    } catch (URISyntaxException e) {
                        responseCode = 400;
                        responseMessage = "Value of 'ShapeTree' link header is not a value URI";
                        break;
                    }

                    ShapeTreePlantResult existingPlantedShapeTree = ecosystem.getExistingShapeTreeFromContainer(requestRemoteResource.getURI(), shapeTreeStep.getURI());
                    if (existingPlantedShapeTree.getRootContainer() == null) {
                        ShapeTreePlantResult plantResult = plantShapeTree(authorizationHeaderValue, requestRemoteResource, shapeTreeStep, requestedName, ".", 0);
                        ecosystem.indexShapeTree(requestRemoteResource.getURI(), shapeTreeStep.getURI(), plantResult.getRootContainer());
                        return createPlantResponse(plantResult, request);
                    } else {
                        return createPlantResponse(existingPlantedShapeTree, request);
                    }
                } else {
                    // This is a POST without a shapetree link
                    // Determine if the container we're posting to is managed or not
                    String containerMetaDataURIString = requestRemoteResource.getFirstLinkHeaderValueByRel(REL_DESCRIBEDBY);
                    RemoteResource containerMetadataResource = new RemoteResource(containerMetaDataURIString, authorizationHeaderValue);
                    Graph containerMetadataGraph = containerMetadataResource.getGraph();
                    Boolean shapeTreeManagedContainer = containerMetadataGraph.contains(null, NodeFactory.createURI(SHAPE_TREE_ROOT_PREDICATE), null);
                    // If managed, do validation
                    if (shapeTreeManagedContainer) {
                        List<Triple> shapeTreeTriple = containerMetadataGraph.find(null, NodeFactory.createURI(SHAPE_TREE_ROOT_PREDICATE), null).toList();
                        if (shapeTreeTriple != null && shapeTreeTriple.size() > 1) {
                            responseCode = 500;
                            responseMessage = "Multiple triples containing " + SHAPE_TREE_ROOT_PREDICATE + " - only one expected";
                            break;
                        }
                        String shapeTreeStepURI = shapeTreeTriple.get(0).getObject().getURI();
                        ShapeTreeStep targetShapeTreeStep = ShapeTreeFactory.getShapeTreeStep(new URI(shapeTreeStepURI));

                        // TODO How to get the focus node for validation -- not folling links.root in JS implementation
                        ValidationResult validationResult = targetShapeTreeStep.validateContent(authorizationHeaderValue, incomingRequestBodyGraph, null);
                        if (validationResult.getValid()) {
                            //  IF successful -- pass through
                            chainedResponse = chain.proceed(chain.request());
                        } else {
                            //  IF not -- create error response (set error code and message and then break)
                            responseCode = 400;
                            responseMessage = "Payload did not meet requirements defined by ShapeTree";
                            break;
                        }
                    } else {
                        // IF NOT managed, pass through the request to server naturally
                        chainedResponse = chain.proceed(chain.request());
                    }

                    // Determine if what was just created was a container
                    if (incomingRequestLinkHeaders.get(REL_TYPE).contains(LDP_CONTAINER)) {
                        // TODO
                        // if so, look at its shapetree to figure out if we should be creating nested containers in it
                        // should be able to use the plantShapeTree method to do this since it is already doing that
                    }


                    return chainedResponse;
                }
            }
            case PUT:
                // TODO -- Is there any logic to choose the appropriate step based on the name of the resource
                // TODO -- matching the uriTemplate?
                break;
            case PATCH:
                // TODO Patch -- handle a local SPARQL update and do validation before passing it on
                break;
            case DELETE:
                break;
            default:
                break;
        }

        if (responseCode == null || responseMessage == null) {
            log.error("Dropped to bottom of interceptor without a statusCode or statusMessage");
            responseCode = 999;
            responseMessage = "Undefined error";
        }

        // TODO this is where we handle an error in our processing and manufacture a response
        // Should talk about the formatting we want to use here
        Response response = new Response.Builder()
                .code(responseCode)
                .body(ResponseBody.create(responseMessage, MediaType.get("text/plain")))
                .request(request)
                .protocol(Protocol.HTTP_1_1)
                .message(responseMessage)
                .build();
        return response;
    }

    private Response createPlantResponse(ShapeTreePlantResult plantResult, Request request) {
        Response response = new Response.Builder()
                .code(201)
                .addHeader(HttpHeaders.LOCATION.getValue(), plantResult.getRootContainer().toString())
                .addHeader(HttpHeaders.LINK.getValue(), "<" + plantResult.getRootContainerMetadata().toString() + ">; rel=\"" + REL_DESCRIBEDBY + "\"")
                .addHeader(HttpHeaders.CONTENT_TYPE.getValue(), "text/turtle")
                .body(ResponseBody.create("", MediaType.get("text/turtle")))
                .request(request)
                .protocol(Protocol.HTTP_1_1)
                .message("Created")
                .build();
        return response;
    }

    @SneakyThrows
    private ShapeTreePlantResult plantShapeTree(String authorizationHeaderValue, RemoteResource parentContainer, ShapeTreeStep shapeTreeStep, String requestedName, String shapeTreePath, int depth) {
        log.debug("plantShapeTree: parent [{}], step [{}], slug [{}], path [{}], depth [{}]", parentContainer.getURI(), shapeTreeStep.getId(), requestedName, shapeTreePath, depth);

        // Create new container with the Slug/Requested Name
        RemoteResource shapeTreeContainer = createContainer(authorizationHeaderValue, parentContainer.getURI(), requestedName);
        // TODO!!!
        // TODO!!!  This next line is a work around.  As of 6/25/2020, ESS does not return the proper headers
        // TODO!!!  After creating a container, as a result, we are GETting the same URI again so we can get
        // TODO!!!  the appropriate headers.  For example, after creating a container the response headers
        // TODO!!!  include the acl/effectiveAcl headers for the parent container, not the newly created one
        // TODO!!!
        shapeTreeContainer = new RemoteResource(shapeTreeContainer.getURI(), authorizationHeaderValue);


        String metaDataURIString = getMetadataResourceURI(shapeTreeContainer);
        RemoteResource shapeTreeContainerMetadataResource = new RemoteResource(metaDataURIString, authorizationHeaderValue);

        // Get the existing graph
        Graph shapeTreeContainerMetadataGraph;
        if (shapeTreeContainerMetadataResource.exists()) {
            shapeTreeContainerMetadataGraph = shapeTreeContainerMetadataResource.getGraph();

            // Remove any previous triples for the shapetree planting metadata
            GraphUtil.remove(shapeTreeContainerMetadataGraph, NodeFactory.createURI(metaDataURIString), NodeFactory.createURI(SHAPE_TREE_ROOT_PREDICATE), null);
            GraphUtil.remove(shapeTreeContainerMetadataGraph, NodeFactory.createURI(metaDataURIString), NodeFactory.createURI(SHAPE_TREE_INSTANCE_PATH_PREDICATE), null);
            GraphUtil.remove(shapeTreeContainerMetadataGraph, NodeFactory.createURI(metaDataURIString), NodeFactory.createURI(SHAPE_TREE_INSTANCE_ROOT_PREDICATE), null);
        }

        shapeTreeContainerMetadataGraph = ModelFactory.createDefaultModel().getGraph();

        List<Triple> triplesToAdd = new ArrayList<>();
        triplesToAdd.add(new Triple(NodeFactory.createURI(metaDataURIString), NodeFactory.createURI(SHAPE_TREE_ROOT_PREDICATE), NodeFactory.createURI(shapeTreeStep.getId())));
        triplesToAdd.add(new Triple(NodeFactory.createURI(metaDataURIString), NodeFactory.createURI(SHAPE_TREE_INSTANCE_PATH_PREDICATE), NodeFactory.createLiteral(shapeTreePath)));
        String relativePath = (depth==0) ? "./" : StringUtils.repeat("../", depth);
        triplesToAdd.add(new Triple(NodeFactory.createURI(metaDataURIString), NodeFactory.createURI(SHAPE_TREE_INSTANCE_ROOT_PREDICATE), NodeFactory.createURI(relativePath)));
        GraphUtil.add(shapeTreeContainerMetadataGraph, triplesToAdd);
        // Write the updates back to the resource
        shapeTreeContainerMetadataResource.updateGraph(shapeTreeContainerMetadataGraph,false, authorizationHeaderValue);

        List<URI> nestedContainersCreated = new ArrayList<>();

        // Recursively call plantShapeTree for any static, nested container contents -- resources and dynamically named containers are ignored
        for (URI contentStepURI : shapeTreeStep.getContents()) {
            ShapeTreeStep contentStep = ShapeTreeFactory.getShapeTreeStep(contentStepURI);
            if (contentStep.getLabel() != null) {
                // the return URI is discarded for recursive calls
                ShapeTreePlantResult nestedResult = plantShapeTree(authorizationHeaderValue, shapeTreeContainer, contentStep, contentStep.getLabel(), shapeTreePath +"/" + contentStep.getLabel(), ++depth);
                nestedContainersCreated.add(nestedResult.getRootContainer());
            }
        }

        return new ShapeTreePlantResult(shapeTreeContainer.getURI(), shapeTreeContainerMetadataResource.getURI(), nestedContainersCreated);
    }

    @NotNull
    private String getMetadataResourceURI(RemoteResource shapeTreeContainer) {
        // This header approach is not currently working, instead, we're going to use a separate metadata file
        /*
        String metaDataURIString = shapeTreeContainer.getFirstLinkHeaderValueByRel(REL_DESCRIBEDBY);
        if (metaDataURIString.startsWith("/")) {
            // If the header value doesn't include scheme/host, prefix it with the scheme & host from container
            URI shapeTreeContainerURI = shapeTreeContainer.getURI();
            metaDataURIString = shapeTreeContainerURI.getScheme() + "://" + shapeTreeContainerURI.getHost() + shapeTreeContainer.getFirstLinkHeaderValueByRel(REL_DESCRIBEDBY);
        }*/

        return shapeTreeContainer.getURI() + ".meta";
    }

    @SneakyThrows
    private RemoteResource createContainer(String authorizationHeaderValue, URI parentURI, String requestedName) {
        log.debug("createContainer: parent [{}], slug [{}]", parentURI, requestedName);
        OkHttpClient httpClient = HttpClientHelper.getClient(true);
        Request createContainerPost = new Request.Builder()
                .addHeader(HttpHeaders.SLUG.getValue(), requestedName)
                .addHeader(HttpHeaders.LINK.getValue(), REL_TYPE_CONTAINER)
                .addHeader(HttpHeaders.CONTENT_TYPE.getValue(), "text/turtle")
                .addHeader(HttpHeaders.AUTHORIZATION.getValue(), authorizationHeaderValue)
                .post(RequestBody.create(new byte[]{}))
                .url(parentURI.toURL()).build();

        Response response = httpClient.newCall(createContainerPost).execute();
        return new RemoteResource(response);
    }

}
