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
import org.apache.jena.update.UpdateAction;
import org.apache.jena.update.UpdateFactory;
import org.apache.jena.update.UpdateRequest;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.*;

@Slf4j
public class ValidatingShapeTreeInterceptor implements Interceptor {

    public static final String FOCUS_NODE = "focusNode";
    private static final String REL_SHAPE_TREE = "ShapeTree";
    private static final String REL_DESCRIBEDBY = "describedby";
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
    private static final String SHAPE_TREE_STEP_PREDICATE = Namespaces.SHAPETREE_NAMESPACE.getValue() + "shapeTreeStep";


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

        String incomingRequestContentType = null;
        if (incomingRequestHeaders.containsKey(HttpHeaders.CONTENT_TYPE.getValue())) {
            incomingRequestContentType = incomingRequestHeaders.get(HttpHeaders.CONTENT_TYPE.getValue()).stream().findFirst().orElse(null);
        }


        Integer responseCode = null;
        String responseMessage = null;

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

                String incomingRequestShapeTreeUri = null;
                if (incomingRequestLinkHeaders.containsKey(REL_SHAPE_TREE)) {
                    incomingRequestShapeTreeUri = incomingRequestLinkHeaders.get(REL_SHAPE_TREE).stream().findFirst().orElse(null);
                }

                Buffer buffer = new Buffer();
                request.body().writeTo(buffer);
                String incomingRequestBody = buffer.readUtf8();
                Graph incomingRequestBodyGraph = null;
                if (incomingRequestBody != null && incomingRequestBody.length() > 0) {
                    incomingRequestBodyGraph = GraphHelper.readStringIntoGraph(incomingRequestBody, new URI(requestRemoteResource.getURI().toString() + requestedName), incomingRequestContentType);
                }

                if (incomingRequestShapeTreeUri != null) {
                    // This means we're Planting a new Shape Tree

                    ShapeTreeStep shapeTreeStep;
                    try {
                        shapeTreeStep = ShapeTreeFactory.getShapeTreeStep(new URI(incomingRequestShapeTreeUri));
                    } catch (URISyntaxException e) {
                        responseCode = 400;
                        responseMessage = "Value of 'ShapeTree' link header is not a value URI";
                        break;
                    }

                    ShapeTreePlantResult existingPlantedShapeTree = ecosystem.getExistingShapeTreeFromContainer(requestRemoteResource.getURI(), shapeTreeStep.getURI());
                    if (existingPlantedShapeTree.getRootContainer() == null) {
                        ShapeTreePlantResult plantResult = plantShapeTree(authorizationHeaderValue, requestRemoteResource, shapeTreeStep, shapeTreeStep, requestedName, ".", 0);
                        ecosystem.indexShapeTree(requestRemoteResource.getURI(), shapeTreeStep.getURI(), plantResult.getRootContainer());
                        return createPlantResponse(plantResult, request);
                    } else {
                        return createPlantResponse(existingPlantedShapeTree, request);
                    }
                } else {
                    // This is a POST without a shapetree link
                    // Determine if the container we're posting to is managed or not
                    String containerMetaDataURIString = getMetadataResourceURI(requestRemoteResource);
                    RemoteResource containerMetadataResource = new RemoteResource(containerMetaDataURIString, authorizationHeaderValue);
                    Graph containerMetadataGraph = containerMetadataResource.getGraph();
                    boolean shapeTreeManagedContainer = containerMetadataGraph.contains(null, NodeFactory.createURI(SHAPE_TREE_STEP_PREDICATE), null);
                    // If managed, do validation
                    if (shapeTreeManagedContainer) {
                        ShapeTreeStep containerShapeTreeStep, containerShapeTreeRootStep;
                        try {
                            // This is the ShapeTree step that managed the container we're posting to
                            containerShapeTreeStep = getShapeTreeStepFromGraphByPredicate(containerMetadataGraph, SHAPE_TREE_STEP_PREDICATE);
                            /* This is the ShapeTree step for the root of the ShapeTree that was planted -
                               may or may not be the same as containerShapeTreeStep
                             */
                            containerShapeTreeRootStep = getShapeTreeStepFromGraphByPredicate(containerMetadataGraph, SHAPE_TREE_ROOT_PREDICATE);
                        } catch (Exception ex) {
                            responseCode = 400;
                            responseMessage = ex.getMessage();
                            break;
                        }

                        /* This is the ShapeTree that the container being created must adhere to
                           it is identified by traversing the ShapeTree steps contained within containerShapeTreeStep
                           and finding the one whose uriTemplate matches the Slug of the container we're about to create
                         */
                        ShapeTreeStep targetShapeTreeStep = containerShapeTreeStep.findMatchingContainsShapeTreeStep(requestedName);

                        Boolean isContainer = incomingRequestLinkHeaders.get(REL_TYPE).contains(LDP_CONTAINER);

                        ValidationResult validationResult = null;
                        // If there is a graph to validate...
                        if (incomingRequestBodyGraph != null) {
                            // ...and a focus node was provided via the focusNode header, then we perform our validation
                            if (incomingRequestLinkHeaders != null && incomingRequestLinkHeaders.get(FOCUS_NODE) != null) {
                                String focusNode = incomingRequestLinkHeaders.get(FOCUS_NODE).get(0);
                                URI focusNodeURI = new URI(requestRemoteResource.getURI() + requestedName).resolve(focusNode);
                                validationResult = targetShapeTreeStep.validateContent(authorizationHeaderValue, incomingRequestBodyGraph, focusNodeURI, isContainer);
                            } else {
                                // ...but no focus node, we return an error
                                responseCode = 400;
                                responseMessage = "No Link header with relation " + FOCUS_NODE + " supplied, unable to perform Shape validation";
                                break;
                            }
                        }
                        // If there is a body graph and it did not pass validation, return an error
                        if (incomingRequestBodyGraph != null && !validationResult.getValid()) {
                            //  IF not -- create error response (set error code and message and then break)
                            responseCode = 400;
                            responseMessage = "Payload did not meet requirements defined by ShapeTree " + targetShapeTreeStep.getURI();
                            break;
                        }

                        // Determine if we are trying to created a container
                        if (isContainer) {
                            // At this point we're trying to create a container inside a managed container
                            // This means we're going to effectively plant that targetShapeTreeStep (which comes from the matching URI Template)
                            // TODO -- if there is no matching URI template, do we just let the POST happen as-is??  Can someone just create a container
                            // TODO -- within a managed container that perhaps doesn't list any contents?
                            // TODO -- inquiring minds want to know
                            ShapeTreePlantResult result = plantShapeTree(authorizationHeaderValue, requestRemoteResource, containerShapeTreeRootStep, targetShapeTreeStep, requestedName, ".", 0);
                            return createPlantResponse(result, request);
                        } else {
                            // if we're creating a resource, pass through
                            return chain.proceed(chain.request());
                        }
                    } else {
                        // IF NOT managed, pass through the request to server naturally
                        return chain.proceed(chain.request());
                    }
                }
            }
            case PUT: {
                // Get the parent container
                URI parentURI = requestRemoteResource.getURI().resolve(requestRemoteResource.isContainer() ? ".." : ".");
                // Remove the parent URI from the full request, leaving the resource name itself
                String requestedName = requestRemoteResource.getURI().toString().replace(parentURI.toString(), "");
                RemoteResource parentContainer = new RemoteResource(parentURI, authorizationHeaderValue);
                String parentContainerMetaDataURIString = getMetadataResourceURI(parentContainer);
                RemoteResource parentContainerMetadataResource = new RemoteResource(parentContainerMetaDataURIString, authorizationHeaderValue);
                Graph parentContainerMetadataGraph = parentContainerMetadataResource.getGraph();
                // Get the shape tree step that manages that container
                boolean shapeTreeManagedContainer = parentContainerMetadataGraph.contains(null, NodeFactory.createURI(SHAPE_TREE_STEP_PREDICATE), null);
                // If managed, do validation
                if (shapeTreeManagedContainer) {
                    ShapeTreeStep containerShapeTreeStep, containerShapeTreeRootStep;
                    try {
                        // This is the ShapeTree step that managed the container we're posting to
                        containerShapeTreeStep = getShapeTreeStepFromGraphByPredicate(parentContainerMetadataGraph, SHAPE_TREE_STEP_PREDICATE);
                            /* This is the ShapeTree step for the root of the ShapeTree that was planted -
                               may or may not be the same as containerShapeTreeStep
                             */
                        containerShapeTreeRootStep = getShapeTreeStepFromGraphByPredicate(parentContainerMetadataGraph, SHAPE_TREE_ROOT_PREDICATE);
                    } catch (Exception ex) {
                        responseCode = 400;
                        responseMessage = ex.getMessage();
                        break;
                    }

                    /* This is the ShapeTree that the container being created must adhere to
                       it is identified by traversing the ShapeTree steps contained within containerShapeTreeStep
                       and finding the one whose uriTemplate matches the Slug of the container we're about to create
                     */
                    ShapeTreeStep targetShapeTreeStep = containerShapeTreeStep.findMatchingContainsShapeTreeStep(requestedName);


                    Buffer buffer = new Buffer();
                    request.body().writeTo(buffer);
                    String incomingRequestBody = buffer.readUtf8();
                    Graph incomingRequestBodyGraph = null;
                    if (incomingRequestBody.length() > 0) {
                        incomingRequestBodyGraph = GraphHelper.readStringIntoGraph(incomingRequestBody, new URI(requestRemoteResource.getURI().toString() + requestedName), incomingRequestContentType);
                    }

                    ValidationResult validationResult = null;
                    // If there is a graph to validate...
                    if (incomingRequestBodyGraph != null) {
                        // ...and a focus node was provided via the focusNode header, then we perform our validation
                        if (incomingRequestLinkHeaders != null && incomingRequestLinkHeaders.get(FOCUS_NODE) != null) {
                            String focusNode = incomingRequestLinkHeaders.get(FOCUS_NODE).get(0);
                            URI focusNodeURI = requestRemoteResource.getURI().resolve(focusNode);
                            validationResult = targetShapeTreeStep.validateContent(authorizationHeaderValue, incomingRequestBodyGraph, focusNodeURI, requestRemoteResource.isContainer());
                        } else {
                            // ...but no focus node, we return an error
                            responseCode = 400;
                            responseMessage = "No Link header with relation " + FOCUS_NODE + " supplied, unable to perform Shape validation";
                            break;
                        }
                    }
                    // If there is a body graph and it did not pass validation, return an error
                    if (incomingRequestBodyGraph != null && !validationResult.getValid()) {
                        //  IF not -- create error response (set error code and message and then break)
                        responseCode = 400;
                        responseMessage = "Payload did not meet requirements defined by ShapeTree " + targetShapeTreeStep.getURI();
                        break;
                    }

                    // Determine if we are trying to created a container
                    if (incomingRequestLinkHeaders.get(REL_TYPE).contains(LDP_CONTAINER)) {
                        // At this point we're trying to create a container inside a managed container
                        // This means we're going to effectively plant that targetShapeTreeStep (which comes from the matching URI Template)
                        // TODO -- if there is no matching URI template, do we just let the POST happen as-is??  Can someone just create a container
                        // TODO -- within a managed container that perhaps doesn't list any contents?
                        // TODO -- inquiring minds want to know
                        ShapeTreePlantResult result = plantShapeTree(authorizationHeaderValue, requestRemoteResource, containerShapeTreeRootStep, targetShapeTreeStep, requestedName, ".", 0);
                        return createPlantResponse(result, request);
                    } else {
                        // if we're creating a resource, pass through
                        return chain.proceed(chain.request());
                    }
                } else {
                    // IF NOT managed, pass through the request to server naturally
                    return chain.proceed(chain.request());
                }
            }
            case PATCH: {
                // TODO Patch -- handle a local SPARQL update and do validation before passing it on
                if (incomingRequestContentType == null || !incomingRequestContentType.toLowerCase().equals("application/sqarl-update")) {
                    log.error("Received a patch without a content type of application/sparql-update");
                    responseCode = 400;
                    responseMessage = "PATCH verb expects a content type of application/sparql-update";
                    break;
                }
                // Get the parent container
                URI parentURI = requestRemoteResource.getURI().resolve(requestRemoteResource.isContainer() ? ".." : ".");
                // Remove the parent URI from the full request, leaving the resource name itself
                String requestedName = requestRemoteResource.getURI().toString().replace(parentURI.toString(), "");
                RemoteResource parentContainer = new RemoteResource(parentURI, authorizationHeaderValue);
                String parentContainerMetaDataURIString = getMetadataResourceURI(parentContainer);
                RemoteResource parentContainerMetadataResource = new RemoteResource(parentContainerMetaDataURIString, authorizationHeaderValue);
                Graph parentContainerMetadataGraph = parentContainerMetadataResource.getGraph();
                // Get the shape tree step that manages that container
                boolean shapeTreeManagedContainer = parentContainerMetadataGraph.contains(null, NodeFactory.createURI(SHAPE_TREE_STEP_PREDICATE), null);
                // If managed, do validation
                if (shapeTreeManagedContainer) {
                    ShapeTreeStep containerShapeTreeStep;
                    try {
                        // This is the ShapeTree step that managed the container we're posting to
                        containerShapeTreeStep = getShapeTreeStepFromGraphByPredicate(parentContainerMetadataGraph, SHAPE_TREE_STEP_PREDICATE);
                    } catch (Exception ex) {
                        responseCode = 400;
                        responseMessage = ex.getMessage();
                        break;
                    }

                    /* This is the ShapeTree that the container being created must adhere to
                       it is identified by traversing the ShapeTree steps contained within containerShapeTreeStep
                       and finding the one whose uriTemplate matches the Slug of the container we're about to create
                     */
                    ShapeTreeStep targetShapeTreeStep = containerShapeTreeStep.findMatchingContainsShapeTreeStep(requestedName);
                    Graph existingResourceGraph = requestRemoteResource.getGraph();
                    Buffer buffer = new Buffer();
                    if (request.body() != null) {
                        request.body().writeTo(buffer);
                    }
                    String incomingRequestBody = buffer.readUtf8();

                    UpdateRequest updateRequest = UpdateFactory.create(incomingRequestBody);
                    UpdateAction.execute(updateRequest, existingResourceGraph);
                    ValidationResult validationResult;
                    if (incomingRequestLinkHeaders != null && incomingRequestLinkHeaders.get(FOCUS_NODE) != null) {
                        String focusNode = incomingRequestLinkHeaders.get(FOCUS_NODE).get(0);
                        URI focusNodeURI = requestRemoteResource.getURI().resolve(focusNode);
                        validationResult = targetShapeTreeStep.validateContent(authorizationHeaderValue, existingResourceGraph, focusNodeURI, requestRemoteResource.isContainer());
                    } else {
                        // ...but no focus node, we return an error
                        responseCode = 400;
                        responseMessage = "No Link header with relation " + FOCUS_NODE + " supplied, unable to perform Shape validation";
                        break;
                    }

                    if (validationResult.getValid()) {
                        // If the result of the locally applied PATCH validates, then pass it to the server
                        return chain.proceed(chain.request());
                    } else {
                        responseCode = 400;
                        responseMessage = "Payload did not meet requirements defined by ShapeTree " + targetShapeTreeStep.getURI();
                        break;
                    }
                } else {
                    // If the parent container is managed, then pass through the PATCH
                    return chain.proceed(chain.request());
                }
            }
            case DELETE:
                break;
            default:
                return chain.proceed(chain.request());
        }

        if (responseCode == null || responseMessage == null) {
            log.error("Dropped to bottom of interceptor without a statusCode or statusMessage");
            responseCode = 999;
            responseMessage = "Undefined error";
        }

        // TODO this is where we handle an error in our processing and manufacture a response
        // Should talk about the formatting we want to use here
        return new Response.Builder()
                .code(responseCode)
                .body(ResponseBody.create(responseMessage, MediaType.get("text/plain")))
                .request(request)
                .protocol(Protocol.HTTP_1_1)
                .message(responseMessage)
                .build();
    }

    private ShapeTreeStep getShapeTreeStepFromGraphByPredicate(Graph graph, String predicate) throws Exception {
        List<Triple> triples = graph.find(null, NodeFactory.createURI(predicate), null).toList();
        if (triples == null || triples.size() == 0) {
            throw new Exception("No triples containing " + predicate + " - one expected");
        }
        if (triples.size() > 1) {
            throw new Exception("Multiple triples containing " + predicate + " - only one expected");
        }
        String stepURI = triples.get(0).getObject().getURI();
        return ShapeTreeFactory.getShapeTreeStep(new URI(stepURI));
    }

    private Response createPlantResponse(ShapeTreePlantResult plantResult, Request request) {
        return new Response.Builder()
                .code(201)
                .addHeader(HttpHeaders.LOCATION.getValue(), plantResult.getRootContainer().toString())
                .addHeader(HttpHeaders.LINK.getValue(), "<" + plantResult.getRootContainerMetadata().toString() + ">; rel=\"" + REL_DESCRIBEDBY + "\"")
                .addHeader(HttpHeaders.CONTENT_TYPE.getValue(), "text/turtle")
                .body(ResponseBody.create("", MediaType.get("text/turtle")))
                .request(request)
                .protocol(Protocol.HTTP_1_1)
                .message("Created")
                .build();
    }

    @SneakyThrows
    private ShapeTreePlantResult plantShapeTree(String authorizationHeaderValue, RemoteResource parentContainer, ShapeTreeStep rootShapeTreeStep, ShapeTreeStep shapeTreeStep, String requestedName, String shapeTreePath, int depth) {
        log.debug("plantShapeTree: parent [{}], root step [{}], step [{}], slug [{}], path [{}], depth [{}]", parentContainer.getURI(), rootShapeTreeStep.getId(), shapeTreeStep.getId(), requestedName, shapeTreePath, depth);

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
            GraphUtil.remove(shapeTreeContainerMetadataGraph, NodeFactory.createURI(metaDataURIString), NodeFactory.createURI(SHAPE_TREE_STEP_PREDICATE), null);
        }

        shapeTreeContainerMetadataGraph = ModelFactory.createDefaultModel().getGraph();

        List<Triple> triplesToAdd = new ArrayList<>();
        triplesToAdd.add(new Triple(NodeFactory.createURI(metaDataURIString), NodeFactory.createURI(SHAPE_TREE_ROOT_PREDICATE), NodeFactory.createURI(shapeTreeStep.getId())));
        triplesToAdd.add(new Triple(NodeFactory.createURI(metaDataURIString), NodeFactory.createURI(SHAPE_TREE_INSTANCE_PATH_PREDICATE), NodeFactory.createLiteral(shapeTreePath)));
        String relativePath = (depth==0) ? "./" : StringUtils.repeat("../", depth);
        triplesToAdd.add(new Triple(NodeFactory.createURI(metaDataURIString), NodeFactory.createURI(SHAPE_TREE_INSTANCE_ROOT_PREDICATE), NodeFactory.createURI(relativePath)));
        triplesToAdd.add(new Triple(NodeFactory.createURI(metaDataURIString), NodeFactory.createURI(SHAPE_TREE_STEP_PREDICATE), NodeFactory.createURI(rootShapeTreeStep.getId())));
        GraphUtil.add(shapeTreeContainerMetadataGraph, triplesToAdd);
        // Write the updates back to the resource
        shapeTreeContainerMetadataResource.updateGraph(shapeTreeContainerMetadataGraph,false, authorizationHeaderValue);

        List<URI> nestedContainersCreated = new ArrayList<>();

        // Recursively call plantShapeTree for any static, nested container contents -- resources and dynamically named containers are ignored
        for (URI contentStepURI : shapeTreeStep.getContents()) {
            ShapeTreeStep contentStep = ShapeTreeFactory.getShapeTreeStep(contentStepURI);
            if (contentStep.getLabel() != null) {
                // the return URI is discarded for recursive calls
                ShapeTreePlantResult nestedResult = plantShapeTree(authorizationHeaderValue, shapeTreeContainer, rootShapeTreeStep, contentStep, contentStep.getLabel(), shapeTreePath +"/" + contentStep.getLabel(), ++depth);
                nestedContainersCreated.add(nestedResult.getRootContainer());
            }
        }

        return new ShapeTreePlantResult(shapeTreeContainer.getURI(), shapeTreeContainerMetadataResource.getURI(), nestedContainersCreated);
    }

    @NotNull
    private String getMetadataResourceURI(RemoteResource shapeTreeContainer) throws IOException {
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
