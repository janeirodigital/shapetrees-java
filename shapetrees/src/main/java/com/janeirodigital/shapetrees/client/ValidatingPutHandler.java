package com.janeirodigital.shapetrees.client;

import com.janeirodigital.shapetrees.RemoteResource;
import com.janeirodigital.shapetrees.ShapeTreeEcosystem;
import com.janeirodigital.shapetrees.ShapeTreeException;
import com.janeirodigital.shapetrees.model.ShapeTreePlantResult;
import com.janeirodigital.shapetrees.model.ShapeTreeStep;
import com.janeirodigital.shapetrees.model.ValidationResult;
import okhttp3.Interceptor;
import okhttp3.Response;
import org.apache.commons.lang3.StringUtils;
import org.apache.jena.graph.Graph;
import org.apache.jena.graph.NodeFactory;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;

public class ValidatingPutHandler extends AbstractValidatingHandler implements ValidatingHandler {

    public ValidatingPutHandler(Interceptor.Chain chain, ShapeTreeEcosystem ecosystem) throws IOException {
        super(chain, ecosystem);
    }

    @Override
    public Response process() throws IOException, URISyntaxException {
        // Get the parent container
        URI parentURI = getParentContainerURI();
        String requestedName = getRequestResourceName();
        RemoteResource parentContainer = new RemoteResource(parentURI, this.authorizationHeaderValue);
        String parentContainerMetaDataURIString = getMetadataResourceURI(parentContainer);
        RemoteResource parentContainerMetadataResource = new RemoteResource(parentContainerMetaDataURIString, this.authorizationHeaderValue);
        Graph parentContainerMetadataGraph = parentContainerMetadataResource.getGraph();
        // Get the shape tree step that manages that container
        boolean shapeTreeManagedContainer = parentContainerMetadataGraph.contains(null, NodeFactory.createURI(SHAPE_TREE_STEP_PREDICATE), null);
        // If managed, do validation
        if (shapeTreeManagedContainer) {
            // This is the ShapeTree step that managed the container we're posting to
            ShapeTreeStep containerShapeTreeStep = getShapeTreeStepFromGraphByPredicate(parentContainerMetadataGraph, SHAPE_TREE_STEP_PREDICATE);
            // This is the ShapeTree step for the root of the ShapeTree that was planted - may or may not be the same as containerShapeTreeStep
            ShapeTreeStep containerShapeTreeRootStep = getShapeTreeStepFromGraphByPredicate(parentContainerMetadataGraph, SHAPE_TREE_ROOT_PREDICATE);
            /* This is the ShapeTree that the container being created must adhere to
               it is identified by traversing the ShapeTree steps contained within containerShapeTreeStep
               and finding the one whose uriTemplate matches the Slug of the container we're about to create
             */
            ShapeTreeStep targetShapeTreeStep = containerShapeTreeStep.findMatchingContainsShapeTreeStep(requestedName);

            Graph incomingRequestBodyGraph = getIncomingBodyGraph(new URI(this.requestRemoteResource.getURI().toString()));

            ValidationResult validationResult = null;
            // If there is a graph to validate...
            if (incomingRequestBodyGraph != null) {
                // ...and a focus node was provided via the focusNode header, then we perform our validation
                if (this.incomingRequestLinkHeaders.get(FOCUS_NODE) != null) {
                    String focusNode = this.incomingRequestLinkHeaders.get(FOCUS_NODE).get(0);
                    URI focusNodeURI = this.requestRemoteResource.getURI().resolve(focusNode);
                    validationResult = targetShapeTreeStep.validateContent(this.authorizationHeaderValue, incomingRequestBodyGraph, focusNodeURI, this.requestRemoteResource.isContainer());
                } else {
                    throw new ShapeTreeException(400, "No Link header with relation " + FOCUS_NODE + " supplied, unable to perform Shape validation");
                }
            }
            // If there is a body graph and it did not pass validation, return an error
            if (incomingRequestBodyGraph != null && !validationResult.getValid()) {
                //  IF not -- create error response (set error code and message and then break)
                throw new ShapeTreeException(400, "Payload did not meet requirements defined by ShapeTree " + targetShapeTreeStep.getURI());
            }

            // Determine if we are trying to created a container
            if (this.requestRemoteResource.isContainer()) {
                // At this point we're trying to create a container inside a managed container
                // This means we're going to effectively plant that targetShapeTreeStep (which comes from the matching URI Template)
                // TODO -- if there is no matching URI template, do we just let the POST happen as-is??  Can someone just create a container
                // TODO -- within a managed container that perhaps doesn't list any contents?
                // TODO -- inquiring minds want to know
                // Determine the depth based on container and the relative depth
                String containerPath = getValueFromGraphByPredicate(parentContainerMetadataGraph, SHAPE_TREE_INSTANCE_PATH_PREDICATE);
                String instanceRoot = getValueFromGraphByPredicate(parentContainerMetadataGraph, SHAPE_TREE_INSTANCE_ROOT_PREDICATE);
                String pathFromRoot = requestRemoteResource.getURI().toString().replace(instanceRoot, "");
                int relativeDepth = StringUtils.countMatches(pathFromRoot, "/") + 1;

                ShapeTreePlantResult result = plantShapeTree(this.authorizationHeaderValue, this.requestRemoteResource, this.incomingRequestBody, containerShapeTreeRootStep, targetShapeTreeStep, requestedName, containerPath + requestedName + "/", relativeDepth);
                return createPlantResponse(result, this.request);
            } else {
                // if we're creating a resource, pass through
                return this.chain.proceed(this.chain.request());
            }
        } else {
            // IF NOT managed, pass through the request to server naturally
            return this.chain.proceed(this.chain.request());
        }
    }
}
