package com.janeirodigital.shapetrees.client;

import com.janeirodigital.shapetrees.RemoteResource;
import com.janeirodigital.shapetrees.ShapeTreeEcosystem;
import com.janeirodigital.shapetrees.ShapeTreeException;
import com.janeirodigital.shapetrees.ShapeTreeFactory;
import com.janeirodigital.shapetrees.enums.HttpHeaders;
import com.janeirodigital.shapetrees.model.ShapeTreePlantResult;
import com.janeirodigital.shapetrees.model.ShapeTreeStep;
import com.janeirodigital.shapetrees.model.ValidationResult;
import okhttp3.Interceptor;
import okhttp3.Response;
import org.apache.jena.graph.Graph;
import org.apache.jena.graph.NodeFactory;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;

public class ValidatingPostHandler extends AbstractValidatingHandler implements ValidatingHandler {

    private static final String REL_SHAPE_TREE = "ShapeTree";

    public ValidatingPostHandler(Interceptor.Chain chain, ShapeTreeEcosystem ecosystem) throws IOException {
        super(chain, ecosystem);
    }

    @Override
    public Response process() throws IOException, URISyntaxException {
        ensureRequestResourceExists("Parent Container not found");

        String requestedName = getIncomingHeaderValueWithDefault(HttpHeaders.SLUG.getValue(), "Container");
        String incomingRequestShapeTreeUri = getIncomingLinkHeaderByRelationValue(REL_SHAPE_TREE);

        Graph incomingRequestBodyGraph = getIncomingBodyGraph(new URI(this.requestRemoteResource.getURI().toString() + requestedName));

        if (incomingRequestShapeTreeUri != null) {
            // This means we're Planting a new Shape Tree

            // Retrieve the ShapeTreeStep we're trying to plant
            ShapeTreeStep shapeTreeStep;
            try {
                shapeTreeStep = ShapeTreeFactory.getShapeTreeStep(new URI(incomingRequestShapeTreeUri));
            } catch (URISyntaxException e) {
                throw new ShapeTreeException(400, "Value of 'ShapeTree' link header is not a value URI");
            }

            // Determine if the ecosystem already has a planted ShapeTree we can reuse
            ShapeTreePlantResult existingPlantedShapeTree = this.ecosystem.getExistingShapeTreeFromContainer(this.requestRemoteResource.getURI(), shapeTreeStep.getURI());
            if (existingPlantedShapeTree.getRootContainer() == null) {
                // If not, plant the ShapeTree
                ShapeTreePlantResult plantResult = plantShapeTree(this.authorizationHeaderValue, this.requestRemoteResource, shapeTreeStep, shapeTreeStep, requestedName, ".", 0);
                // Provide to the ecosystem to index
                this.ecosystem.indexShapeTree(requestRemoteResource.getURI(), shapeTreeStep.getURI(), plantResult.getRootContainer());
                // Create and return a response
                return createPlantResponse(plantResult, this.request);
            } else {
                // If an existing ShapeTree exists, nothing to do here
                return createPlantResponse(existingPlantedShapeTree, this.request);
            }
        } else {
            // This is a POST without a ShapeTree link header, meaning we're doing a POST to either a managed or unmanaged container
            // Determine if the container we're posting to is managed or not
            // Get URI of the POST container's metadata resource
            String containerMetaDataURIString = getMetadataResourceURI(this.requestRemoteResource);
            // Dereference the container's metadata resource
            RemoteResource containerMetadataResource = new RemoteResource(containerMetaDataURIString, this.authorizationHeaderValue);
            // Read the graph of the container's metadata resource
            Graph containerMetadataGraph = containerMetadataResource.getGraph();
            // If that graph contains the SHAPE_TREE_STEP_PREDICATE it means the container being POSTed to is a managed container
            boolean shapeTreeManagedContainer = containerMetadataGraph.contains(null, NodeFactory.createURI(SHAPE_TREE_STEP_PREDICATE), null);
            // If managed, do validation
            if (shapeTreeManagedContainer) {
                // This is the ShapeTree step that managed the container we're posting to
                ShapeTreeStep containerShapeTreeStep = getShapeTreeStepFromGraphByPredicate(containerMetadataGraph, SHAPE_TREE_STEP_PREDICATE);
                // This is the ShapeTree step for the root of the ShapeTree that was planted - may or may not be the same as containerShapeTreeStep
                ShapeTreeStep containerShapeTreeRootStep = getShapeTreeStepFromGraphByPredicate(containerMetadataGraph, SHAPE_TREE_ROOT_PREDICATE);

                /* This is the ShapeTree that the container being created must adhere to
                   it is identified by traversing the ShapeTree steps contained within containerShapeTreeStep
                   and finding the one whose uriTemplate matches the Slug of the container we're about to create
                 */
                ShapeTreeStep targetShapeTreeStep = containerShapeTreeStep.findMatchingContainsShapeTreeStep(requestedName);

                // Look at the incoming Link headers for a relation of 'type' to determine if a Container is being created with the POST
                Boolean isContainer = this.incomingRequestLinkHeaders.get(REL_TYPE).contains(LDP_CONTAINER);

                ValidationResult validationResult = null;
                // If there is a graph to validate...
                if (incomingRequestBodyGraph != null) {
                    // ...and a focus node was provided via the focusNode header, then we perform our validation
                    if (this.incomingRequestLinkHeaders.get(FOCUS_NODE) != null) {
                        String focusNode = this.incomingRequestLinkHeaders.get(FOCUS_NODE).get(0);
                        URI focusNodeURI = new URI(this.requestRemoteResource.getURI() + requestedName).resolve(focusNode);
                        validationResult = targetShapeTreeStep.validateContent(this.authorizationHeaderValue, incomingRequestBodyGraph, focusNodeURI, isContainer);
                    } else {
                        throw new ShapeTreeException(400, "No Link header with relation " + FOCUS_NODE + " supplied, unable to perform Shape validation");
                    }
                }
                // If there is a body graph and it did not pass validation, return an error
                if (incomingRequestBodyGraph != null && !validationResult.getValid()) {
                    throw new ShapeTreeException(400, "Payload did not meet requirements defined by ShapeTree " + targetShapeTreeStep.getURI());
                }

                // Determine if we are trying to created a container
                if (isContainer) {
                    // At this point we're trying to create a container inside a managed container
                    // This means we're going to effectively plant that targetShapeTreeStep (which comes from the matching URI Template)
                    // TODO -- if there is no matching URI template, do we just let the POST happen as-is??  Can someone just create a container
                    // TODO -- within a managed container that perhaps doesn't list any contents?
                    // TODO -- inquiring minds want to know
                    ShapeTreePlantResult result = plantShapeTree(this.authorizationHeaderValue, this.requestRemoteResource, containerShapeTreeRootStep, targetShapeTreeStep, requestedName, ".", 0);
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
}
