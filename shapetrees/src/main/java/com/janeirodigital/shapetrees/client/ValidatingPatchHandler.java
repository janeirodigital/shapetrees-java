package com.janeirodigital.shapetrees.client;

import com.janeirodigital.shapetrees.RemoteResource;
import com.janeirodigital.shapetrees.ShapeTreeEcosystem;
import com.janeirodigital.shapetrees.ShapeTreeException;
import com.janeirodigital.shapetrees.model.ShapeTreeStep;
import com.janeirodigital.shapetrees.model.ValidationResult;
import lombok.extern.slf4j.Slf4j;
import okhttp3.Interceptor;
import okhttp3.Response;
import org.apache.jena.graph.Graph;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.update.UpdateAction;
import org.apache.jena.update.UpdateFactory;
import org.apache.jena.update.UpdateRequest;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;

@Slf4j
public class ValidatingPatchHandler extends AbstractValidatingHandler implements ValidatingHandler {

    public ValidatingPatchHandler(Interceptor.Chain chain, ShapeTreeEcosystem ecosystem) throws IOException {
        super(chain, ecosystem);
    }

    @Override
    public Response process() throws  IOException, URISyntaxException {
        ensureRequestResourceExists("Resource being PATCHed does not exist");
        if (this.incomingRequestContentType == null || !this.incomingRequestContentType.toLowerCase().equals("application/sparql-update")) {
            log.error("Received a patch without a content type of application/sparql-update");
            throw new ShapeTreeException(400, "PATCH verb expects a content type of application/sparql-update");
        }
        // Get the parent container URI
        URI parentURI = getParentContainerURI();
        // Get requested name (resource being PATCHed)
        String requestedName = getRequestResourceName();
        // Dereference parent container
        RemoteResource parentContainer = new RemoteResource(parentURI, authorizationHeaderValue);
        // Get URI of metadata resource for parent container
        String parentContainerMetaDataURIString = getMetadataResourceURI(parentContainer);
        // Dereference parent container metadata resource
        RemoteResource parentContainerMetadataResource = new RemoteResource(parentContainerMetaDataURIString, authorizationHeaderValue);
        // Retrieve graph of parent container metadata resource
        Graph parentContainerMetadataGraph = parentContainerMetadataResource.getGraph(parentURI);
        // Get the shape tree step that manages that container
        boolean shapeTreeManagedContainer = parentContainerMetadataGraph != null && parentContainerMetadataGraph.contains(null, NodeFactory.createURI(SHAPE_TREE_STEP_PREDICATE), null);
        // If managed, do validation
        if (shapeTreeManagedContainer) {
            // This is the ShapeTree step that managed the container we're posting to
            ShapeTreeStep containerShapeTreeStep = getShapeTreeStepFromGraphByPredicate(parentContainerMetadataGraph, SHAPE_TREE_STEP_PREDICATE);
            /* This is the ShapeTree that the container being created must adhere to
               it is identified by traversing the ShapeTree steps contained within containerShapeTreeStep
               and finding the one whose uriTemplate matches the Slug of the container we're about to create
             */
            ShapeTreeStep targetShapeTreeStep = containerShapeTreeStep.findMatchingContainsShapeTreeStep(requestedName);

            // Get existing resource graph (prior to PATCH)
            Graph existingResourceGraph = requestRemoteResource.getGraph(requestRemoteResource.getURI());

            // Perform a SPARQL update locally to ensure that resulting graph validates against ShapeTree
            UpdateRequest updateRequest = UpdateFactory.create(this.incomingRequestBody);
            UpdateAction.execute(updateRequest, existingResourceGraph);
            ValidationResult validationResult;
            if (this.incomingRequestLinkHeaders != null && this.incomingRequestLinkHeaders.get(FOCUS_NODE) != null) {
                String focusNode = this.incomingRequestLinkHeaders.get(FOCUS_NODE).get(0);
                URI focusNodeURI = this.requestRemoteResource.getURI().resolve(focusNode);
                validationResult = targetShapeTreeStep.validateContent(this.authorizationHeaderValue, existingResourceGraph, focusNodeURI, this.requestRemoteResource.isContainer());
            } else {
                // ...but no focus node, we return an error
                throw new ShapeTreeException(400, "No Link header with relation " + FOCUS_NODE + " supplied, unable to perform Shape validation");
            }

            if (validationResult.getValid()) {
                // If the result of the locally applied PATCH validates, then pass it to the server
                return chain.proceed(chain.request());
            } else {
                // Otherwise, return a validation error
                throw new ShapeTreeException(400, "Payload did not meet requirements defined by ShapeTree " + targetShapeTreeStep.getURI());
            }
        } else {
            // If the parent container is managed, then pass through the PATCH
            return chain.proceed(chain.request());
        }
    }
}
