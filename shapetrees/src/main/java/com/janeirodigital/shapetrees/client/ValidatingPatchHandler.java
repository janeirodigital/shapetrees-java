package com.janeirodigital.shapetrees.client;

import com.janeirodigital.shapetrees.*;
import com.janeirodigital.shapetrees.model.ShapeTreeLocator;
import com.janeirodigital.shapetrees.model.ShapeTree;
import com.janeirodigital.shapetrees.model.ValidationResult;
import com.janeirodigital.shapetrees.vocabulary.ShapeTreeVocabulary;
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
import java.util.ArrayList;
import java.util.List;

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
            throw new ShapeTreeException(415, "PATCH verb expects a content type of application/sparql-update");
        }

        // Get the parent container URI
        URI parentURI = getParentContainerURI();
        // Get requested name (resource being PATCHed)
        String requestedName = getRequestResourceName();
        // Dereference parent container
        RemoteResource parentContainer = new RemoteResource(parentURI, authorizationHeaderValue);
        // Dereference parent container metadata resource
        RemoteResource parentContainerMetadataResource = parentContainer.getMetadataResource(authorizationHeaderValue);
        // Retrieve graph of parent container metadata resource
        Graph parentContainerMetadataGraph = parentContainerMetadataResource.getGraph(parentURI);

        URI normalizedBaseURI = normalizeBaseURI(this.requestRemoteResource.getURI(), null, this.requestRemoteResource.isContainer());
        // Get the shape tree that manages that container
        boolean shapeTreeManagedContainer = parentContainerMetadataGraph != null && parentContainerMetadataGraph.contains(null, NodeFactory.createURI(ShapeTreeVocabulary.HAS_SHAPE_TREE_LOCATOR), null);
        // If managed, do validation
        if (shapeTreeManagedContainer) {

            List<ShapeTreeLocator> shapeTreeLocatorMetadatas = getShapeTreeLocators(parentContainerMetadataGraph);

            List<ShapeTree> shapeTrees = new ArrayList<>();
            for (ShapeTreeLocator locator : shapeTreeLocatorMetadatas) {
                shapeTrees.add(ShapeTreeFactory.getShapeTree(new URI(locator.getShapeTree())));
            }

            ShapeTree validatingShapeTree = getShapeTreeWithContents(shapeTrees);

            /* This is the ShapeTree that the container being created must adhere to
               it is identified by traversing the ShapeTrees contained within containerShapeTree
               and finding the one:
                - whose uriTemplate matches the Slug of the container we're about to create
                - whose URI matches the target shape tree hint provided via Link header
             */
            URI targetShapeTreeHint = getIncomingTargetShapeTreeHint();
            ShapeTree targetShapeTree = validatingShapeTree.findMatchingContainsShapeTree(requestedName, targetShapeTreeHint, requestRemoteResource.isContainer(), this.isNonRdfSource);

            ValidationResult validationResult = null;
            if (targetShapeTree != null) {
                // Get existing resource graph (prior to PATCH)
                Graph existingResourceGraph = requestRemoteResource.getGraph(normalizedBaseURI);

                // Perform a SPARQL update locally to ensure that resulting graph validates against ShapeTree
                UpdateRequest updateRequest = UpdateFactory.create(this.incomingRequestBody);
                UpdateAction.execute(updateRequest, existingResourceGraph);

                if (existingResourceGraph == null) {
                    throw new ShapeTreeException(400, "No graph after update");
                }

                URI focusNodeURI = getIncomingResolvedFocusNode(normalizedBaseURI);
                validationResult = targetShapeTree.validateContent(this.authorizationHeaderValue, existingResourceGraph, focusNodeURI, this.requestRemoteResource.isContainer());
            }

            if (targetShapeTree == null || validationResult.getValid()) {
                // If there was no targetShapeTree returned to indicate validation should occur, then pass it to the server
                // If the result of the locally applied PATCH validates, then pass it to the server
                return chain.proceed(chain.request());
            } else {
                // Otherwise, return a validation error
                throw new ShapeTreeException(422, "Payload did not meet requirements defined by ShapeTree " + targetShapeTree.getURI());
            }
        } else {
            // If the parent container is managed, then pass through the PATCH
            return chain.proceed(chain.request());
        }
    }

}
