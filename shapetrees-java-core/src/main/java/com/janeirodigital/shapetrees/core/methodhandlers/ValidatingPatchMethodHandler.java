package com.janeirodigital.shapetrees.core.methodhandlers;

import com.janeirodigital.shapetrees.core.*;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.*;
import com.janeirodigital.shapetrees.core.vocabularies.ShapeTreeVocabulary;
import lombok.extern.slf4j.Slf4j;
import org.apache.jena.graph.Graph;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.update.UpdateAction;
import org.apache.jena.update.UpdateFactory;
import org.apache.jena.update.UpdateRequest;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

@Slf4j
public class ValidatingPatchMethodHandler extends AbstractValidatingMethodHandler implements ValidatingMethodHandler {

    public ValidatingPatchMethodHandler(ResourceAccessor resourceAccessor) {
        super(resourceAccessor);
    }

    @Override
    public ShapeTreeValidationResponse validateRequest(ShapeTreeRequest<?> shapeTreeRequest) {
        try {
            if (shapeTreeRequest.getContentType() == null || !shapeTreeRequest.getContentType().equalsIgnoreCase("application/sparql-update")) {
                log.error("Received a patch without a content type of application/sparql-update");
                throw new ShapeTreeException(415, "PATCH verb expects a content type of application/sparql-update");
            }

            // TODO: Catch if this is a plant operation on a shape tree locator and handle that

            ShapeTreeContext shapeTreeContext = buildContextFromRequest(shapeTreeRequest);
            ShapeTreeResource existingResource = getRequestResource(shapeTreeContext, shapeTreeRequest);
            shapeTreeRequest.setResourceType(determineResourceType(shapeTreeRequest, existingResource));

            // Get the parent container URI
            URI parentURI = getParentContainerURI(existingResource);
            // Get requested name (resource being PATCHed)
            String requestedName = getRequestResourceName(existingResource);
            // Dereference parent container
            ShapeTreeResource parentContainerResource = this.resourceAccessor.getResource(shapeTreeContext, parentURI);
            ShapeTreeResource parentContainerMetadataResource = getShapeTreeMetadataResourceForResource(shapeTreeContext, parentContainerResource);
            // Retrieve graph of parent container metadata resource
            Graph parentContainerMetadataGraph = getGraphForResource(parentContainerMetadataResource, parentURI);

            URI normalizedBaseURI = normalizeBaseURI(existingResource.getUri(), null, shapeTreeRequest.getResourceType());
            // Get the shape tree that manages that container
            boolean shapeTreeManagedContainer = parentContainerMetadataGraph != null && parentContainerMetadataGraph.contains(null, NodeFactory.createURI(ShapeTreeVocabulary.HAS_SHAPE_TREE_LOCATOR), null);
            // If managed, do validation
            if (shapeTreeManagedContainer) {

                ShapeTreeLocator locator = ShapeTreeLocator.getShapeTreeLocatorFromGraph(parentContainerMetadataGraph);

                List<ShapeTree> shapeTrees = new ArrayList<>();
                for (ShapeTreeLocation location : locator.getLocations()) {
                    shapeTrees.add(ShapeTreeFactory.getShapeTree(new URI(location.getShapeTree())));
                }

                ShapeTree validatingShapeTree = getShapeTreeWithContents(shapeTrees);

            /* This is the ShapeTree that the container being created must adhere to
               it is identified by traversing the ShapeTrees contained within containerShapeTree
               and finding the one:
                - whose uriTemplate matches the Slug of the container we're about to create
                - whose URI matches the target shape tree hint provided via Link header
             */
                URI targetShapeTreeHint = getIncomingTargetShapeTreeHint(shapeTreeRequest);
                ShapeTree targetShapeTree = validatingShapeTree.findMatchingContainsShapeTree(requestedName, targetShapeTreeHint, shapeTreeRequest.getResourceType());

                ValidationResult validationResult = null;
                if (targetShapeTree != null) {
                    // Get existing resource graph (prior to PATCH)
                    Graph existingResourceGraph = getGraphForResource(existingResource, normalizedBaseURI);
                    if (existingResourceGraph == null) {
                        log.debug("Existing graph to patch does not exist.  Creating an empty graph.");
                        existingResourceGraph = ModelFactory.createDefaultModel().getGraph();
                    }

                    // Perform a SPARQL update locally to ensure that resulting graph validates against ShapeTree
                    UpdateRequest updateRequest = UpdateFactory.create(shapeTreeRequest.getBody(), normalizedBaseURI.toString());
                    UpdateAction.execute(updateRequest, existingResourceGraph);

                    if (existingResourceGraph == null) {
                        throw new ShapeTreeException(400, "No graph after update");
                    }

                    URI focusNodeURI = getIncomingResolvedFocusNode(shapeTreeRequest, normalizedBaseURI);
                    validationResult = targetShapeTree.validateContent(existingResourceGraph, focusNodeURI, shapeTreeRequest.getResourceType());
                }

                if (targetShapeTree == null || validationResult.getValid()) {
                    return ShapeTreeValidationResponse.passThroughResponse();
                } else {
                    // Otherwise, return a validation error
                    throw new ShapeTreeException(422, "Payload did not meet requirements defined by ShapeTree " + targetShapeTree.getURI());
                }
            } else {
                // If the parent container is not managed, then pass through the PATCH
                return ShapeTreeValidationResponse.passThroughResponse();
            }
        } catch (ShapeTreeException ste) {
            return new ShapeTreeValidationResponse(ste);
        } catch (Exception ex) {
            return new ShapeTreeValidationResponse(new ShapeTreeException(500, ex.getMessage()));
        }

    }
}
