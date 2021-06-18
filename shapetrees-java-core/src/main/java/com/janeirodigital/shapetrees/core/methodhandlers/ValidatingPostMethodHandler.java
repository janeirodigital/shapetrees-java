package com.janeirodigital.shapetrees.core.methodhandlers;

import com.janeirodigital.shapetrees.core.*;
import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.core.enums.LinkRelations;
import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.*;
import com.janeirodigital.shapetrees.core.vocabularies.ShapeTreeVocabulary;
import lombok.extern.slf4j.Slf4j;
import org.apache.jena.graph.Graph;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.*;

@Slf4j
public class ValidatingPostMethodHandler extends AbstractValidatingMethodHandler implements ValidatingMethodHandler {

    public ValidatingPostMethodHandler(ResourceAccessor resourceAccessor) {
        super(resourceAccessor);
    }

    @Override
    public ShapeTreeValidationResponse validateRequest(ShapeTreeRequest<?> shapeTreeRequest) {
        try {
            ShapeTreeContext shapeTreeContext = buildContextFromRequest(shapeTreeRequest);
            ShapeTreeResource existingResource = getRequestResource(shapeTreeContext, shapeTreeRequest);
            shapeTreeRequest.setResourceType(determineResourceType(shapeTreeRequest, existingResource));

            ensureRequestResourceExists(existingResource,"Parent Container not found");

            String requestedName = getIncomingHeaderValueWithDefault(shapeTreeRequest, HttpHeaders.SLUG.getValue(), UUID.randomUUID().toString());
            List<String> incomingRequestShapeTreeUris = getIncomingLinkHeaderByRelationValue(shapeTreeRequest, LinkRelations.SHAPETREE.getValue());
            URI normalizedBaseURI = normalizeBaseURI(existingResource.getUri(), requestedName, shapeTreeRequest.getResourceType());
            Graph incomingRequestBodyGraph = getIncomingBodyGraph(shapeTreeRequest, normalizedBaseURI);

            if (incomingRequestShapeTreeUris != null && !incomingRequestShapeTreeUris.isEmpty()) {
                // TODO: Remove planting logic from here. Plant is done with PUT or PATCH on a shape tree locator
                // This means we're Planting a new Shape Tree

                // If we are performing a plant there are few levels of validation we need to perform:
                // 1.  Ensure that between existing and new ShapeTrees the configuration is not invalid
                // 2.  The body of the incoming request against the ShapeTree, if it has a shapeURI present
                // 3.  The ShapeTree of the parent container looking at the appropriate contents node

                // Determine the ShapeTrees that are being requested to be planted
                List<ShapeTree> shapeTreesToPlant = new ArrayList<>();
                ShapeTree shapeTree;

                for (String shapeTreeUri : incomingRequestShapeTreeUris) {
                    shapeTree = ShapeTreeFactory.getShapeTree(new URI(shapeTreeUri));
                    shapeTreesToPlant.add(shapeTree);
                }

                // 1.  Validate the potentially multiple ShapeTrees to ensure they don't conflict
                //     this will also retrieve any already planted ShapeTrees and ensure both existing and are valid
                validateShapeTrees(shapeTreeContext, existingResource.getUri(), requestedName, shapeTreesToPlant);


                // 2. Validate the request body using the appropriate ShapeTree
                validateRequestBody(shapeTreeRequest, incomingRequestBodyGraph, normalizedBaseURI, shapeTreesToPlant);

                // 3. Validate the request against the parent container which may already be a managed container
                validateAgainstParentContainer(shapeTreeContext, incomingRequestBodyGraph, normalizedBaseURI, existingResource, requestedName, shapeTreeRequest);

                // At this point all validations have been passed and the ShapeTree can be planted
                List<ShapeTreePlantResult> plantResults = new ArrayList<>();
                for (ShapeTree shapeTreeToPlant : shapeTreesToPlant) {
                    ShapeTreePlantResult plantResult = plantShapeTree(shapeTreeContext, existingResource, incomingRequestBodyGraph, shapeTreeToPlant, null, shapeTreeToPlant, requestedName);
                    plantResults.add(plantResult);
                }
                // Create and return a response
                return createPlantResponse(plantResults, shapeTreeRequest);
            } else {
                ValidationContext validationContext = validateAgainstParentContainer(shapeTreeContext, incomingRequestBodyGraph, normalizedBaseURI, existingResource, requestedName, shapeTreeRequest);
                // Two reasons for passing through the request (and not performing validation):
                // 1. Validation returns no locators, meaning the parent container is not managed
                // 2. We're creating a resource and it has already passed validation
                if (validationContext == null || validationContext.getParentContainerLocators() == null || validationContext.getValidatingShapeTree() == null || shapeTreeRequest.getResourceType() != ShapeTreeResourceType.CONTAINER) {
                    return ShapeTreeValidationResponse.passThroughResponse();
                }

                // We're creating a container, have already passed validation and will now call Plant as it may
                // lead to nested static content to be created.  We will iterate the shapeTreeLocator meta data
                // which describe the ShapeTrees present on the container.
                List<ShapeTreePlantResult> results = new ArrayList<>();
                for (ShapeTreeLocator locator : validationContext.getParentContainerLocators()) {

                    if (requestedName.endsWith("/")) {
                        requestedName = requestedName.replace("/", "");
                    }
                    ShapeTreePlantResult result = plantShapeTree(shapeTreeContext, existingResource, shapeTreeRequest.getBody(), shapeTreeRequest.getContentType(), locator, validationContext.getValidatingShapeTree(), requestedName);
                    results.add(result);
                }

                return createPlantResponse(results, shapeTreeRequest);
            }
        } catch (ShapeTreeException ste) {
            return new ShapeTreeValidationResponse(ste);
        } catch (URISyntaxException e) {
            return new ShapeTreeValidationResponse(new ShapeTreeException(400, "Value of 'ShapeTree' link header is not a value URI"));
        } catch (Exception ex) {
            return new ShapeTreeValidationResponse(new ShapeTreeException(500, ex.getMessage()));
        }

    }

    private void validateRequestBody(ShapeTreeRequest<?> shapeTreeRequest, Graph graphToValidate, URI baseURI, List<ShapeTree> shapeTreesToPlant) throws IOException, URISyntaxException {
        ShapeTree validatingShapeTree = getShapeTreeWithShapeURI(shapeTreesToPlant);

        ValidationResult validationResult = null;
        // If there is a graph to validate...and a ShapeTree indicates it wants to validate the container body
        if (graphToValidate != null && validatingShapeTree != null && validatingShapeTree.getValidatedByShapeUri() != null) {
            // ...and a focus node was provided via the focusNode header, then we perform our validation
            URI focusNodeURI = getIncomingResolvedFocusNode(shapeTreeRequest, baseURI);
            validationResult = validatingShapeTree.validateContent(graphToValidate, focusNodeURI, shapeTreeRequest.getResourceType());
        }

        // If there is a body graph and it did not pass validation, return an error
        if (graphToValidate != null && validationResult != null && Boolean.FALSE.equals(validationResult.getValid())) {
            throw new ShapeTreeException(422, "Payload did not meet requirements defined by ShapeTree " + validatingShapeTree.getURI());
        }
    }

    private void validateShapeTrees(ShapeTreeContext shapeTreeContext, URI requestURI, String requestedName, List<ShapeTree> shapeTreesToPlant) throws IOException, URISyntaxException {

        URI targetContainerURI = URI.create(requestURI.toString() + requestedName);

        // Determine if the target container exists, if so, retrieve any existing ShapeTrees to validate alongside the newly requested ones
        ShapeTreeResource targetContainerResource = this.resourceAccessor.getResource(shapeTreeContext, targetContainerURI);
        if (targetContainerResource.isExists()) {
            ShapeTreeResource targetContainerMetadataResource = getShapeTreeMetadataResourceForResource(shapeTreeContext, targetContainerResource);
            if (targetContainerMetadataResource.isExists()) {
                Graph targetContainerMetadataGraph = getGraphForResource(targetContainerMetadataResource, targetContainerURI);
                List<ShapeTreeLocator> locators = ShapeTreeLocator.getShapeTreeLocatorsFromGraph(targetContainerMetadataGraph);
                for (ShapeTreeLocator locator : locators) {
                    ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(new URI(locator.getShapeTree()));
                    if (shapeTree != null) {
                        log.debug("Found ShapeTree [{}] already planted in existing container, adding to list to validate", shapeTree.getURI());
                        shapeTreesToPlant.add(shapeTree);
                    } else {
                        throw new ShapeTreeException(500, "Existing container is managed by a shape tree " + locator.getShapeTree() + " which cannot be found");
                    }
                }
            }
        }

        String foundShapeURI = null;
        List<URI> foundContents = null;

        for (ShapeTree shapeTree : shapeTreesToPlant) {
            if (!shapeTree.getExpectedResourceType().equals(ShapeTreeVocabulary.SHAPETREE_CONTAINER)) {
                throw new ShapeTreeException(400, "The root of any ShapeTree hierarchy must be of type Container");
            }
            if (shapeTree.getValidatedByShapeUri() != null) {
                if (foundShapeURI == null) {
                    foundShapeURI = shapeTree.getValidatedByShapeUri();
                } else {
                    throw new ShapeTreeException(400, "Only one ShapeTree provided can specify a ShapeURI");
                }
            }

            if (shapeTree.getContains() != null && !shapeTree.getContains().isEmpty()) {
                if (foundContents == null) {
                    foundContents = shapeTree.getContains();
                } else {
                    throw new ShapeTreeException(400, "Only one ShapeTree provided can specify Contents");
                }
            }
        }
    }

    private void ensureRequestResourceExists(ShapeTreeResource shapeTreeResource, String message) throws ShapeTreeException {
        if (!shapeTreeResource.isExists()) {
            throw new ShapeTreeException(404, message);
        }
    }

    private String getIncomingHeaderValueWithDefault(ShapeTreeRequest<?> shapeTreeRequest, String headerName, String defaultValue) {
        if (shapeTreeRequest.getHeaders().containsKey(headerName)) {
            return shapeTreeRequest.getHeaders().get(headerName).stream().findFirst().orElse(defaultValue);
        } else {
            return defaultValue;
        }
    }

    protected List<String> getIncomingLinkHeaderByRelationValue(ShapeTreeRequest<?> shapeTreeRequest, String relation) {
        if (shapeTreeRequest.getLinkHeaders().containsKey(relation)) {
            return shapeTreeRequest.getLinkHeaders().get(relation);
        }
        return Collections.emptyList();
    }
}
