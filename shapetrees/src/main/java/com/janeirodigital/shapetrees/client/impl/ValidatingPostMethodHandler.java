package com.janeirodigital.shapetrees.client.impl;

import com.janeirodigital.shapetrees.*;
import com.janeirodigital.shapetrees.client.AbstractValidatingMethodHandler;
import com.janeirodigital.shapetrees.client.ValidatingMethodHandler;
import com.janeirodigital.shapetrees.enums.HttpHeaders;
import com.janeirodigital.shapetrees.enums.LinkRelations;
import com.janeirodigital.shapetrees.helper.PlantHelper;
import com.janeirodigital.shapetrees.model.*;
import com.janeirodigital.shapetrees.vocabulary.ShapeTreeVocabulary;
import lombok.extern.slf4j.Slf4j;
import okhttp3.Interceptor;
import okhttp3.Response;
import org.apache.jena.graph.Graph;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.*;

@Slf4j
public class ValidatingPostMethodHandler extends AbstractValidatingMethodHandler implements ValidatingMethodHandler {

    public ValidatingPostMethodHandler(Interceptor.Chain chain, ShapeTreeEcosystem ecosystem) throws IOException {
        super(chain, ecosystem);
    }

    @Override
    public Response process() throws IOException, URISyntaxException {

        log.debug("Evaluating POST request for shape tree validation");

        ensureRequestResourceExists("Parent Container not found");

        String requestedName = getIncomingHeaderValueWithDefault(HttpHeaders.SLUG.getValue(), UUID.randomUUID().toString());
        List<String> incomingRequestShapeTreeUris = getIncomingLinkHeaderByRelationValue(LinkRelations.SHAPETREE.getValue());
        Boolean isContainer = getIsContainerFromIncomingLinkHeaders();
        URI normalizedBaseURI = normalizeBaseURI(this.requestRemoteResource.getURI(), requestedName, isContainer);
        Graph incomingRequestBodyGraph = getIncomingBodyGraph(normalizedBaseURI);

        if (incomingRequestShapeTreeUris != null && incomingRequestShapeTreeUris.size() > 0) {
           // This means we're Planting a new Shape Tree

            // If we are performing a plant there are few levels of validation we need to perform:
            // 1.  Ensure that between existing and new ShapeTrees the configuration is not invalid
            // 2.  The body of the incoming request against the ShapeTree, if it has a shapeURI present
            // 3.  The ShapeTree of the parent container looking at the appropriate contents node

            // Determine the ShapeTrees that are being requested to be planted
            List<ShapeTree> shapeTreesToPlant = new ArrayList<>();
            ShapeTree shapeTree;
            try {
                for (String shapeTreeUri : incomingRequestShapeTreeUris) {
                    shapeTree = ShapeTreeFactory.getShapeTree(new URI(shapeTreeUri));
                    shapeTreesToPlant.add(shapeTree);
                }
            } catch (URISyntaxException e) {
                throw new ShapeTreeException(400, "Value of 'ShapeTree' link header is not a value URI");
            }

            // 1.  Validate the potentially multiple ShapeTrees to ensure they don't conflict
            //     this will also retrieve any already planted ShapeTrees and ensure both existing and are valid
            validateShapeTrees(this.requestRemoteResource.getURI(), requestedName, shapeTreesToPlant);

            // Determine if the ecosystem already has a planted ShapeTree we can reuse
            ShapeTreePlantResult existingPlantedShapeTree = this.ecosystem.getExistingShapeTreeFromContainer(this.shapeTreeContext, this.requestRemoteResource.getURI(), shapeTreesToPlant, requestedName);
            if (existingPlantedShapeTree != null && existingPlantedShapeTree.getRootContainer() != null) {
                // If an existing ShapeTree exists, nothing to do here
                return createPlantResponse(Collections.singletonList(existingPlantedShapeTree), this.request, this.incomingRequestLinkHeaders);
            }

            // Before doing validation, ensure the ecosystem has the ability to transmogrify the body graph
            Graph ecosystemUpdatedBodyGraph = ecosystem.beforePlantShapeTree(this.shapeTreeContext, normalizedBaseURI, incomingRequestBodyGraph, shapeTreesToPlant, this.incomingRequestLinkHeaders);

            // 2. Validate the request body using the appropriate ShapeTree
            validateRequestBody(ecosystemUpdatedBodyGraph, normalizedBaseURI, shapeTreesToPlant);

            // 3. Validate the request against the parent container which may already be a managed container
            validateAgainstParentContainer(ecosystemUpdatedBodyGraph, normalizedBaseURI, this.requestRemoteResource, requestedName, true);

            // At this point all validations have been passed and the ShapeTree can be planted
            List<ShapeTreePlantResult> plantResults = new ArrayList<>();
            for (ShapeTree shapeTreeToPlant : shapeTreesToPlant) {
                ShapeTreePlantResult plantResult = PlantHelper.plantShapeTree(this.shapeTreeContext.getAuthorizationHeaderValue(), this.requestRemoteResource, ecosystemUpdatedBodyGraph, shapeTreeToPlant, null, shapeTreeToPlant, requestedName);
                plantResults.add(plantResult);
                // Provide to the ecosystem to index
                this.ecosystem.indexShapeTree(this.shapeTreeContext, requestRemoteResource.getURI(), shapeTreeToPlant.getURI(), plantResult.getRootContainer(), this.incomingRequestLinkHeaders);
            }
            // Create and return a response
            return createPlantResponse(plantResults, this.request, this.incomingRequestLinkHeaders);
        } else {
            ValidationContext validationContext = validateAgainstParentContainer(incomingRequestBodyGraph, normalizedBaseURI, this.requestRemoteResource, requestedName, isContainer);
            // Two reasons for passing through the request (and not performing validation):
            // 1. Validation returns no locators, meaning the parent container is not managed
            // 2. We're creating a resource and it has already passed validation
            if (validationContext == null || validationContext.getParentContainerLocators() == null || validationContext.getValidatingShapeTree() == null || !isContainer) {
                Response response = this.chain.proceed(this.chain.request());
                // If there is a ShapeTree managing the new resource, register it
                if (validationContext != null && validationContext.getValidatingShapeTree() != null) {
                    String locationUrl = response.header(HttpHeaders.LOCATION.getValue());
                    if (locationUrl != null) {
                        this.ecosystem.indexShapeTreeDataInstance(this.shapeTreeContext, requestRemoteResource.getURI(), validationContext.getValidatingShapeTree().getURI(), new URI(locationUrl));
                    } else {
                        log.warn("POST of validated content did not yield a location response header");
                    }
                }
                return response;
            }

            // We're creating a container, have already passed validation and will now call Plant as it may
            // lead to nested static content to be created.  We will iterate the shapeTreeLocatorMetadatas
            // which describe the ShapeTrees present on the container.
            List<ShapeTreePlantResult> results = new ArrayList<>();
            for (ShapeTreeLocator locator : validationContext.getParentContainerLocators()) {

                if (requestedName.endsWith("/")) {
                    requestedName = requestedName.replace("/","");
                }
                ShapeTreePlantResult result = PlantHelper.plantShapeTree(this.shapeTreeContext.getAuthorizationHeaderValue(), this.requestRemoteResource, this.incomingRequestBody, locator, validationContext.getValidatingShapeTree(), requestedName);
                results.add(result);
            }

            return createPlantResponse(results, this.request, this.incomingRequestLinkHeaders);
        }
    }

    private void validateRequestBody(Graph graphToValidate, URI baseURI, List<ShapeTree> shapeTreesToPlant) throws IOException, URISyntaxException {
        ShapeTree validatingShapeTree = getShapeTreeWithShapeURI(shapeTreesToPlant);

        ValidationResult validationResult = null;
        // If there is a graph to validate...and a ShapeTree indicates it wants to validate the container body
        if (graphToValidate != null && validatingShapeTree != null && validatingShapeTree.getValidatedByShapeUri() != null) {
            // ...and a focus node was provided via the focusNode header, then we perform our validation
            URI focusNodeURI = getIncomingResolvedFocusNode(baseURI);
            validationResult = validatingShapeTree.validateContent(graphToValidate, focusNodeURI, true);
        }

        // If there is a body graph and it did not pass validation, return an error
        if (graphToValidate != null && validationResult != null && !validationResult.getValid()) {
            throw new ShapeTreeException(422, "Payload did not meet requirements defined by ShapeTree " + validatingShapeTree.getURI());
        }
    }

    private void validateShapeTrees(URI requestURI, String requestedName, List<ShapeTree> shapeTreesToPlant) throws IOException, URISyntaxException {

        // Determine if the target container exists, if so, retrieve any existing ShapeTrees to validate alongside the newly requested ones
        RemoteResource targetContainer = new RemoteResource(requestURI + requestedName, this.shapeTreeContext.getAuthorizationHeaderValue());
        if (targetContainer.exists()) {
            RemoteResource targetMetadata = targetContainer.getMetadataResource(this.shapeTreeContext.getAuthorizationHeaderValue());
            if (targetMetadata.exists()) {
                List<ShapeTreeLocator> locators = ShapeTreeLocator.getShapeTreeLocatorsFromGraph(targetMetadata.getGraph(new URI(requestURI.toString() + requestedName)));
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
                }
                else {
                    throw new ShapeTreeException(400, "Only one ShapeTree provided can specify a ShapeURI");
                }
            }

            if (shapeTree.getContains() != null && shapeTree.getContains().size() > 0) {
                if (foundContents == null) {
                    foundContents = shapeTree.getContains();
                } else {
                    throw new ShapeTreeException(400, "Only one ShapeTree provided can specify Contents");
                }
            }
        }
    }
}
