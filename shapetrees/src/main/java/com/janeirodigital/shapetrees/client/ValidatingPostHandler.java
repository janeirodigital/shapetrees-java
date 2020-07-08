package com.janeirodigital.shapetrees.client;

import com.janeirodigital.shapetrees.*;
import com.janeirodigital.shapetrees.enums.HttpHeaders;
import com.janeirodigital.shapetrees.model.ShapeTree;
import com.janeirodigital.shapetrees.model.ShapeTreeLocator;
import com.janeirodigital.shapetrees.model.ShapeTreePlantResult;
import com.janeirodigital.shapetrees.model.ValidationResult;
import lombok.extern.slf4j.Slf4j;
import okhttp3.Interceptor;
import okhttp3.Response;
import org.apache.commons.lang3.StringUtils;
import org.apache.jena.graph.Graph;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

@Slf4j
public class ValidatingPostHandler extends AbstractValidatingHandler implements ValidatingHandler {

    public ValidatingPostHandler(Interceptor.Chain chain, ShapeTreeEcosystem ecosystem) throws IOException {
        super(chain, ecosystem);
    }

    @Override
    public Response process() throws IOException, URISyntaxException {
        ensureRequestResourceExists("Parent Container not found");

        String requestedName = getIncomingHeaderValueWithDefault(HttpHeaders.SLUG.getValue(), "Container");
        List<String> incomingRequestShapeTreeUris = getIncomingLinkHeaderByRelationValue(REL_SHAPE_TREE);
        Boolean isContainer = this.incomingRequestLinkHeaders.get(REL_TYPE).contains(LDP_CONTAINER);
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
            ShapeTreePlantResult existingPlantedShapeTree = this.ecosystem.getExistingShapeTreeFromContainer(getShapeTreeContext(), this.requestRemoteResource.getURI(), shapeTreesToPlant, requestedName);
            if (existingPlantedShapeTree != null && existingPlantedShapeTree.getRootContainer() != null) {
                // If an existing ShapeTree exists, nothing to do here
                return createPlantResponse(Collections.singletonList(existingPlantedShapeTree), this.request, this.incomingRequestLinkHeaders);
            }

            // Before doing validation, ensure the ecosystem has the ability to transmogrify the body graph
            Graph ecosystemUpdatedBodyGraph = ecosystem.beforePlantShapeTree(this.getShapeTreeContext(), normalizedBaseURI, incomingRequestBodyGraph, shapeTreesToPlant, this.incomingRequestLinkHeaders);

            // 2. Validate the request body using the appropriate ShapeTree
            validateRequestBody(ecosystemUpdatedBodyGraph, normalizedBaseURI, shapeTreesToPlant);

            // 3. Validate the request against the parent container which may already be a managed container
            validateAgainstParentContainer(ecosystemUpdatedBodyGraph, normalizedBaseURI, this.requestRemoteResource, requestedName, true);

            // At this point all validations have been passed and the ShapeTree can be planted
            List<ShapeTreePlantResult> plantResults = new ArrayList<>();
            for (ShapeTree shapeTreeToPlant : shapeTreesToPlant) {
                ShapeTreePlantResult plantResult = plantShapeTree(this.authorizationHeaderValue, this.requestRemoteResource, ecosystemUpdatedBodyGraph, shapeTreeToPlant, shapeTreeToPlant, requestedName, ".", 0);
                plantResults.add(plantResult);
                // Provide to the ecosystem to index
                this.ecosystem.indexShapeTree(this.getShapeTreeContext(), requestRemoteResource.getURI(), shapeTreeToPlant.getURI(), plantResult.getRootContainer(), this.incomingRequestLinkHeaders);
            }
            // Create and return a response
            return createPlantResponse(plantResults, this.request, this.incomingRequestLinkHeaders);
        } else {
            List<ShapeTreeLocator> shapeTreeLocatorMetadatas = validateAgainstParentContainer(incomingRequestBodyGraph, normalizedBaseURI, this.requestRemoteResource, requestedName, isContainer);
            if (shapeTreeLocatorMetadatas == null) {
                // If validation returns no locators, that means the parent container is not managed and the request should be passed through
                return this.chain.proceed(this.chain.request());
            }
            if (!isContainer) {
                // We're creating a resource and it has already passed validation, pass through
                return this.chain.proceed(this.chain.request());
            }

            // We're creating a container, have already passed validation and will now call Plant as it may
            // lead to nested static content to be created
            List<ShapeTreePlantResult> results = new ArrayList<>();
            for (ShapeTreeLocator locator : shapeTreeLocatorMetadatas) {

                ShapeTree rootShapeTree = ShapeTreeFactory.getShapeTree(new URI(locator.getRootShapeTree()));
                ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(new URI(locator.getShapeTree()));

                // Determine the depth based on container and the relative depth
                String containerPath = locator.getShapeTreeInstancePath();
                String instanceRoot = locator.getShapeTreeRoot();
                String pathFromRoot = requestRemoteResource.getURI().toString().replace(instanceRoot, "");
                // In this case the URI is going to end in a slash for the contain that is being requested to create
                // because of this extra slash, we just count the slashes instead of adding one as seen in the ValidatingPostHandler
                int relativeDepth = StringUtils.countMatches(pathFromRoot, "/") + 1;

                if (requestedName.endsWith("/")) {
                    requestedName = requestedName.replace("/","");
                }
                ShapeTreePlantResult result = plantShapeTree(this.authorizationHeaderValue, this.requestRemoteResource, this.incomingRequestBody, rootShapeTree, shapeTree, requestedName, containerPath + requestedName + "/", relativeDepth);
                results.add(result);
            }

            return createPlantResponse(results, this.request, this.incomingRequestLinkHeaders);
        }
    }

    private void validateRequestBody(Graph graphToValidate, URI baseURI, List<ShapeTree> shapeTreesToPlant) throws IOException, URISyntaxException {
        ShapeTree validatingShapeTree = getShapeTreeWithShapeURI(shapeTreesToPlant);

        ValidationResult validationResult = null;
        // If there is a graph to validate...and a ShapeTree indicates it wants to validate the container body
        if (graphToValidate != null && validatingShapeTree != null && validatingShapeTree.getShapeUri() != null) {
            // ...and a focus node was provided via the focusNode header, then we perform our validation
            URI focusNodeURI = getIncomingResolvedFocusNode(baseURI, true);
            validationResult = validatingShapeTree.validateContent(this.authorizationHeaderValue, graphToValidate, focusNodeURI, true);
        }

        // If there is a body graph and it did not pass validation, return an error
        if (graphToValidate != null && validationResult != null && !validationResult.getValid()) {
            throw new ShapeTreeException(422, "Payload did not meet requirements defined by ShapeTree " + validatingShapeTree.getURI());
        }
    }

    private void validateShapeTrees(URI requestURI, String requestedName, List<ShapeTree> shapeTreesToPlant) throws IOException, URISyntaxException {

        // Determine if the target container exists, if so, retrieve any existing ShapeTrees to validate alongside the newly requested ones
        RemoteResource targetContainer = new RemoteResource(requestURI + requestedName, authorizationHeaderValue);
        if (targetContainer.exists()) {
            String existingMetaDataURIString = getMetadataResourceURI(targetContainer);
            RemoteResource targetMetadata = new RemoteResource(existingMetaDataURIString, authorizationHeaderValue);
            if (targetMetadata.exists()) {
                List<ShapeTreeLocator> locators = getShapeTreeLocators(targetMetadata.getGraph(new URI(requestURI.toString() + requestedName)));
                for (ShapeTreeLocator locator : locators) {
                    ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(new URI(locator.getShapeTree()));
                    log.debug("Found ShapeTree [{}] already planted in existing container, adding to list to validate", shapeTree.getURI());
                    shapeTreesToPlant.add(shapeTree);
                }
            }
        }

        String foundShapeURI = null;
        List<URI> foundContents = null;

        for (ShapeTree shapeTree : shapeTreesToPlant) {
            if (shapeTree.getRdfResourceType().contains("Resource")) {
                throw new ShapeTreeException(400, "The root of any ShapeTree hierarchy must be of type Container");
            }
            if (shapeTree.getShapeUri() != null) {
                if (foundShapeURI == null) {
                    foundShapeURI = shapeTree.getShapeUri();
                }
                else {
                    throw new ShapeTreeException(400, "Only one ShapeTree provided can specify a ShapeURI");
                }
            }

            if (shapeTree.getContents() != null && shapeTree.getContents().size() > 0) {
                if (foundContents == null) {
                    foundContents = shapeTree.getContents();
                } else {
                    throw new ShapeTreeException(400, "Only one ShapeTree provided can specify Contents");
                }
            }
        }
    }
}
