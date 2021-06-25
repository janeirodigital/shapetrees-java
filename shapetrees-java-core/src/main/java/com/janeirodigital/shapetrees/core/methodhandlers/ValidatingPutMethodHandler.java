package com.janeirodigital.shapetrees.core.methodhandlers;

import com.janeirodigital.shapetrees.core.ResourceAccessor;
import com.janeirodigital.shapetrees.core.ShapeTreeRequest;
import com.janeirodigital.shapetrees.core.ShapeTreeResource;
import com.janeirodigital.shapetrees.core.ShapeTreeValidationResponse;
import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;
import com.janeirodigital.shapetrees.core.models.ShapeTreeLocator;
import com.janeirodigital.shapetrees.core.models.ShapeTreePlantResult;
import com.janeirodigital.shapetrees.core.models.ValidationContext;
import org.apache.jena.graph.Graph;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

public class ValidatingPutMethodHandler extends AbstractValidatingMethodHandler implements ValidatingMethodHandler {

    public ValidatingPutMethodHandler(ResourceAccessor resourceAccessor) {
        super(resourceAccessor);
    }

    @Override
    public ShapeTreeValidationResponse validateRequest(ShapeTreeRequest<?> shapeTreeRequest) {
        try {

            // TODO: Catch if this is a plant operation on a shape tree locator and handle that
            ShapeTreeContext shapeTreeContext = buildContextFromRequest(shapeTreeRequest);
            ShapeTreeResource existingResource = getRequestResource(shapeTreeContext, shapeTreeRequest);
            shapeTreeRequest.setResourceType(determineResourceType(shapeTreeRequest, existingResource));

            URI parentURI = getParentContainerURI(existingResource);

            URI normalizedBaseURI = normalizeBaseURI(existingResource.getUri(), null, shapeTreeRequest.getResourceType());
            Graph incomingRequestBodyGraph = getIncomingBodyGraph(shapeTreeRequest, normalizedBaseURI);
            String requestedName = getRequestResourceName(existingResource);
            ShapeTreeResource parentContainerResource = this.resourceAccessor.getResource(shapeTreeContext, parentURI);
            ValidationContext validationContext = validateAgainstParentContainer(shapeTreeContext, incomingRequestBodyGraph, normalizedBaseURI, parentContainerResource, requestedName, shapeTreeRequest);
            // Two reasons for passing through the request (and not performing validation):
            // 1. Validation returns no locators, meaning the parent container is not managed
            // 2. We're creating a resource and it has already passed validation
            if (validationContext == null || validationContext.getParentContainerLocator() == null || shapeTreeRequest.getResourceType() != ShapeTreeResourceType.CONTAINER) {
                return ShapeTreeValidationResponse.passThroughResponse(validationContext);
            }

            // TODO: If this is a managed hierarchy we need to create a locator no matter what the resource is
            // TODO: Need to be calling the proper assignment algorithm here, rather than planting over and over
            List<ShapeTreePlantResult> results = new ArrayList<>();
            ShapeTreeLocator locator = validationContext.getParentContainerLocator();

            if (requestedName.endsWith("/")) {
                requestedName = requestedName.replace("/","");
            }
            ShapeTreePlantResult result = plantShapeTree(shapeTreeContext, parentContainerResource, shapeTreeRequest.getBody(), shapeTreeRequest.getContentType(), locator, validationContext.getValidatingShapeTree(), requestedName);
            results.add(result);

            return createPlantResponse(results, shapeTreeRequest);
        } catch (ShapeTreeException ste) {
            return new ShapeTreeValidationResponse(ste);
        }
        catch (Exception ex) {
            return new ShapeTreeValidationResponse(new ShapeTreeException(500, ex.getMessage()));
        }
    }
}
