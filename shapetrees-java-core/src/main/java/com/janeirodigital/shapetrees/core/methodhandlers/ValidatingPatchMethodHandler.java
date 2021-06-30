package com.janeirodigital.shapetrees.core.methodhandlers;

import com.janeirodigital.shapetrees.core.ResourceAccessor;
import com.janeirodigital.shapetrees.core.ShapeTreeRequest;
import com.janeirodigital.shapetrees.core.ShapeTreeResource;
import com.janeirodigital.shapetrees.core.ShapeTreeValidationResponse;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;
import lombok.extern.slf4j.Slf4j;

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

            return ShapeTreeValidationResponse.passThroughResponse();


        } catch (ShapeTreeException ste) {
            return new ShapeTreeValidationResponse(ste);
        } catch (Exception ex) {
            return new ShapeTreeValidationResponse(new ShapeTreeException(500, ex.getMessage()));
        }

    }
}
