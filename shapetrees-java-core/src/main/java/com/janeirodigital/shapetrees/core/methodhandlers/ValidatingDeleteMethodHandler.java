package com.janeirodigital.shapetrees.core.methodhandlers;

import com.janeirodigital.shapetrees.core.ResourceAccessor;
import com.janeirodigital.shapetrees.core.ShapeTreeRequest;
import com.janeirodigital.shapetrees.core.ShapeTreeResource;
import com.janeirodigital.shapetrees.core.ShapeTreeValidationResponse;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;

public class ValidatingDeleteMethodHandler extends AbstractValidatingMethodHandler implements ValidatingMethodHandler {

    public ValidatingDeleteMethodHandler(ResourceAccessor resourceAccessor) {
        super(resourceAccessor);
    }

    @Override
    public ShapeTreeValidationResponse validateRequest(ShapeTreeRequest<?> shapeTreeRequest) {
        try {

            ShapeTreeContext shapeTreeContext = buildContextFromRequest(shapeTreeRequest);
            ShapeTreeResource targetResource = getRequestResource(shapeTreeContext, shapeTreeRequest);

            if (targetResource.isMetadata() && targetResource.isExists()) {
                // If the DELETE request is for an existing shapetree metadata resource,
                // it must be evaluated to determine if unplanting is necessary
                return manageShapeTree(shapeTreeContext, shapeTreeRequest, targetResource);
            }

            // Reaching this point means validation was not necessary
            // Pass the request along with no validation
            return ShapeTreeValidationResponse.passThroughResponse();

        } catch (ShapeTreeException ste) {
            return new ShapeTreeValidationResponse(ste);
        } catch (Exception ex) {
            return new ShapeTreeValidationResponse(new ShapeTreeException(500, ex.getMessage()));
        }
    }

}
