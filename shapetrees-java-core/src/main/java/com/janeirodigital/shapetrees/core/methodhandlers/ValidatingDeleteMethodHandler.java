package com.janeirodigital.shapetrees.core.methodhandlers;

import com.janeirodigital.shapetrees.core.ResourceAccessor;
import com.janeirodigital.shapetrees.core.ShapeTreeRequest;
import com.janeirodigital.shapetrees.core.ShapeTreeValidationResponse;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;

public class ValidatingDeleteMethodHandler extends AbstractValidatingMethodHandler implements ValidatingMethodHandler {

    public ValidatingDeleteMethodHandler(ResourceAccessor resourceAccessor) {
        super(resourceAccessor);
    }

    @Override
    public ShapeTreeValidationResponse validateRequest(ShapeTreeRequest<?> shapeTreeRequest) throws ShapeTreeException {
        try {
            return ShapeTreeValidationResponse.passThroughResponse();
        } catch (Exception ex) {
            return new ShapeTreeValidationResponse(new ShapeTreeException(500, ex.getMessage()));
        }
    }
}
