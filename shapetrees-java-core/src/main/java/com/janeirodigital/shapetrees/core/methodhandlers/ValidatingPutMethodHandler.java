package com.janeirodigital.shapetrees.core.methodhandlers;

import com.janeirodigital.shapetrees.core.ResourceAccessor;
import com.janeirodigital.shapetrees.core.ShapeTreeRequest;
import com.janeirodigital.shapetrees.core.ShapeTreeValidationResponse;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;

public class ValidatingPutMethodHandler extends AbstractValidatingMethodHandler implements ValidatingMethodHandler {

    public ValidatingPutMethodHandler(ResourceAccessor resourceAccessor) {
        super(resourceAccessor);
    }

    @Override
    public ShapeTreeValidationResponse validateRequest(ShapeTreeRequest<?> shapeTreeRequest) {
        try {

            ShapeTreeContext shapeTreeContext = buildContextFromRequest(shapeTreeRequest);

            // 1. Determine whether we are creating a new resource or updating one
            // 2. If we are creating a new resource
            //    1. Go through the same logic / evaluation as POST
            // 3. If we are updating a resource
            //    1. If the resource is managed, check resource conformance

            return ShapeTreeValidationResponse.passThroughResponse();

        //} catch (ShapeTreeException ste) {
        //    return new ShapeTreeValidationResponse(ste);
        } catch (Exception ex) {
            return new ShapeTreeValidationResponse(new ShapeTreeException(500, ex.getMessage()));
        }
    }
}
