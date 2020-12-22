package com.janeirodigital.shapetrees.core.methodhandlers;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.ShapeTreeRequest;
import com.janeirodigital.shapetrees.core.ShapeTreeValidationResponse;

public interface ValidatingMethodHandler {
    ShapeTreeValidationResponse validateRequest(ShapeTreeRequest<?> shapeTreeRequest) throws ShapeTreeException;
}
