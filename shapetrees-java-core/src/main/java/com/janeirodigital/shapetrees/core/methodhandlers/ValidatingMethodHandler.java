package com.janeirodigital.shapetrees.core.methodhandlers;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.ShapeTreeRequest;
import com.janeirodigital.shapetrees.core.ShapeTreeValidationResponse;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.Optional;

public interface ValidatingMethodHandler {
    Optional<ShapeTreeValidationResponse> validateRequest(ShapeTreeRequest shapeTreeRequest) throws IOException, URISyntaxException;
}
