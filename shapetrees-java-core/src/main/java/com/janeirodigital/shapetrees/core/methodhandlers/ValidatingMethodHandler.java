package com.janeirodigital.shapetrees.core.methodhandlers;

import com.janeirodigital.shapetrees.core.ShapeTreeResponse;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.ShapeTreeRequest;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.Optional;

public interface ValidatingMethodHandler {
    Optional<ShapeTreeResponse> validateRequest(ShapeTreeRequest shapeTreeRequest) throws ShapeTreeException, IOException, URISyntaxException;
}
