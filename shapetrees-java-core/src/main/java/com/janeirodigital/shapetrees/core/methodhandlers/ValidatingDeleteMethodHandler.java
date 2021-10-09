package com.janeirodigital.shapetrees.core.methodhandlers;

import com.janeirodigital.shapetrees.core.*;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.Optional;

public class ValidatingDeleteMethodHandler extends AbstractValidatingMethodHandler implements ValidatingMethodHandler {

    public ValidatingDeleteMethodHandler(ResourceAccessor resourceAccessor) {
        super(resourceAccessor);
    }

    @Override
    public Optional<DocumentResponse> validateRequest(ShapeTreeRequest shapeTreeRequest) throws ShapeTreeException, URISyntaxException {
            ShapeTreeContext shapeTreeContext = buildContextFromRequest(shapeTreeRequest);
            ResourceConstellation rc = new ResourceConstellation(shapeTreeRequest.getURI(), this.resourceAccessor, shapeTreeContext);

            if (rc.isMetadata() && rc.getMetadataResource().isExists()) {
                // If the DELETE request is for an existing shapetree metadata resource,
                // it must be evaluated to determine if unplanting is necessary
                return Optional.of(manageShapeTree(rc, shapeTreeRequest, rc.getMetadataResource()));
            }

            // Reaching this point means validation was not necessary
            // Pass the request along with no validation
            return Optional.empty();
    }

}
