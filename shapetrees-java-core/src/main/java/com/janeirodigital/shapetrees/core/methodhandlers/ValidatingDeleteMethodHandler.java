package com.janeirodigital.shapetrees.core.methodhandlers;

import com.janeirodigital.shapetrees.core.*;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;

import java.net.MalformedURLException;
import java.util.Optional;

public class ValidatingDeleteMethodHandler extends AbstractValidatingMethodHandler implements ValidatingMethodHandler {

    public ValidatingDeleteMethodHandler(ResourceAccessor resourceAccessor) {
        super(resourceAccessor);
    }

    @Override
    public Optional<DocumentResponse> validateRequest(ShapeTreeRequest shapeTreeRequest) throws ShapeTreeException {
            ShapeTreeContext shapeTreeContext = buildContextFromRequest(shapeTreeRequest);
            ShapeTreeResource rc = new ShapeTreeResource(shapeTreeRequest.getUrl(), this.resourceAccessor, shapeTreeContext);

            if (rc.wasCreatedFromMetadata() && rc.getMetadataResourceFork().isExists()) {
                // If the DELETE request is for an existing shapetree metadata resource,
                // it must be evaluated to determine if unplanting is necessary
                return Optional.of(manageShapeTree(rc, shapeTreeRequest));
            }

            // Reaching this point means validation was not necessary
            // Pass the request along with no validation
            return Optional.empty();
    }

}
