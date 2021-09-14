package com.janeirodigital.shapetrees.core.methodhandlers;

import com.janeirodigital.shapetrees.core.*;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.Optional;

public class ValidatingPutMethodHandler extends AbstractValidatingMethodHandler implements ValidatingMethodHandler {

    public ValidatingPutMethodHandler(ResourceAccessor resourceAccessor) {
        super(resourceAccessor);
    }

    @Override
    public Optional<DocumentResponse> validateRequest(ShapeTreeRequest shapeTreeRequest) throws ShapeTreeException, URISyntaxException {
            ShapeTreeContext shapeTreeContext = buildContextFromRequest(shapeTreeRequest);

            ShapeTreeResource targetResource = getRequestResource(shapeTreeContext, shapeTreeRequest);

            if (targetResource.isMetadata()) {
                // Target resource is for shape tree metadata, manage shape trees to plant and/or unplant
                return Optional.of(manageShapeTree(shapeTreeContext, shapeTreeRequest, targetResource));
            } else {
                if (targetResource.isExists()) {
                    // The target resource already exists
                    if (targetResource.isManaged()) {
                        // If it is managed by a shape tree the update must be validated
                        return updateShapeTreeInstance(shapeTreeContext, shapeTreeRequest);
                    }
                } else {
                    // The target resource doesn't exist
                    ShapeTreeResource parentResource = this.resourceAccessor.getResource(shapeTreeContext, getParentContainerURI(targetResource));
                    if (parentResource.isManaged()) {
                        // If the parent container is managed by a shape tree, the resource to create must be validated
                        return createShapeTreeInstance(shapeTreeContext, shapeTreeRequest, getRequestResourceName(targetResource));
                    }
                }
            }

            // Reaching this point means validation was not necessary
            // Pass the request along with no validation
            return Optional.empty();
    }
}
