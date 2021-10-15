package com.janeirodigital.shapetrees.core.methodhandlers;

import com.janeirodigital.shapetrees.core.*;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;

import java.net.URISyntaxException;
import java.util.Optional;

public class ValidatingPutMethodHandler extends AbstractValidatingMethodHandler implements ValidatingMethodHandler {

    public ValidatingPutMethodHandler(ResourceAccessor resourceAccessor) {
        super(resourceAccessor);
    }

    @Override
    public Optional<DocumentResponse> validateRequest(ShapeTreeRequest shapeTreeRequest) throws ShapeTreeException, URISyntaxException {
            ShapeTreeContext shapeTreeContext = buildContextFromRequest(shapeTreeRequest);

            ShapeTreeResource rc = new ShapeTreeResource(shapeTreeRequest.getURI(), this.resourceAccessor, shapeTreeContext);
            if (rc.createdFromMetadata()) {
                // Target resource is for shape tree metadata, manage shape trees to plant and/or unplant
                return Optional.of(manageShapeTree(rc, shapeTreeRequest));
            } else {
                ShapeTreeResource.UserOwned targetResource = rc.getUserOwnedResourceFork();
                shapeTreeRequest.setResourceType(determineResourceType(shapeTreeRequest, rc));
                if (targetResource.isExists()) {
                    // The target resource already exists
                    if (!targetResource.getMetadataResourceUri().isEmpty()) {
                        // If it is managed by a shape tree the update must be validated
                        return updateShapeTreeInstance(rc, shapeTreeContext, shapeTreeRequest);
                    }
                } else {
                    // The target resource doesn't exist
                    ShapeTreeResource parentResource = new ShapeTreeResource(getParentContainerURI(targetResource), this.resourceAccessor, shapeTreeContext);
                    if (!parentResource.getUserOwnedResourceFork().getMetadataResourceUri().isEmpty()) {
                        // If the parent container is managed by a shape tree, the resource to create must be validated
                        return createShapeTreeInstance(rc, parentResource, shapeTreeRequest, getRequestResourceName(targetResource));
                    }
                }
            }

            // Reaching this point means validation was not necessary
            // Pass the request along with no validation
            return Optional.empty();
    }
}
