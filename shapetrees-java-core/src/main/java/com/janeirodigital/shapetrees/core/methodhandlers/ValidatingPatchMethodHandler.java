package com.janeirodigital.shapetrees.core.methodhandlers;

import com.janeirodigital.shapetrees.core.*;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.helpers.RequestHelper;
import com.janeirodigital.shapetrees.core.ShapeTreeContext;
import lombok.extern.slf4j.Slf4j;

import java.util.Optional;

@Slf4j
public class ValidatingPatchMethodHandler extends AbstractValidatingMethodHandler implements ValidatingMethodHandler {

    public ValidatingPatchMethodHandler(ResourceAccessor resourceAccessor) {
        super(resourceAccessor);
    }

    @Override
    public Optional<DocumentResponse> validateRequest(ShapeTreeRequest shapeTreeRequest) throws ShapeTreeException {
            if (shapeTreeRequest.getContentType() == null || !shapeTreeRequest.getContentType().equalsIgnoreCase("application/sparql-update")) {
                log.error("Received a patch without a content type of application/sparql-update");
                throw new ShapeTreeException(415, "PATCH verb expects a content type of application/sparql-update");
            }

            ShapeTreeContext shapeTreeContext = RequestHelper.buildContextFromRequest(shapeTreeRequest);

            ManageableInstance targetInstance = this.resourceAccessor.getInstance(shapeTreeContext, shapeTreeRequest.getUrl());

            if (targetInstance.wasRequestForManager()) {
                // Target resource is for shape tree manager, manage shape trees to plant and/or unplant
                return Optional.of(this.requestHandler.manageShapeTree(targetInstance, shapeTreeRequest));
            } else {
                ManageableResource targetResource = targetInstance.getManageableResource();
                shapeTreeRequest.setResourceType(RequestHelper.determineResourceType(shapeTreeRequest, targetInstance));
                if (targetResource.isExists()) {
                    // The target resource already exists
                    if (targetInstance.isManaged()) {
                        // If it is managed by a shape tree the update must be validated
                        return this.requestHandler.updateShapeTreeInstance(targetInstance, shapeTreeContext, shapeTreeRequest);
                    }
                } else {
                    // The target resource doesn't exist
                    ManageableInstance parentInstance = this.resourceAccessor.getInstance(shapeTreeContext, targetResource.getParentContainerUrl());
                    if (parentInstance.isManaged()) {
                        // If the parent container is managed by a shape tree, the resource to create must be validated
                        return this.requestHandler.createShapeTreeInstance(targetInstance, parentInstance, shapeTreeRequest, targetResource.getName());
                    }
                }
            }

            // Reaching this point means validation was not necessary
            // Pass the request along with no validation
            return Optional.empty();
    }
}
