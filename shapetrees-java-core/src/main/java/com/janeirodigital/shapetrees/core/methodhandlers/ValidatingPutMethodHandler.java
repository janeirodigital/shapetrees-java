package com.janeirodigital.shapetrees.core.methodhandlers;

import com.janeirodigital.shapetrees.core.DocumentResponse;
import com.janeirodigital.shapetrees.core.ResourceAccessor;
import com.janeirodigital.shapetrees.core.ManageableInstance;
import com.janeirodigital.shapetrees.core.ShapeTreeRequest;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.helpers.RequestHelper;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;

import java.util.Optional;

public class ValidatingPutMethodHandler extends AbstractValidatingMethodHandler implements ValidatingMethodHandler {

    public ValidatingPutMethodHandler(ResourceAccessor resourceAccessor) {
        super(resourceAccessor);
    }

    @Override
    public Optional<DocumentResponse> validateRequest(ShapeTreeRequest shapeTreeRequest) throws ShapeTreeException {
            ShapeTreeContext shapeTreeContext = RequestHelper.buildContextFromRequest(shapeTreeRequest);

            ManageableInstance targetInstance = new ManageableInstance(shapeTreeRequest.getUrl(), this.resourceAccessor, shapeTreeContext);
            if (targetInstance.wasCreatedFromManager()) {
                // Target resource is for shape tree manager, manage shape trees to plant and/or unplant
                return Optional.of(manageShapeTree(targetInstance, shapeTreeRequest));
            } else {
                ManageableInstance.ManageableResource targetResource = targetInstance.getManageableResource();
                shapeTreeRequest.setResourceType(RequestHelper.determineResourceType(shapeTreeRequest, targetInstance));
                if (targetResource.wasSuccessful()) {
                    // The target resource already exists
                    if (!targetResource.getManagerResourceUrl().isEmpty()) {
                        // If it is managed by a shape tree the update must be validated
                        return updateShapeTreeInstance(targetInstance, shapeTreeContext, shapeTreeRequest);
                    }
                } else {
                    // The target resource doesn't exist
                    ManageableInstance parentInstance = new ManageableInstance(targetResource.getParentContainerUrl(), this.resourceAccessor, shapeTreeContext);
                    if (!parentInstance.getManageableResource().getManagerResourceUrl().isEmpty()) {
                        // If the parent container is managed by a shape tree, the resource to create must be validated
                        return createShapeTreeInstance(targetInstance, parentInstance, shapeTreeRequest, targetResource.getName());
                    }
                }
            }

            // Reaching this point means validation was not necessary
            // Pass the request along with no validation
            return Optional.empty();
    }
}
