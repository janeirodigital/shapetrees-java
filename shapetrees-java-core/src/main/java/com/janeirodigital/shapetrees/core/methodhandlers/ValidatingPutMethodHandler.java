package com.janeirodigital.shapetrees.core.methodhandlers;

import com.janeirodigital.shapetrees.core.*;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.helpers.RequestHelper;
import com.janeirodigital.shapetrees.core.ShapeTreeContext;

import java.util.Optional;

import static com.janeirodigital.shapetrees.core.ManageableInstance.getInstance;
import static com.janeirodigital.shapetrees.core.helpers.RequestHelper.getIncomingParentContainerUrl;

public class ValidatingPutMethodHandler extends AbstractValidatingMethodHandler implements ValidatingMethodHandler {

    public ValidatingPutMethodHandler(ResourceAccessor resourceAccessor) {
        super(resourceAccessor);
    }

    @Override
    public Optional<DocumentResponse> validateRequest(ShapeTreeRequest shapeTreeRequest) throws ShapeTreeException {

        ShapeTreeContext shapeTreeContext = RequestHelper.buildContextFromRequest(shapeTreeRequest);

            ManageableInstance targetInstance = getInstance(this.resourceAccessor, shapeTreeContext, shapeTreeRequest.getUrl());

            if (targetInstance.wasRequestForManager()) {
                // Target resource is for shape tree manager, manage shape trees to plant and/or unplant
                return Optional.of(this.requestHandler.updateShapeTreeManager(targetInstance, shapeTreeRequest));
            } else {
                ManageableResource targetResource = targetInstance.getManageableResource();
                shapeTreeRequest.setResourceType(RequestHelper.getIncomingResourceType(shapeTreeRequest));
                if (targetResource.isExists()) {
                    // The target resource already exists
                    if (targetInstance.isManaged()) {
                        // If it is managed by a shape tree the update must be validated
                        return this.requestHandler.updateShapeTreeInstance(targetInstance, shapeTreeRequest);
                    }
                } else {
                    // The target resource doesn't exist
                    ManageableInstance parentInstance = getInstance(this.resourceAccessor, shapeTreeContext, getIncomingParentContainerUrl(shapeTreeRequest));
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
