package com.janeirodigital.shapetrees.core.methodhandlers;

import com.janeirodigital.shapetrees.core.DocumentResponse;
import com.janeirodigital.shapetrees.core.ResourceAccessor;
import com.janeirodigital.shapetrees.core.ShapeTreeInstance;
import com.janeirodigital.shapetrees.core.ShapeTreeRequest;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.helpers.RequestHelper;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;
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

            ShapeTreeInstance targetInstance = new ShapeTreeInstance(shapeTreeRequest.getUrl(), this.resourceAccessor, shapeTreeContext);

            if (targetInstance.wasCreatedFromManager()) {
                // Target resource is for shape tree manager, manage shape trees to plant and/or unplant
                return Optional.of(manageShapeTree(targetInstance, shapeTreeRequest));
            } else {
                ShapeTreeInstance.ManagedResource targetResource = targetInstance.getManagedResource();
                shapeTreeRequest.setResourceType(RequestHelper.determineResourceType(shapeTreeRequest, targetInstance));
                if (targetResource.wasSuccessful()) {
                    // The target resource already exists
                    if (!targetResource.getManagerResourceUrl().isEmpty()) {
                        // If it is managed by a shape tree the update must be validated
                        return updateShapeTreeInstance(targetInstance, shapeTreeContext, shapeTreeRequest);
                    }
                } else {
                    // The target resource doesn't exist
                    ShapeTreeInstance parentInstance = new ShapeTreeInstance(targetResource.getParentContainerUrl(), this.resourceAccessor, shapeTreeContext);
                    if (!targetResource.getManagerResourceUrl().isEmpty()) {
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
