package com.janeirodigital.shapetrees.core.methodhandlers;

import com.janeirodigital.shapetrees.core.*;
import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.helpers.RequestHelper;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;
import lombok.extern.slf4j.Slf4j;

import java.util.Optional;
import java.util.UUID;

@Slf4j
public class ValidatingPostMethodHandler extends AbstractValidatingMethodHandler implements ValidatingMethodHandler {

    public ValidatingPostMethodHandler(ResourceAccessor resourceAccessor) {
        super(resourceAccessor);
    }

    @Override
    public Optional<DocumentResponse> validateRequest(ShapeTreeRequest shapeTreeRequest) throws ShapeTreeException {
            ShapeTreeContext shapeTreeContext = RequestHelper.buildContextFromRequest(shapeTreeRequest);

            // Look up the target container for the POST. Error if it doesn't exist, or is a manager resource
            ManageableInstance targetContainer = new ManageableInstance(shapeTreeRequest.getUrl(), this.resourceAccessor, shapeTreeContext);

            // Get resource name from the slug or default to UUID
            String proposedName = shapeTreeRequest.getHeaders().firstValue(HttpHeaders.SLUG.getValue()).orElse(UUID.randomUUID().toString());

            // If the parent container is managed by a shape tree, the proposed resource being posted must be
            // validated against the parent tree.
            if (!targetContainer.getManageableResource().getManagerResourceUrl().isEmpty()) {
                shapeTreeRequest.setResourceType(RequestHelper.determineResourceType(shapeTreeRequest, targetContainer));
                return createShapeTreeInstance(targetContainer, targetContainer, shapeTreeRequest, proposedName);
            }

            // Reaching this point means validation was not necessary
            // Pass the request along with no validation
            return Optional.empty();
    }
}
