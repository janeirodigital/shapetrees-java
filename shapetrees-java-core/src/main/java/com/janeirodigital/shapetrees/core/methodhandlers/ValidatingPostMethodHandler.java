package com.janeirodigital.shapetrees.core.methodhandlers;

import com.janeirodigital.shapetrees.core.*;
import com.janeirodigital.shapetrees.core.enums.HttpHeader;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.helpers.RequestHelper;
import com.janeirodigital.shapetrees.core.ShapeTreeContext;
import lombok.extern.slf4j.Slf4j;

import java.util.Optional;
import java.util.UUID;

import static com.janeirodigital.shapetrees.core.ManageableInstance.getInstance;

@Slf4j
public class ValidatingPostMethodHandler extends AbstractValidatingMethodHandler implements ValidatingMethodHandler {

    public ValidatingPostMethodHandler(ResourceAccessor resourceAccessor) {
        super(resourceAccessor);
    }

    @Override
    public Optional<DocumentResponse> validateRequest(ShapeTreeRequest shapeTreeRequest) throws ShapeTreeException {
            ShapeTreeContext shapeTreeContext = RequestHelper.buildContextFromRequest(shapeTreeRequest);

            // Look up the target container for the POST. Error if it doesn't exist, or is a manager resource
            ManageableInstance targetContainer = getInstance(this.resourceAccessor, shapeTreeContext, shapeTreeRequest.getUrl());

            // Get resource name from the slug or default to UUID
            String proposedName = shapeTreeRequest.getHeaders().firstValue(HttpHeader.SLUG.getValue()).orElse(UUID.randomUUID().toString());

            // If the parent container is managed by a shape tree, the proposed resource being posted must be
            // validated against the parent tree.
            if (targetContainer.isManaged()) {
                shapeTreeRequest.setResourceType(RequestHelper.getIncomingResourceType(shapeTreeRequest));
                return this.requestHandler.createShapeTreeInstance(targetContainer, targetContainer, shapeTreeRequest, proposedName);
            }

            // Reaching this point means validation was not necessary
            // Pass the request along with no validation
            return Optional.empty();
    }
}
