package com.janeirodigital.shapetrees.core.methodhandlers;

import com.janeirodigital.shapetrees.core.DocumentResponse;
import com.janeirodigital.shapetrees.core.ResourceAccessor;
import com.janeirodigital.shapetrees.core.ManageableInstance;
import com.janeirodigital.shapetrees.core.ShapeTreeRequest;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.helpers.RequestHelper;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;

import java.util.Optional;

public class ValidatingDeleteMethodHandler extends AbstractValidatingMethodHandler implements ValidatingMethodHandler {

    public ValidatingDeleteMethodHandler(ResourceAccessor resourceAccessor) {
        super(resourceAccessor);
    }

    @Override
    public Optional<DocumentResponse> validateRequest(ShapeTreeRequest shapeTreeRequest) throws ShapeTreeException {
            ShapeTreeContext shapeTreeContext = RequestHelper.buildContextFromRequest(shapeTreeRequest);
            ManageableInstance targetInstance = new ManageableInstance(shapeTreeRequest.getUrl(), this.resourceAccessor, shapeTreeContext);

            if (targetInstance.wasCreatedFromManager() && targetInstance.getManagerResource().wasSuccessful()) {
                // If the DELETE request is for an existing shapetree manager resource,
                // it must be evaluated to determine if unplanting is necessary
                return Optional.of(manageShapeTree(targetInstance, shapeTreeRequest));
            }

            // Reaching this point means validation was not necessary
            // Pass the request along with no validation
            return Optional.empty();
    }

}
