package com.janeirodigital.shapetrees.core.methodhandlers;

import com.janeirodigital.shapetrees.core.DocumentResponse;
import com.janeirodigital.shapetrees.core.ResourceAccessor;
import com.janeirodigital.shapetrees.core.ManageableInstance;
import com.janeirodigital.shapetrees.core.ShapeTreeRequest;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.helpers.RequestHelper;
import com.janeirodigital.shapetrees.core.ShapeTreeContext;

import java.util.Optional;

import static com.janeirodigital.shapetrees.core.ManageableInstance.getInstance;

public class ValidatingDeleteMethodHandler extends AbstractValidatingMethodHandler implements ValidatingMethodHandler {

    public ValidatingDeleteMethodHandler(ResourceAccessor resourceAccessor) {
        super(resourceAccessor);
    }

    @Override
    public Optional<DocumentResponse> validateRequest(ShapeTreeRequest shapeTreeRequest) throws ShapeTreeException {
            ShapeTreeContext shapeTreeContext = RequestHelper.buildContextFromRequest(shapeTreeRequest);
            ManageableInstance targetInstance = getInstance(this.resourceAccessor, shapeTreeContext, shapeTreeRequest.getUrl());

            if (targetInstance.wasRequestForManager() && targetInstance.getManagerResource().isExists()) {
                // If the DELETE request is for an existing shapetree manager resource,
                // it must be evaluated to determine if unplanting is necessary
                return Optional.of(this.requestHandler.manageShapeTree(targetInstance, shapeTreeRequest));
            }

            // Reaching this point means validation was not necessary
            // Pass the request along with no validation
            return Optional.empty();
    }

}
