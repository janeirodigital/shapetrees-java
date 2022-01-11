package com.janeirodigital.shapetrees.client.okhttp;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.validation.ShapeTreeContext;
import com.janeirodigital.shapetrees.core.validation.ShapeTreeRequest;
import com.janeirodigital.shapetrees.core.resources.ManageableInstance;
import com.janeirodigital.shapetrees.core.resources.ManageableResource;
import com.janeirodigital.shapetrees.core.resources.ResourceAccessor;
import lombok.extern.slf4j.Slf4j;
import okhttp3.Request;
import okhttp3.Response;

import java.util.Optional;

import static com.janeirodigital.shapetrees.core.resources.ManageableInstance.getInstance;
import static com.janeirodigital.shapetrees.core.helpers.RequestHelper.getIncomingParentContainerUrl;

@Slf4j
public class OkHttpValidatingPutMethodHandler extends OkHttpValidatingMethodHandler {

    protected OkHttpValidatingPutMethodHandler(ResourceAccessor accessor) {
        super(accessor);
    }

    @Override
    protected Optional<Response> validateRequest(Request nativeRequest, ShapeTreeRequest shapeTreeRequest, ShapeTreeContext shapeTreeContext, ManageableInstance targetInstance) throws ShapeTreeException {
        ManageableResource targetResource = targetInstance.getManageableResource();
        if (targetResource.isExists()) {
            // The target resource already exists
            if (targetInstance.isManaged()) { return validateManagedResourceUpdate(nativeRequest, shapeTreeRequest, targetInstance); }
        } else {
            // The target resource doesn't exist
            ManageableInstance parentInstance = getInstance(this.resourceAccessor, shapeTreeContext, getIncomingParentContainerUrl(shapeTreeRequest));
            if (parentInstance.isManaged()) { return validateManagedResourceCreation(nativeRequest, shapeTreeRequest, shapeTreeContext, parentInstance, targetInstance); }
        }
        // Reaching this point means a response did not need to be fabricated
        // Pass the request along to the target resource server
        return Optional.empty();
    }

}
