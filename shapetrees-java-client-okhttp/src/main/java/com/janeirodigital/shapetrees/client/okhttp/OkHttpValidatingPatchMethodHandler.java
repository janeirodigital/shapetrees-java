package com.janeirodigital.shapetrees.client.okhttp;

import com.janeirodigital.shapetrees.core.*;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import lombok.extern.slf4j.Slf4j;
import okhttp3.Request;
import okhttp3.Response;

import java.util.Optional;

import static com.janeirodigital.shapetrees.core.ManageableInstance.getInstance;
import static com.janeirodigital.shapetrees.core.helpers.RequestHelper.getIncomingParentContainerUrl;

@Slf4j
public class OkHttpValidatingPatchMethodHandler extends OkHttpValidatingMethodHandler {

    protected OkHttpValidatingPatchMethodHandler(ResourceAccessor accessor) {
        super(accessor);
    }

    @Override
    protected Optional<Response> validateRequest(Request nativeRequest, ShapeTreeRequest shapeTreeRequest, ShapeTreeContext shapeTreeContext, ManageableInstance targetInstance) throws ShapeTreeException {
        if (shapeTreeRequest.getContentType() == null || !shapeTreeRequest.getContentType().equalsIgnoreCase("application/sparql-update")) {
            log.error("Received a patch without a content type of application/sparql-update");
            throw new ShapeTreeException(415, "PATCH verb expects a content type of application/sparql-update");
        }
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
