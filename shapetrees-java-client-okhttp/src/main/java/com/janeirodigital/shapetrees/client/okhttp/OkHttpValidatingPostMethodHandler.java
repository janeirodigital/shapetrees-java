package com.janeirodigital.shapetrees.client.okhttp;

import com.janeirodigital.shapetrees.core.enums.HttpHeader;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.helpers.RequestHelper;
import com.janeirodigital.shapetrees.core.validation.ShapeTreeContext;
import com.janeirodigital.shapetrees.core.validation.ShapeTreeRequest;
import com.janeirodigital.shapetrees.core.resources.ManageableInstance;
import com.janeirodigital.shapetrees.core.resources.ManageableResource;
import com.janeirodigital.shapetrees.core.resources.ResourceAccessor;
import lombok.extern.slf4j.Slf4j;
import okhttp3.Request;
import okhttp3.Response;

import java.net.URL;
import java.util.Optional;
import java.util.UUID;

import static com.janeirodigital.shapetrees.core.resources.ManageableInstance.getInstance;
import static com.janeirodigital.shapetrees.core.helpers.DocumentResponseHelper.getHeader;

@Slf4j
public class OkHttpValidatingPostMethodHandler extends OkHttpValidatingMethodHandler {

    protected OkHttpValidatingPostMethodHandler(ResourceAccessor accessor) {
        super(accessor);
    }

    @Override
    protected Optional<Response> validateRequest(Request nativeRequest, ShapeTreeRequest shapeTreeRequest, ShapeTreeContext shapeTreeContext, ManageableInstance targetInstance) throws ShapeTreeException {
        ManageableResource containerResource = targetInstance.getManageableResource();
        if (containerResource.isExists()) {
            // The target container already exists that is being POSTed into
            if (targetInstance.isManaged()) {
                String proposedName = getHeader(shapeTreeRequest.getHeaders(), HttpHeader.SLUG).orElse(UUID.randomUUID().toString());
                URL targetResourceUrl = RequestHelper.normalizeSolidResourceUrl(containerResource.getUrl(), proposedName, shapeTreeRequest.getResourceType());
                ManageableInstance proposedInstance = getInstance(this.resourceAccessor, shapeTreeContext, targetResourceUrl);
                if (proposedInstance.getManageableResource().isExists()) { throw new ShapeTreeException(409, "Cannot create target resource " + targetResourceUrl + "because it already exists"); }
                return validateManagedResourceCreation(nativeRequest, shapeTreeRequest, shapeTreeContext, targetInstance, proposedInstance);
            }
        } else {
            // The target container doesn't exist, which is going to be problem. Let the server tell them rather than
            // fabricate our own 404
            log.error("{} request to {} will fail because container doesn't exist. Passing request to server to respond.", shapeTreeRequest.getMethod(), shapeTreeRequest.getUrl());
            return Optional.empty();
        }
        // Reaching this point means a response did not need to be fabricated
        // Pass the request along to the target resource server
        return Optional.empty();
    }

}
