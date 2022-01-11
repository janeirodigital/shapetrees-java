package com.janeirodigital.shapetrees.client.okhttp;

import com.janeirodigital.shapetrees.core.*;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import lombok.extern.slf4j.Slf4j;
import okhttp3.Request;
import okhttp3.Response;

import java.util.Optional;

import static com.janeirodigital.shapetrees.core.ManageableInstance.createInstanceResource;
import static com.janeirodigital.shapetrees.core.ShapeTreeRequestHandler.*;
import static com.janeirodigital.shapetrees.client.okhttp.OkHttpHelper.createInvalidResponse;
import static com.janeirodigital.shapetrees.client.okhttp.OkHttpHelper.createResponse;

/**
 * Abstract class providing reusable functionality to different method handlers
 */
@Slf4j
public abstract class OkHttpValidatingMethodHandler {

    protected final ResourceAccessor resourceAccessor;

    protected OkHttpValidatingMethodHandler(ResourceAccessor resourceAccessor) {
        this.resourceAccessor = resourceAccessor;
    }

    protected abstract Optional<Response> validateRequest(Request nativeRequest, ShapeTreeRequest shapeTreeRequest, ShapeTreeContext shapeTreeContext, ManageableInstance requestInstance) throws ShapeTreeException;

    Optional<Response> validateManagedResourceUpdate(Request nativeRequest, ShapeTreeRequest shapeTreeRequest, ManageableInstance proposedInstance) throws ShapeTreeException {
        // Validate the proposed update against the shapetree(s) managing the managed resource
        ValidationResult result = validateResourceUpdate(this.resourceAccessor, proposedInstance, shapeTreeRequest);
        // If validation was unsuccessful, craft and return invalid response
        if (!result.isValid()) { return Optional.of(createInvalidResponse(nativeRequest, result)); }
        log.info("Validation Successful: {} request to {}", shapeTreeRequest.getMethod(), shapeTreeRequest.getUrl());
        // Returning an empty value tells the interceptor that we want to allow this update to proceed to the
        // resource server directly, rather than fulfilling it ourselves and fabricating a response
        return Optional.empty();
    }

    Optional<Response> validateManagedResourceCreation(Request nativeRequest, ShapeTreeRequest shapeTreeRequest, ShapeTreeContext shapeTreeContext, ManageableInstance parentInstance, ManageableInstance proposedInstance) throws ShapeTreeException {
        ManageableResource targetResource = proposedInstance.getManageableResource();
        ShapeTreeManager manager = parentInstance.getManagerResource().getManager();
        if (!manager.hasContainingAssignments()) { return Optional.empty(); } // Parent container doesn't constrain members with st:contains
        ContainingValidationResult containingResult = validateResourceCreation(this.resourceAccessor, proposedInstance, parentInstance, shapeTreeRequest, targetResource.getName());
        // check for bad result and create error response
        if (!containingResult.isValid()) { return Optional.of(createInvalidResponse(nativeRequest, containingResult)); }
        ManageableInstance createdInstance = createInstanceResource(this.resourceAccessor, shapeTreeContext, targetResource.getUrl(), shapeTreeRequest.getHeaders(), shapeTreeRequest.getBody(), shapeTreeRequest.getContentType());
        for (var validationMap : containingResult.getEntries()) {
            ShapeTreeAssignment containingAssignment = validationMap.getKey();
            ValidationResult result = validationMap.getValue();
            ShapeTreeAssignment rootAssignment = getRootAssignment(this.resourceAccessor, shapeTreeContext, containingAssignment);
            ValidationResult assignmentResult = assignShapeTreeToResource(this.resourceAccessor, createdInstance, shapeTreeContext, null, rootAssignment, containingAssignment, result);
            if (!assignmentResult.isValid()) { return Optional.of(createInvalidResponse(nativeRequest, assignmentResult)); }
        }
        return Optional.of(createResponse(nativeRequest, 201));
    }
}
