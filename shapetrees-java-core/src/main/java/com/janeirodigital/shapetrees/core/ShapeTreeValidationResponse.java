package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.ValidationResult;

public class ShapeTreeValidationResponse extends ShapeTreeResponse {
    private boolean validRequest;
    private ValidationResult validationResult;
    public boolean isValidRequest() { return this.validRequest; }

    public ShapeTreeValidationResponse(ValidationResult validationResult) { // fail: {create,update}ShapeTreeInstance, assignShapeTreeToResource
        super(
                null,
                new ResourceAttributes(),
                validationResult.getMessage(),
                422
        );
        this.validRequest = Boolean.TRUE.equals(validationResult.isValid());
        this.validationResult = validationResult;
    }

    public ShapeTreeValidationResponse() { // pass: {manage,plant,unplant,assign,unassign}ShapeTree...{To,From}Resource
        super(
                null,
                new ResourceAttributes(),
                "OK",
                201
        );
        this.validRequest = true;
        this.validationResult = null;

    }

    public ShapeTreeValidationResponse(ShapeTreeException ste) { // catch: {Delete,Patch,Post,Put}MethodHandler.validateRequest !! re-add?
        super(
                null,
                new ResourceAttributes(),
                ste.getMessage(),
                ste.getStatusCode()
        );
        this.validRequest = false;
    }
}
