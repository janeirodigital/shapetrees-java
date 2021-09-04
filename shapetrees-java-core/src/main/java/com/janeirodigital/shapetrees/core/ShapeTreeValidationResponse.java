package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.ValidationResult;

public class ShapeTreeValidationResponse extends ShapeTreeResponse {
    private boolean requestFulfilled;
    private boolean validRequest;
    private ValidationResult validationResult;
    public boolean isValidRequest() { return this.validRequest; }
    public boolean isRequestFulfilled() { return this.requestFulfilled; }

    public ShapeTreeValidationResponse(ValidationResult validationResult) { // fail: {create,update}ShapeTreeInstance, assignShapeTreeToResource

        super(
                null,
                new ResourceAttributes(),
                Boolean.FALSE.equals(validationResult.isValid()) ? validationResult.getMessage() : null,
                Boolean.FALSE.equals(validationResult.isValid()) ? 422 : 999
        );
        this.requestFulfilled = true;
        this.validRequest = Boolean.TRUE.equals(validationResult.isValid());
        this.validationResult = validationResult;
    }

    public ShapeTreeValidationResponse(Boolean validRequest, Boolean requestFulfilled) { // pass: {manage,plant,unplant,assign,unassign}ShapeTree...{To,From}Resource
        super(
                null,
                new ResourceAttributes(),
                "OK",
                201
        );
        this.requestFulfilled = true;
        this.validRequest = validRequest;
        this.validationResult = null;

    }

    public ShapeTreeValidationResponse(Boolean requestFulfilled, Boolean validRequest, ValidationResult validationResult) { // passThroughResponse

        super(
                null,
                new ResourceAttributes(),
                null,
                999
        );
        this.requestFulfilled = false;
        this.validRequest = validRequest;
        this.validationResult = validationResult;

    }

    public ShapeTreeValidationResponse(ShapeTreeException ste) { // catch: {Delete,Patch,Post,Put}MethodHandler.validateRequest
        super(
                null,
                new ResourceAttributes(),
                ste.getMessage(),
                ste.getStatusCode()
        );
        this.validRequest = false;
        this.requestFulfilled = true;
    }

    public static ShapeTreeValidationResponse passThroughResponse() { // unmanaged: {Delete,Patch,Post,Put}MethodHandler.validateRequest
        return new ShapeTreeValidationResponse(false, true, null);
    }
}
