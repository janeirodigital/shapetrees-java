package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.ValidationResult;

public class ShapeTreeValidationResponse extends ShapeTreeResponse {
    private boolean requestFulfilled;
    private boolean validRequest;
    private ValidationResult validationResult;
    public boolean isValidRequest() { return this.validRequest; }
    public boolean isRequestFulfilled() { return this.requestFulfilled; }

    public ShapeTreeValidationResponse(ValidationResult validationResult) {

        super(
                null,
                new ResourceAttributes(),
                Boolean.FALSE.equals(validationResult.isValid()) ? validationResult.getMessage() : null,
                Boolean.FALSE.equals(validationResult.isValid()) ? 422 : 999
        );
        this.requestFulfilled = false;
        this.validRequest = Boolean.TRUE.equals(validationResult.isValid());
        this.validationResult = validationResult;
    }

    public ShapeTreeValidationResponse(Boolean validRequest, Boolean requestFulfilled) {
        this(validRequest, requestFulfilled, 201, "OK");
    }

    public ShapeTreeValidationResponse(Boolean validRequest, Boolean requestFulfilled, int statusCode, String body) {

        super(
                null,
                new ResourceAttributes(),
                body,
                statusCode
        );
        this.requestFulfilled = requestFulfilled;
        this.validRequest = validRequest;
        this.validationResult = null;

    }

    public ShapeTreeValidationResponse(Boolean requestFulfilled, Boolean validRequest, ValidationResult validationResult) {

        super(
                null,
                new ResourceAttributes(),
                null,
                999
        );
        this.requestFulfilled = requestFulfilled;
        this.validRequest = validRequest;
        this.validationResult = validationResult;

    }

    public ShapeTreeValidationResponse(ShapeTreeException ste) {
        super(
                null,
                new ResourceAttributes(),
                ste.getMessage(),
                ste.getStatusCode()
        );
        this.validRequest = false;
        this.requestFulfilled = false;
    }

    public static ShapeTreeValidationResponse passThroughResponse() {
        return new ShapeTreeValidationResponse(false, true, null);
    }

    public static ShapeTreeValidationResponse passThroughResponse(ValidationResult validationResult) {
        return new ShapeTreeValidationResponse(false, true, validationResult);
    }

}
