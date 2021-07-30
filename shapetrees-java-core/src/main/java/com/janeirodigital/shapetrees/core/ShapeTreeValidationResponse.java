package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.ValidationResult;
import lombok.Getter;
import lombok.Setter;

@Getter @Setter
public class ShapeTreeValidationResponse extends ShapeTreeResponse {
    private boolean requestFulfilled;
    private boolean validRequest;
    private ValidationResult validationResult;

    public ShapeTreeValidationResponse() {
        super();
    }

    public ShapeTreeValidationResponse(ValidationResult validationResult) {

        super();
        this.requestFulfilled = false;
        this.validRequest = true;
        this.validationResult = validationResult;
        if (!validationResult.isValid()) {
            this.validRequest = false;
            this.statusCode = 422;
            this.body = this.validationResult.getMessage();
        }

    }

    public ShapeTreeValidationResponse(Boolean validRequest, Boolean requestFulfilled) {
        this(validRequest, requestFulfilled, 201, "OK");
    }

    public ShapeTreeValidationResponse(Boolean validRequest, Boolean requestFulfilled, int statusCode, String body) {

        super();
        this.statusCode = statusCode;
        this.body = body;
        this.requestFulfilled = requestFulfilled;
        this.validRequest = validRequest;
        this.validationResult = null;

    }


    public ShapeTreeValidationResponse(ShapeTreeException ste) {
        super();
        this.validRequest = false;
        this.requestFulfilled = false;
        this.statusCode = ste.getStatusCode();
        this.body = ste.getMessage();
    }

    public static ShapeTreeValidationResponse passThroughResponse() {
        return passThroughResponse(null);
    }

    public static ShapeTreeValidationResponse passThroughResponse(ValidationResult validationResult) {
        ShapeTreeValidationResponse response = new ShapeTreeValidationResponse();
        response.setValidRequest(true);
        response.setRequestFulfilled(false);
        response.setValidationResult(validationResult);
        return response;
    }

}
