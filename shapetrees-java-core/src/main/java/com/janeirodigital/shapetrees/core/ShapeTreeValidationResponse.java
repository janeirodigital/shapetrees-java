package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.ValidationContext;
import lombok.Getter;
import lombok.Setter;

@Getter @Setter
public class ShapeTreeValidationResponse extends ShapeTreeResponse {
    private boolean requestFulfilled;
    private boolean validRequest;
    private ValidationContext validationContext;

    public ShapeTreeValidationResponse() {
        super();
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

    public static ShapeTreeValidationResponse passThroughResponse(ValidationContext validationContext) {
        ShapeTreeValidationResponse response = new ShapeTreeValidationResponse();
        response.setValidRequest(true);
        response.setRequestFulfilled(false);
        response.setValidationContext(validationContext);
        return response;
    }

}