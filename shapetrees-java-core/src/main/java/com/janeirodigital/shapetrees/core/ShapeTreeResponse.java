package com.janeirodigital.shapetrees.core;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

@Getter @Setter @AllArgsConstructor
public class ShapeTreeResponse {

    protected int statusCode;
    protected String body;
    protected ResourceAttributes headers;

    public ShapeTreeResponse() {
        headers = new ResourceAttributes();
    }

    public ResourceAttributes getResponseHeaders() {
        return this.headers;
    }

    public boolean exists() {
        return this.statusCode != 404;
    }
}