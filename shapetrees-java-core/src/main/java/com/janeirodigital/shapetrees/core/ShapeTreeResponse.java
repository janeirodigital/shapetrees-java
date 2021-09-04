package com.janeirodigital.shapetrees.core;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.net.URI;

@Getter @AllArgsConstructor
public class ShapeTreeResponse {

    private URI uri;
    private ResourceAttributes resourceAttributes;
    protected String body;
    protected int statusCode;

    public boolean exists() {
        return this.statusCode != 404;
    }
}