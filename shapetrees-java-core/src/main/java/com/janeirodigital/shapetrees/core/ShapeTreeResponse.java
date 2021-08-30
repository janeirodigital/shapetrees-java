package com.janeirodigital.shapetrees.core;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

import java.util.ArrayList;

@Getter @Setter @AllArgsConstructor
public class ShapeTreeResponse {

    protected int statusCode;
    protected String body;
    protected HttpHeaders headers;

    public ShapeTreeResponse() {
        headers = new HttpHeaders();
    }

    public HttpHeaders getResponseHeaders() {
        return this.headers;
    }

    public void addResponseHeader(String header, String value) {
        this.headers.computeIfAbsent(header, k -> new ArrayList<>());
        this.headers.get(header).add(value);
    }

    public boolean exists() {
        return this.statusCode != 404;
    }
}