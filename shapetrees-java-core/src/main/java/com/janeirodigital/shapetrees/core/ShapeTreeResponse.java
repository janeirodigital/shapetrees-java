package com.janeirodigital.shapetrees.core;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Getter @Setter @AllArgsConstructor
public class ShapeTreeResponse {

    protected int statusCode;
    protected String body;
    protected Map<String, List<String>> headers;

    public ShapeTreeResponse() {
        headers = new HashMap<>();
    }

    public Map<String, List<String>> getResponseHeaders() {
        return this.headers;
    }

    public void addResponseHeader(String header, String value) {
        if (!this.headers.containsKey(header)) {
            this.headers.put(header, new ArrayList<>());
        }

        this.headers.get(header).add(value);
    }

    public boolean exists() {
        return this.statusCode != 404;
    }
}
