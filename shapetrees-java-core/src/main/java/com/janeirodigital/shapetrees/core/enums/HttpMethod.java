package com.janeirodigital.shapetrees.core.enums;

/**
 * Enumerated list of HTTP methods
 */
public enum HttpMethod {
    POST("POST"),
    PUT("PUT"),
    PATCH("PATCH"),
    DELETE("DELETE"),
    GET("GET"),
    HEAD("HEAD"),
    OPTIONS("OPTIONS"),
    TRACE("TRACE"),
    CONNECT("CONNECT");

    public String getValue() {
        return this.value;
    }

    private final String value;

    HttpMethod(String value) {
        this.value = value;
    }
}
