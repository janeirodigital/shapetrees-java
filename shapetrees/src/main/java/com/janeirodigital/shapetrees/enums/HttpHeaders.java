package com.janeirodigital.shapetrees.enums;

public enum HttpHeaders {
    ACCEPT("Accept"),
    AUTHORIZATION("Authorization"),
    CONTENT_TYPE("Content-Type"),
    LINK("Link"),
    LOCATION("Location"),
    SLUG("Slug");

    public String getValue() {
        return this.value;
    }

    private String value;

    HttpHeaders(String value) {
        this.value = value;
    }
}
