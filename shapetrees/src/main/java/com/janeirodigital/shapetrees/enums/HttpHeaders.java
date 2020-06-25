package com.janeirodigital.shapetrees.enums;

public enum HttpHeaders {
    LINK("Link"),
    CONTENT_TYPE("Content-Type"),
    SLUG("Slug");

    public String getValue() {
        return this.value;
    }

    private String value;

    HttpHeaders(String value) {
        this.value = value;
    }
}
