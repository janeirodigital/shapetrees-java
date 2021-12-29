package com.janeirodigital.shapetrees.core.enums;

public enum HttpHeader {
    ACCEPT("Accept"),
    AUTHORIZATION("Authorization"),
    CONTENT_TYPE("Content-Type"),
    LINK("Link"),
    LOCATION("Location"),
    SLUG("Slug"),
    INTEROP_ORIGINATOR("InteropOrigin"),
    INTEROP_WEBID("InteropWebID");

    public String getValue() {
        return this.value;
    }

    private final String value;

    HttpHeader(String value) {
        this.value = value;
    }
}
