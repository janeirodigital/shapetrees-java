package com.janeirodigital.shapetrees.enums;

public enum Namespaces {
    SHAPETREE_NAMESPACE("http://www.w3.org/ns/shapetree#");

    private String value;

    public String getValue() {
        return this.value;
    }

    Namespaces(String value) {
        this.value = value;
    }
}
