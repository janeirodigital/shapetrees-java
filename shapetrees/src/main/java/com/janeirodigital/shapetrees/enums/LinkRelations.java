package com.janeirodigital.shapetrees.enums;

public enum LinkRelations {
    DESCRIBED_BY("describedby"),
    FOCUS_NODE("focusNode"),
    SHAPETREE("ShapeTree"),
    TYPE("type");


    private String value;

    public String getValue() {
        return this.value;
    }

    LinkRelations(String value) {
        this.value = value;
    }
}
