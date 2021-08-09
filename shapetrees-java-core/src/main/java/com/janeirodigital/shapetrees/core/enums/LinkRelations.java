package com.janeirodigital.shapetrees.core.enums;

public enum LinkRelations {
    DESCRIBED_BY("describedby"),
    FOCUS_NODE("http://www.w3.org/ns/shapetrees#FocusNode"),
    SHAPETREE_LOCATOR("http://www.w3.org/ns/shapetrees#ShapeTreeLocator"),
    TARGET_SHAPETREE("http://www.w3.org/ns/shapetrees#TargetShapeTree"),
    TYPE("type"),
    ACL("acl");

    private final String value;

    public String getValue() {
        return this.value;
    }

    LinkRelations(String value) {
        this.value = value;
    }
}
