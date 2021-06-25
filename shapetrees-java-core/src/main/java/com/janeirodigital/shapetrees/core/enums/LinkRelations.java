package com.janeirodigital.shapetrees.core.enums;

public enum LinkRelations {
    DESCRIBED_BY("describedby"),
    FOCUS_NODE("http://shapetrees.org/#FocusNode"),
    SHAPETREE("http://shapetrees.org/#ShapeTreeLocator"),
    TARGET_SHAPETREE("http://shapetrees.org/#TargetShapeTree"),
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
