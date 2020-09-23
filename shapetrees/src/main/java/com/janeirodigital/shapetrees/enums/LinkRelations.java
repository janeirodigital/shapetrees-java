package com.janeirodigital.shapetrees.enums;

public enum LinkRelations {
    DESCRIBED_BY("describedby"),
    FOCUS_NODE("http://shapetrees.org/#FocusNode"),
    SHAPETREE("http://shapetrees.org/#ShapeTree"),
    TARGET_SHAPETREE("http://shapetrees.org/#TargetShapeTree"),
    TYPE("type"),
    ACL("acl");



    private String value;

    public String getValue() {
        return this.value;
    }

    LinkRelations(String value) {
        this.value = value;
    }
}
