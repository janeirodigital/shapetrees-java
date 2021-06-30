package com.janeirodigital.shapetrees.core.enums;

public enum LinkRelations {
    DESCRIBED_BY("describedby"),
    FOCUS_NODE("http://shapetrees.org/#FocusNode"),
    SHAPETREE_LOCATOR("http://www.w3.org/ns/shapetrees#ShapeTreeLocator"),
    PLANT_SHAPETREE("http://www.w3.org/ns/shapetrees#PlantShapeTree"),
    PLANT_SHAPETREE_HIERARCHY("http://www.w3.org/ns/shapetrees#PlantShapeTreeHierarchy"),
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
