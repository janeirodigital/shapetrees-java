package com.janeirodigital.shapetrees.core.enums;

import com.janeirodigital.shapetrees.core.vocabularies.ShapeTreeVocabulary;

public enum ShapeTreeResourceType {
    CONTAINER(ShapeTreeVocabulary.CONTAINER),
    RESOURCE(ShapeTreeVocabulary.RESOURCE),
    NON_RDF(ShapeTreeVocabulary.NON_RDF_RESOURCE);

    private final String value;

    public String getValue() {
        return this.value;
    }

    ShapeTreeResourceType(String value) {
        this.value = value;
    }
}
