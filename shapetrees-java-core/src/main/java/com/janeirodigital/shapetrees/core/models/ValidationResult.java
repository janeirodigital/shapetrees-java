package com.janeirodigital.shapetrees.core.models;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.net.URL;

@Getter
@AllArgsConstructor
public class ValidationResult {
    private Boolean valid;
    private ShapeTree validatingShapeTree;
    private ShapeTree matchingShapeTree;
    private ShapeTreeAssignment managingAssignment;
    private URL matchingFocusNode;
    private String message;

    public Boolean isValid() {
        return (this.valid != null && this.valid);
    }

    public ValidationResult(Boolean valid, String message) {
        this.valid = valid;
        this.message = message;
        this.validatingShapeTree = null;
        this.matchingShapeTree = null;
        this.managingAssignment = null;
        this.matchingFocusNode = null;
    }

    public ValidationResult(Boolean valid, ShapeTree validatingShapeTree) {
        this.valid = valid;
        this.message = null;
        this.validatingShapeTree = validatingShapeTree;
        this.matchingShapeTree = null;
        this.managingAssignment = null;
        this.matchingFocusNode = null;
    }

    public ValidationResult(Boolean valid, ShapeTree validatingShapeTree, String message) {
        this.valid = valid;
        this.message = message;
        this.validatingShapeTree = validatingShapeTree;
        this.matchingShapeTree = null;
        this.managingAssignment = null;
        this.matchingFocusNode = null;
    }

    public ValidationResult(Boolean valid, ShapeTree validatingShapeTree, URL matchingFocusNode) {
        this.valid = valid;
        this.message = null;
        this.validatingShapeTree = validatingShapeTree;
        this.matchingShapeTree = null;
        this.managingAssignment = null;
        this.matchingFocusNode = matchingFocusNode;
    }

    public ValidationResult(Boolean valid, ShapeTree validatingShapeTree, ShapeTree matchingShapeTree, URL matchingFocusNode) {
        this.valid = valid;
        this.message = null;
        this.validatingShapeTree = validatingShapeTree;
        this.matchingShapeTree = matchingShapeTree;
        this.managingAssignment = null;
        this.matchingFocusNode = matchingFocusNode;
    }

}
