package com.janeirodigital.shapetrees.core.models;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.net.URL;

@Getter
@AllArgsConstructor
public class ValidationResult {
    private Boolean valid;
    private ShapeTree validatingShapeTree;
    private ShapeTree matchingShapeTree;
    private ShapeTreeLocation managingLocation;
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
        this.managingLocation = null;
        this.matchingFocusNode = null;
    }

    public ValidationResult(Boolean valid, ShapeTree validatingShapeTree) {
        this.valid = valid;
        this.message = null;
        this.validatingShapeTree = validatingShapeTree;
        this.matchingShapeTree = null;
        this.managingLocation = null;
        this.matchingFocusNode = null;
    }

    public ValidationResult(Boolean valid, ShapeTree validatingShapeTree, String message) {
        this.valid = valid;
        this.message = message;
        this.validatingShapeTree = validatingShapeTree;
        this.matchingShapeTree = null;
        this.managingLocation = null;
        this.matchingFocusNode = null;
    }

    public ValidationResult(Boolean valid, ShapeTree validatingShapeTree, URL matchingFocusNode) {
        this.valid = valid;
        this.message = null;
        this.validatingShapeTree = validatingShapeTree;
        this.matchingShapeTree = null;
        this.managingLocation = null;
        this.matchingFocusNode = matchingFocusNode;
    }

    public ValidationResult(Boolean valid, ShapeTree validatingShapeTree, ShapeTree matchingShapeTree, URL matchingFocusNode) {
        this.valid = valid;
        this.message = null;
        this.validatingShapeTree = validatingShapeTree;
        this.matchingShapeTree = matchingShapeTree;
        this.managingLocation = null;
        this.matchingFocusNode = matchingFocusNode;
    }

}
