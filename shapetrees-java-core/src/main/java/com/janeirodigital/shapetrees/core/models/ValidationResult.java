package com.janeirodigital.shapetrees.core.models;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.net.URI;

@Getter @Setter
@NoArgsConstructor @AllArgsConstructor
public class ValidationResult {
    private Boolean valid;
    private ShapeTree validatingShapeTree;
    private ShapeTree matchingShapeTree;
    private ShapeTreeLocation managingLocation;
    private URI matchingFocusNode;
    private String message;

    public Boolean isValid() {
        if (this.valid != null && this.valid == true) {
            return true;
        }
        return false;
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

    public ValidationResult(Boolean valid, ShapeTree validatingShapeTree, URI matchingFocusNode) {
        this.valid = valid;
        this.message = null;
        this.validatingShapeTree = validatingShapeTree;
        this.matchingShapeTree = null;
        this.managingLocation = null;
        this.matchingFocusNode = matchingFocusNode;
    }

    public ValidationResult(Boolean valid, ShapeTree validatingShapeTree, ShapeTree matchingShapeTree, URI matchingFocusNode) {
        this.valid = valid;
        this.message = null;
        this.validatingShapeTree = validatingShapeTree;
        this.matchingShapeTree = matchingShapeTree;
        this.managingLocation = null;
        this.matchingFocusNode = matchingFocusNode;
    }

}