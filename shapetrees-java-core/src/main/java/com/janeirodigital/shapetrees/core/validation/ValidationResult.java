package com.janeirodigital.shapetrees.core.validation;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.net.URL;

@Getter
@AllArgsConstructor
public class ValidationResult {
    private boolean valid;
    private ShapeTree validatingShapeTree;
    private ShapeTree matchingShapeTree;   // TODO - consider renaming this to containingShapeTree
    private ShapeTreeAssignment managingAssignment;
    private URL matchingFocusNode;
    private String message;
    // TODO - consider adding status code here

    public ValidationResult(boolean valid, String message) {
        this.valid = valid;
        this.message = message;
        this.validatingShapeTree = null;
        this.matchingShapeTree = null;
        this.managingAssignment = null;
        this.matchingFocusNode = null;
    }

    public ValidationResult(boolean valid, ShapeTree validatingShapeTree, String message) {
        this.valid = valid;
        this.message = message;
        this.validatingShapeTree = validatingShapeTree;
        this.matchingShapeTree = null;
        this.managingAssignment = null;
        this.matchingFocusNode = null;
    }

    public ValidationResult(boolean valid, ShapeTree validatingShapeTree, ShapeTree matchingShapeTree, URL matchingFocusNode) {
        this.valid = valid;
        this.message = null;
        this.validatingShapeTree = validatingShapeTree;
        this.matchingShapeTree = matchingShapeTree;
        this.managingAssignment = null;
        this.matchingFocusNode = matchingFocusNode;
    }

}
