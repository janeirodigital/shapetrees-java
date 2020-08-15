package com.janeirodigital.shapetrees.model;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.List;

@Getter @AllArgsConstructor
public class ValidationContext {
    private ShapeTree validatingShapeTree;
    private ValidationResult validationResult;
    private List<ShapeTreeLocator> parentContainerLocators;
}
