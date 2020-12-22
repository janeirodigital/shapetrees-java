package com.janeirodigital.shapetrees.core.models;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.List;

@Getter @AllArgsConstructor
public class ValidationContext {
    private final ShapeTree validatingShapeTree;
    private final ValidationResult validationResult;
    private final List<ShapeTreeLocator> parentContainerLocators;
}
