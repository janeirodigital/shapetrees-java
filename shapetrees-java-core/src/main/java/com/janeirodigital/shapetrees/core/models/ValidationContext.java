package com.janeirodigital.shapetrees.core.models;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter @AllArgsConstructor
public class ValidationContext {
    private final ShapeTree validatingShapeTree;
    private final ValidationResult validationResult;
    private final ShapeTreeLocator parentContainerLocator;
}
