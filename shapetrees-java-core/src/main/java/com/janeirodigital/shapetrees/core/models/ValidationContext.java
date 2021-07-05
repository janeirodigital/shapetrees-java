package com.janeirodigital.shapetrees.core.models;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter
@AllArgsConstructor @NoArgsConstructor
public class ValidationContext {
    private ShapeTree validatingShapeTree;
    private ValidationResult validationResult;
    private ShapeTreeLocation managingLocation;
}
