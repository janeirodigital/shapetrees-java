package com.janeirodigital.shapetrees.core.models;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter @AllArgsConstructor
public class ValidationResult {
    private final Boolean valid;
    //private final List<String> nonConformingNodes;
}
