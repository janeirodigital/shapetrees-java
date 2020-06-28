package com.janeirodigital.shapetrees.model;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.List;

@Getter @AllArgsConstructor
public class ValidationResult {
    private final Boolean valid;
    private final List<String> nonConformantNodes;
}
