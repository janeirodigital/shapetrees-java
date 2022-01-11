package com.janeirodigital.shapetrees.core.validation;

import lombok.Getter;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

public class ContainingValidationResult {

    @Getter
    private boolean valid;
    @Getter
    private String message;
    private final Map<ShapeTreeAssignment, ValidationResult> containingResults;

    public ContainingValidationResult() {
        this.containingResults = new HashMap<>();
        this.valid = true;
        this.message = "";
    }

    public ContainingValidationResult(boolean valid, String message) {
        this();
        this.valid = valid;
        this.message = message;
    }

    public void add(ShapeTreeAssignment assignment, ValidationResult result) {
        this.containingResults.put(assignment, result);
        if (!result.isValid()) { this.valid = false; }
    }

    public Collection<ValidationResult> getResults() {
        return this.containingResults.values();
    }

    public Collection<ShapeTreeAssignment> getAssignments() {
        return this.containingResults.keySet();
    }

    public Set<Map.Entry<ShapeTreeAssignment, ValidationResult>> getEntries() {
        return this.containingResults.entrySet();
    }

    public boolean isValid() {
        for (var containingResult : this.containingResults.entrySet()) {
            if (!containingResult.getValue().isValid()) { return false; }
        }
        return true;
    }

}
