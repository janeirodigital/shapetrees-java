package com.janeirodigital.shapetrees.core.models;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.net.URI;

@Getter @NoArgsConstructor @AllArgsConstructor
public class ValidationResult {
    private Boolean valid;
    private URI matchingNode;
    private String message;

    public Boolean isValid() {
        if (this.valid != null && this.valid == true) {
            return true;
        }
        return false;
    }

}