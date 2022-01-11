package com.janeirodigital.shapetrees.core.validation;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter @AllArgsConstructor
public class ShapeTreeContext {
    private final String credentials;

    public boolean hasCredentials() {
        return credentials != null;
    }
}
