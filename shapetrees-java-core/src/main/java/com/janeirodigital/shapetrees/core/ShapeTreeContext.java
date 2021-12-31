package com.janeirodigital.shapetrees.core;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter @AllArgsConstructor
public class ShapeTreeContext {
    private final String credentials;

    public boolean hasCredentials() {
        return credentials != null;
    }
}
