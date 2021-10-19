package com.janeirodigital.shapetrees.core.models;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.jetbrains.annotations.NotNull;

import java.util.Optional;

@Getter @Setter @NoArgsConstructor @AllArgsConstructor
public class ShapeTreeContext {
    private @NotNull
    Optional<String> authorizationHeaderValue = Optional.empty();
}
