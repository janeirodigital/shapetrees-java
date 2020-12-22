package com.janeirodigital.shapetrees.client.core;

import lombok.*;

@Getter @Setter @EqualsAndHashCode @AllArgsConstructor @NoArgsConstructor
public class ShapeTreeClientConfiguration {
    private Boolean useValidation;
    private Boolean skipSslValidation;
}
