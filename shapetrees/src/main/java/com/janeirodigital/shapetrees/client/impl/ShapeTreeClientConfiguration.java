package com.janeirodigital.shapetrees.client.impl;

import com.janeirodigital.shapetrees.ShapeTreeEcosystem;
import lombok.*;

@Getter @Setter @EqualsAndHashCode @AllArgsConstructor @NoArgsConstructor
public class ShapeTreeClientConfiguration {
    private ShapeTreeEcosystem ecosystem;
    private Boolean useValidation;
    private Boolean skipSslValidation;

    public Boolean isValid() {
        if (useValidation && ecosystem == null) {
            return false;
        }
        return true;
    }
}
