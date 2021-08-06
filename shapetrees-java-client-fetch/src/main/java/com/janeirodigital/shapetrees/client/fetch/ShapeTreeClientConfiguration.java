package com.janeirodigital.shapetrees.client.fetch;

import lombok.*;

/**
 * Defines a permutation of OkHttpClient configuration
 */
@Getter @Setter @EqualsAndHashCode @AllArgsConstructor @NoArgsConstructor
public class ShapeTreeClientConfiguration {
    private Boolean useValidation;
    private Boolean skipSslValidation;
}
