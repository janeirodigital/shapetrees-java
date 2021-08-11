package com.janeirodigital.shapetrees.client.http;

import lombok.*;

/**
 * Defines a permutation of OkHttpClient configuration
 */
@Getter @Setter @EqualsAndHashCode @AllArgsConstructor @NoArgsConstructor
public class HttpShapeTreeClientConfiguration {
    private Boolean useValidation;
    private Boolean skipSslValidation;
}
