package com.janeirodigital.shapetrees.model;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter @NoArgsConstructor @AllArgsConstructor
public class ShapeTreeContext {
    private String authorizationHeaderValue;
    private String originatorIRI;
    private String webID;
}
