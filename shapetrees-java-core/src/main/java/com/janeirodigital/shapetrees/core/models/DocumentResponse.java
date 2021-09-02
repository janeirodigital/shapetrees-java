package com.janeirodigital.shapetrees.core.models;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.net.URI;

@Getter @AllArgsConstructor
public class DocumentResponse {
    private final URI uri;
    private final String body;
    private final String contentType;
}
