package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.Optional;

@Getter @AllArgsConstructor
public class DocumentResponse {
    private final ResourceAttributes resourceAttributes;
    private final String body;
    private final int statusCode;

    public Optional<String> getContentType() {
        return resourceAttributes.firstValue(HttpHeaders.CONTENT_TYPE.getValue());
    }
}
