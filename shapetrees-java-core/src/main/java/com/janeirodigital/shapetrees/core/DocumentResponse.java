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
        return this.resourceAttributes.firstValue(HttpHeaders.CONTENT_TYPE.getValue());
    }

    // TODO: lots of choices re non-404, not >= 4xx, not 3xx. not 201 (meaning there's no body)
    public boolean exists() {
        return this.statusCode / 100 == 2;
    }
}
