package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.enums.HttpHeader;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.Optional;

@Getter @AllArgsConstructor
public class DocumentResponse {
    private final ResourceAttributes resourceAttributes;
    private final String body;
    private final int statusCode;

    public Optional<String> getContentType() {
        return this.resourceAttributes.firstValue(HttpHeader.CONTENT_TYPE.getValue());
    }

    // TODO: lots of choices re non-404, not >= 4xx, not 3xx. not 201 (meaning there's no body)
    public boolean isExists() {
        return this.statusCode / 100 == 2;
    }
}
