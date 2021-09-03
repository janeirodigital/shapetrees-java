package com.janeirodigital.shapetrees.core.models;

import com.janeirodigital.shapetrees.core.ResourceAttributes;
import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import lombok.Getter;

import java.net.URI;
import java.util.Optional;

@Getter
public class DocumentResponse {
    private final URI uri;
    private final ResourceAttributes resourceAttributes;
    private final String body;

    public DocumentResponse(URI uri, ResourceAttributes resourceAttributes, String body) {
        this.uri = uri;
        this.body = body;
        this.resourceAttributes = resourceAttributes;
    }

    public Optional<String> getContentType() {
        return resourceAttributes.firstValue(HttpHeaders.CONTENT_TYPE.getValue());
    }
}
