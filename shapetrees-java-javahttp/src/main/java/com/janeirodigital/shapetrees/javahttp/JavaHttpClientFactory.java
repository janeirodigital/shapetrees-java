package com.janeirodigital.shapetrees.javahttp;

import com.janeirodigital.shapetrees.client.http.HttpClientFactory;
import com.janeirodigital.shapetrees.client.http.HttpRequest;
import com.janeirodigital.shapetrees.core.ShapeTreeResponse;
import com.janeirodigital.shapetrees.core.contentloaders.BlackWhiteList;
import com.janeirodigital.shapetrees.core.contentloaders.ExternalDocumentLoader;
import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.DocumentResponse;

import java.net.URI;

public class JavaHttpClientFactory implements HttpClientFactory, ExternalDocumentLoader {
    boolean useSslValidation;
    private final BlackWhiteList blackWhiteList;

    JavaHttpClientFactory(boolean useSslValidation, BlackWhiteList blackWhiteList) {
        this.useSslValidation = useSslValidation;
        this.blackWhiteList = blackWhiteList;
    }

    public JavaHttpClient get(boolean useShapeTreeValidation) throws ShapeTreeException {
        try {
            return new JavaHttpClient(useSslValidation, useShapeTreeValidation);
        } catch (Exception ex) {
            throw new ShapeTreeException(500, ex.getMessage());
        }
    }

    @Override
    public DocumentResponse loadExternalDocument(URI resourceURI) throws ShapeTreeException {
        if (blackWhiteList != null) { blackWhiteList.check(resourceURI); }

        ShapeTreeResponse response = this.get(false).fetchShapeTreeResponse(new HttpRequest("GET", resourceURI, null, null, null));
        if (response.getStatusCode() != 200) { throw new ShapeTreeException(500, "Failed to load contents of document: " + resourceURI); }
        return new DocumentResponse(resourceURI, response.getBody(), response.getHeaders().firstValue(HttpHeaders.CONTENT_TYPE.getValue()).orElse("text/turtle"));
    }
}
