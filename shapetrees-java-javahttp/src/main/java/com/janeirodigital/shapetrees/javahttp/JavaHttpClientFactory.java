package com.janeirodigital.shapetrees.javahttp;

import com.janeirodigital.shapetrees.client.http.HttpClientFactory;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;

public class JavaHttpClientFactory implements HttpClientFactory {
    boolean useSslValidation;

    JavaHttpClientFactory(boolean useSslValidation) {
        this.useSslValidation = useSslValidation;
    }

    public JavaHttpClient get(boolean useShapeTreeValidation) throws ShapeTreeException {
        try {
            return new JavaHttpClient(useSslValidation, useShapeTreeValidation);
        } catch (Exception ex) {
            throw new ShapeTreeException(500, ex.getMessage());
        }
    }
}

