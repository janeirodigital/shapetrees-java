package com.janeirodigital.shapetrees.javahttp;

import com.janeirodigital.shapetrees.client.http.HttpClientFactory;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;

public class JavaHttpClientFactory implements HttpClientFactory {
    boolean useSslValidation;

    JavaHttpClientFactory(boolean useSslValidation) {
        this.useSslValidation = useSslValidation;
    }

    // Array of instantiatable OkHttpClients
    final int NON_VALIDATING = 0;
    final int VALIDATING = 1;
    private static final JavaHttpClient okHttpClients[][] = {{null, null}, {null, null}};

    public JavaHttpClient get(boolean useShapeTreeValidation) throws ShapeTreeException {
        int ssl = useSslValidation ? VALIDATING : NON_VALIDATING;
        int shapes = useShapeTreeValidation ? VALIDATING : NON_VALIDATING;

        if (okHttpClients[ssl][shapes] != null) {
            return okHttpClients[ssl][shapes];
        }
        try {
            JavaHttpClient client = new JavaHttpClient(useSslValidation, useShapeTreeValidation);
            okHttpClients[ssl][shapes] = client;
            return client;
        } catch (Exception ex) {
            throw new ShapeTreeException(500, ex.getMessage());
        }
    }
}

