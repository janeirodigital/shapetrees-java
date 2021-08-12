package com.janeirodigital.shapetrees.okhttp;

import com.janeirodigital.shapetrees.client.http.HttpClientFactory;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;

import java.util.concurrent.ConcurrentHashMap;

public class OkHttpClientFactory implements HttpClientFactory {
    boolean useSslValidation;

    OkHttpClientFactory(boolean useSslValidation) {
        this.useSslValidation = useSslValidation;
    }

    // Array of instantiatable OkHttpClients
    final int NON_VALIDATING = 0;
    final int VALIDATING = 1;
    private static final OkHttpClient okHttpClients[][] = {{null, null}, {null, null}};

    public OkHttpClient get(boolean useShapeTreeValidation) throws ShapeTreeException {
        int ssl = useSslValidation ? VALIDATING : NON_VALIDATING;
        int shapes = useShapeTreeValidation ? VALIDATING : NON_VALIDATING;

        if (okHttpClients[ssl][shapes] != null) {
            return okHttpClients[ssl][shapes];
        }
        try {
            OkHttpClient client = new OkHttpClient(useSslValidation, useShapeTreeValidation);
            okHttpClients[ssl][shapes] = client;
            return client;
        } catch (Exception ex) {
            throw new ShapeTreeException(500, ex.getMessage());
        }
    }
}

