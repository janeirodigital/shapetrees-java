package com.janeirodigital.shapetrees.okhttp;

import com.janeirodigital.shapetrees.client.http.AbstractHttpClientFactory;
import com.janeirodigital.shapetrees.client.http.HttpClientFactory;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;

public class OkHttpClientFactory extends AbstractHttpClientFactory implements HttpClientFactory {
    boolean useSslValidation;
    static final int NON_VALIDATING = 0;
    static final int VALIDATING = 1;
    private static OkHttpClient[][] okHttpClients = {{null, null}, {null, null}};

    OkHttpClientFactory(boolean useSslValidation) {
        this.useSslValidation = useSslValidation;
    }

    public OkHttpClient get(boolean useClientShapeTreeValidation) throws ShapeTreeException {
        return getProtected(this.useSslValidation, useClientShapeTreeValidation);
    }

    private static synchronized OkHttpClient getProtected(boolean useSslValidation, boolean useClientShapeTreeValidation) throws ShapeTreeException {

        int ssl = useSslValidation ? VALIDATING : NON_VALIDATING;
        int shapes = useClientShapeTreeValidation ? VALIDATING : NON_VALIDATING;

        if (okHttpClients[ssl][shapes] != null) {
            return okHttpClients[ssl][shapes];
        }
        try {
            OkHttpClient client = new OkHttpClient(useSslValidation, useClientShapeTreeValidation);
            okHttpClients[ssl][shapes] = client;
            return client;
        } catch (Exception ex) {
            throw new ShapeTreeException(500, ex.getMessage());
        }

    }

}

