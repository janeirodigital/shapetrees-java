package com.janeirodigital.shapetrees.okhttp;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import okhttp3.OkHttpClient;

/**
 * Factory to provide OkHttpClient instances where shape tree validation is enabled. Clients
 * are sourced from the {@link OkHttpClientFactory} configured by
 * {@link OkHttpClientFactoryManager}.
 * @see <a href="https://square.github.io/okhttp/3.x/okhttp/okhttp3/OkHttpClient.html">OkHttp - OkHttpClient</a>
 */
public class OkHttpValidatingClientFactory {

    private OkHttpValidatingClientFactory() { }

    /**
     * Customized the basic OkHttpClient received from the configured {@link OkHttpClientFactory}
     * with an interceptor to provide client-side shape tree validation. The returned client is a
     * derivative of the one received by {@link OkHttpClientFactory}, which reuses the same
     * connection pool, and thread pools, and adds shape tree validation to the existing
     * configuration.
     * @return Validating OkHttpClient
     * @throws ShapeTreeException
     */
    public static OkHttpClient get() throws ShapeTreeException {
        OkHttpClient okHttpClient = OkHttpClientFactoryManager.getFactory().getOkHttpClient();
        return okHttpClient.newBuilder().addInterceptor(new OkHttpValidatingShapeTreeInterceptor()).build();
    }

}
