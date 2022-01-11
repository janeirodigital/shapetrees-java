package com.janeirodigital.shapetrees.client.okhttp;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import okhttp3.OkHttpClient;

/**
 * Constructs OkHttpClients for use by shapetrees-java-client-okhttp
 */
public interface OkHttpClientFactory {
        /**
         * Reuses or constructs a new regular OkHttpClient
         * @return OkHttpClient
         */
        OkHttpClient getOkHttpClient() throws ShapeTreeException;
}
