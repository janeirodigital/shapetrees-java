package com.janeirodigital.shapetrees.okhttp;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import okhttp3.OkHttpClient;

/**
 * Constructs OkHttpClients that do or do not have client-side validation activated
 */
public interface OkHttpClientFactory {
        /**
         * Reuses or constructs a new regular OkHttpClient
         * @return OkHttpClient
         */
        OkHttpClient get() throws ShapeTreeException;
}
