package com.janeirodigital.shapetrees.okhttp;

import okhttp3.OkHttpClient;

/**
 * Basic factory to get a OkHttp HTTP client, implementing {@link OkHttpClientFactory}.
 * This is the default client factory provided by {@link OkHttpClientFactoryManager} when a
 * more robust client factory is not assigned.
 * @see <a href="https://square.github.io/okhttp/">OkHttp</a> - HTTP client library from Square
 */
public class OkHttpBasicClientFactory implements OkHttpClientFactory {

    private final OkHttpClient okHttpClient;

    public OkHttpBasicClientFactory() {
        this.okHttpClient = new OkHttpClient();
    }

    /**
     * Get an OkHttpClient without any special configuration or settings
     * @return Basic OkHttpClient
     */
    @Override
    public OkHttpClient getOkHttpClient() {
        return this.okHttpClient;
    }

}
