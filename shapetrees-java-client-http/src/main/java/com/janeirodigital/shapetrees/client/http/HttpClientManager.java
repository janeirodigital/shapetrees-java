package com.janeirodigital.shapetrees.client.http;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;


public class HttpClientManager {

    private static HttpClientFactory factory;

    private HttpClientManager() {
        throw new IllegalStateException("Utility classes are not meant to be instantiated");
    }

    public static HttpClientFactory getFactory() throws ShapeTreeException {
        if (factory == null) {
            throw new ShapeTreeException(500, "HttpClientFactory not set int HttpClientManager");
        }
        return factory;
    }
    public static void setFactory(HttpClientFactory clientFactory) {
        factory = clientFactory;
    }
}