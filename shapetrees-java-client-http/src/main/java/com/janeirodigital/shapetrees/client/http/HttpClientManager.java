package com.janeirodigital.shapetrees.client.http;

import com.janeirodigital.shapetrees.core.ShapeTreeResource;
import com.janeirodigital.shapetrees.core.ShapeTreeResponse;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;

import java.io.IOException;
import java.net.URI;
import java.util.List;
import java.util.Map;


public class HttpClientManager {
    private static HttpClientFactory factory;

    public static HttpClientFactory getFactory() throws ShapeTreeException {
        if (factory == null) {
            throw new ShapeTreeException(500, "HttpClientFactory not set int HttpClientManager");
        }
        return factory;
    }
    public static void setFactory(HttpClientFactory factoryP) {
        factory = factoryP;
    }
}