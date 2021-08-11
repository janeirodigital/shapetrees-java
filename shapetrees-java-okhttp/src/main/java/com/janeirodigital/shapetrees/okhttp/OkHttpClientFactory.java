package com.janeirodigital.shapetrees.okhttp;

import com.janeirodigital.shapetrees.client.http.HttpClientFactory;
import com.janeirodigital.shapetrees.client.http.HttpShapeTreeClientConfiguration;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;

import java.util.concurrent.ConcurrentHashMap;

public class OkHttpClientFactory implements HttpClientFactory {
    // Map from configuration to already-instantiated OkHttpClient.
    private static final ConcurrentHashMap<HttpShapeTreeClientConfiguration, OkHttpClient> clientMap = new ConcurrentHashMap<>();

    public OkHttpClient getForConfig(HttpShapeTreeClientConfiguration configuration) throws ShapeTreeException {
        if (clientMap.containsKey(configuration)) {
            return clientMap.get(configuration);
        }
        try {
            OkHttpClient client = new OkHttpClient(configuration);
            clientMap.put(configuration, client);
            return client;
        } catch (Exception ex) {
            throw new ShapeTreeException(500, ex.getMessage());
        }
    }
}

