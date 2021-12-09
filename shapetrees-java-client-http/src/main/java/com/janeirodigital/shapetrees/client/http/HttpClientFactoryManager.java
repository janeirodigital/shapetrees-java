package com.janeirodigital.shapetrees.client.http;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import lombok.Setter;
import lombok.Synchronized;

public abstract class HttpClientFactoryManager {
    @Setter(onMethod_={@Synchronized})
    private static HttpClientFactory factory;

    private HttpClientFactoryManager() {
        throw new IllegalStateException("Utility class");
    }

    @Synchronized
    public static HttpClientFactory getFactory() throws ShapeTreeException {
        if (factory == null) { throw new ShapeTreeException(500, "Must provide a valid HTTP client factory"); }
        return HttpClientFactoryManager.factory;
    }
}
