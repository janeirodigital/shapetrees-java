package com.janeirodigital.shapetrees.client.http;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;

public interface HttpClientFactory {
    public HttpClient getForConfig(ShapeTreeClientConfiguration configuration) throws ShapeTreeException;
}
