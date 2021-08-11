package com.janeirodigital.shapetrees.client.http;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;

public interface HttpClientFactory {
    public HttpClient getForConfig(HttpShapeTreeClientConfiguration configuration) throws ShapeTreeException;
}
