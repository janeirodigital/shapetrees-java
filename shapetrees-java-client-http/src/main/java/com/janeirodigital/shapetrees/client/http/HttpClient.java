package com.janeirodigital.shapetrees.client.http;

import com.janeirodigital.shapetrees.core.HttpClientHeaders;
import com.janeirodigital.shapetrees.core.ShapeTreeResource;
import com.janeirodigital.shapetrees.core.ShapeTreeResponse;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;

import java.io.IOException;
import java.net.URI;

public abstract class HttpClient {
    public abstract ShapeTreeResource fetchShapeTreeResource(String method, URI resourceURI, HttpClientHeaders headers,
                                                             String body, String contentType) throws ShapeTreeException;

    public abstract ShapeTreeResponse fetchShapeTreeResponse(HttpRequest request) throws ShapeTreeException;

    public abstract void fetchIntoRemoteResource(String method, URI resourceURI, HttpClientHeaders headers,
                                                 String body, String contentType, HttpRemoteResource remoteResource) throws IOException;

}
