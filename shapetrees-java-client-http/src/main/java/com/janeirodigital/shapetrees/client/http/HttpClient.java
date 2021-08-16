package com.janeirodigital.shapetrees.client.http;

import com.janeirodigital.shapetrees.core.ShapeTreeResource;
import com.janeirodigital.shapetrees.core.ShapeTreeResponse;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;

import java.io.IOException;
import java.net.URI;
import java.util.List;
import java.util.Map;

public interface HttpClient {
    public ShapeTreeResource fetchShapeTreeResource(String method, URI resourceURI, Map<String, List<String>> headers,
                                                    String authorizationHeaderValue, String body, String contentType) throws ShapeTreeException;

    public ShapeTreeResponse fetchShapeTreeResponse(String method, URI resourceURI, Map<String, List<String>> headers,
                                                    String authorizationHeaderValue, String body, String contentType) throws ShapeTreeException;

    public void fetchIntoRemoteResource(String method, URI resourceURI, Map<String, List<String>> headers,
                                        String authorizationHeaderValue, String body, String contentType, HttpRemoteResource remoteResource) throws IOException;

}
