package com.janeirodigital.shapetrees.client.http;

import com.janeirodigital.shapetrees.core.DocumentResponse;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;

/**
 * abstract base class for ShapeTree library network drivers
 */
public interface HttpClient {
    String GET = "GET";
    String PUT = "PUT";
    String POST = "POST";
    String PATCH = "PATCH";
    String DELETE = "DELETE";

    /**
     * Execute an HTTP request to create a DocumentResponse object
     * Implements `HttpClient` interface
     * @param request an HTTP request with appropriate headers for ShapeTree interactions
     * @return new DocumentResponse with response headers and contents
     * @throws ShapeTreeException
     */
    DocumentResponse fetchShapeTreeResponse(ShapeTreeHttpRequest request) throws ShapeTreeException;

}
