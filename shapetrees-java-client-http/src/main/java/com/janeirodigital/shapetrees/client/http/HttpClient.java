package com.janeirodigital.shapetrees.client.http;

import com.janeirodigital.shapetrees.core.ResourceAttributes;
import com.janeirodigital.shapetrees.core.ShapeTreeResource;
import com.janeirodigital.shapetrees.core.DocumentResponse;
import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.core.enums.LinkRelations;
import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.vocabularies.LdpVocabulary;

import java.io.IOException;
import java.util.List;
import java.util.Set;

/**
 * abstract base class for ShapeTree library network drivers
 */
public abstract class HttpClient {
    protected static final String GET = "GET";
    protected static final String PUT = "PUT";
    protected static final String POST = "POST";
    protected static final String PATCH = "PATCH";
    protected static final String DELETE = "DELETE";

    /**
     * Execute an HTTP request to create a DocumentResponse object
     * Implements `HttpClient` interface
     * @param request an HTTP request with appropriate headers for ShapeTree interactions
     * @return new DocumentResponse with response headers and contents
     * @throws ShapeTreeException
     */
    public abstract DocumentResponse fetchShapeTreeResponse(HttpRequest request) throws ShapeTreeException;

    /**
     * Execute an HTTP request and store the results in the passed HttpRemoteResource
     * @param request to execute
     * @param remoteResource to be updated
     * @throws IOException if HTTP request fails
     */
    public abstract void fetchIntoRemoteResource(HttpRequest request, HttpRemoteResource remoteResource) throws IOException;
}
