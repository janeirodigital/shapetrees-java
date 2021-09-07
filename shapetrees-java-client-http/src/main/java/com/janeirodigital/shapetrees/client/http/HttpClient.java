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

    protected static final Set<String> supportedRDFContentTypes = Set.of("text/turtle", "application/rdf+xml", "application/n-triples", "application/ld+json");

    /**
     * Execute an HTTP request to create a ShapeTreeResource object
     * Implements `HttpClient` interface
     * @param request an HTTP request with appropriate headers for ShapeTree interactions
     * @return new ShapeTreeResource with response headers and contents
     * @throws ShapeTreeException
     */
    public abstract ShapeTreeResource fetchShapeTreeResource(HttpRequest request) throws ShapeTreeException;

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

    /**
     * Look for a Link rel=type of ldp:Container or ldp:BasicContainer
     * @param headers to parse
     * @return
     */
    protected static boolean isContainerFromHeaders(ResourceAttributes headers) {

        List<String> linkHeaders = headers.allValues(HttpHeaders.LINK.getValue());

        if (linkHeaders == null) { return false; }

        ResourceAttributes parsedLinkHeaders = ResourceAttributes.parseLinkHeaders(linkHeaders);

        List<String> typeLinks = parsedLinkHeaders.allValues(LinkRelations.TYPE.getValue());
        if (typeLinks != null) {
            return typeLinks.contains(LdpVocabulary.CONTAINER) ||
                    typeLinks.contains(LdpVocabulary.BASIC_CONTAINER);
        }
        return false;
    }

    /**
     * Determine a resource type by parsing Link rel=type headers
     * @param headers to parse
     * @return
     */
    protected static ShapeTreeResourceType getResourceTypeFromHeaders(ResourceAttributes headers) {

        List<String> linkHeaders = headers.allValues(HttpHeaders.LINK.getValue());

        if (linkHeaders == null) { return null; }

        ResourceAttributes parsedLinkHeaders = ResourceAttributes.parseLinkHeaders(linkHeaders);

        List<String> typeLinks = parsedLinkHeaders.allValues(LinkRelations.TYPE.getValue());
        if (typeLinks != null &&
           (typeLinks.contains(LdpVocabulary.CONTAINER) ||
            typeLinks.contains(LdpVocabulary.BASIC_CONTAINER))) {
            return ShapeTreeResourceType.CONTAINER;
        }

        if (supportedRDFContentTypes.contains(headers.firstValue(HttpHeaders.CONTENT_TYPE.getValue()).orElse(null))) {
            return ShapeTreeResourceType.RESOURCE;
        }
        return ShapeTreeResourceType.NON_RDF;
    }
}
