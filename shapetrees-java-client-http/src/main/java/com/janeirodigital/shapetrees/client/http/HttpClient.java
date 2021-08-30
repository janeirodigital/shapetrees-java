package com.janeirodigital.shapetrees.client.http;

import com.janeirodigital.shapetrees.core.HttpHeaders;
import com.janeirodigital.shapetrees.core.ShapeTreeResource;
import com.janeirodigital.shapetrees.core.ShapeTreeResponse;
import com.janeirodigital.shapetrees.core.enums.LinkRelations;
import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.vocabularies.LdpVocabulary;

import java.io.IOException;
import java.util.List;
import java.util.Set;

public abstract class HttpClient {
    protected static final String GET = "GET";
    protected static final String PUT = "PUT";
    protected static final String POST = "POST";
    protected static final String PATCH = "PATCH";
    protected static final String DELETE = "DELETE";

    protected static final Set<String> supportedRDFContentTypes = Set.of("text/turtle", "application/rdf+xml", "application/n-triples", "application/ld+json");

    public abstract ShapeTreeResource fetchShapeTreeResource(HttpRequest request) throws ShapeTreeException;

    public abstract ShapeTreeResponse fetchShapeTreeResponse(HttpRequest request) throws ShapeTreeException;

    public abstract void fetchIntoRemoteResource(HttpRequest response, HttpRemoteResource remoteResource) throws IOException;

    // header helpers
    protected static boolean isContainerFromHeaders(HttpHeaders requestHeaders) {

        List<String> linkHeaders = requestHeaders.get(com.janeirodigital.shapetrees.core.enums.HttpHeaders.LINK.getValue());

        if (linkHeaders == null) { return false; }

        HttpHeaders parsedLinkHeaders = HttpHeaders.parseLinkHeaders(linkHeaders);

        if (parsedLinkHeaders.get(LinkRelations.TYPE.getValue()) != null) {
            return parsedLinkHeaders.get(LinkRelations.TYPE.getValue()).contains(LdpVocabulary.CONTAINER) ||
                    parsedLinkHeaders.get(LinkRelations.TYPE.getValue()).contains(LdpVocabulary.BASIC_CONTAINER);
        }
        return false;
    }

    protected static ShapeTreeResourceType getResourceTypeFromHeaders(HttpHeaders requestHeaders) {

        List<String> linkHeaders = requestHeaders.get(com.janeirodigital.shapetrees.core.enums.HttpHeaders.LINK.getValue());

        if (linkHeaders == null) { return null; }

        HttpHeaders parsedLinkHeaders = HttpHeaders.parseLinkHeaders(linkHeaders);

        if (parsedLinkHeaders.get(LinkRelations.TYPE.getValue()) != null &&
           (parsedLinkHeaders.get(LinkRelations.TYPE.getValue()).contains(LdpVocabulary.CONTAINER) ||
            parsedLinkHeaders.get(LinkRelations.TYPE.getValue()).contains(LdpVocabulary.BASIC_CONTAINER))) {
            return ShapeTreeResourceType.CONTAINER;
        }

        if (requestHeaders.get(com.janeirodigital.shapetrees.core.enums.HttpHeaders.CONTENT_TYPE.getValue()) != null &&
            supportedRDFContentTypes.contains(requestHeaders.get(com.janeirodigital.shapetrees.core.enums.HttpHeaders.CONTENT_TYPE.getValue()).get(0))) {
            return ShapeTreeResourceType.RESOURCE;
        }
        return ShapeTreeResourceType.NON_RDF;
    }
}
