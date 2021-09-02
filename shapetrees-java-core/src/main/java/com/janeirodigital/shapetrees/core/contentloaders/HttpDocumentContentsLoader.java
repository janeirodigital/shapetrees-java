package com.janeirodigital.shapetrees.core.contentloaders;

import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.DocumentResponse;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.util.Set;

/**
 * An implementation of ExternalDocumentLoader that retrieves resources using
 * the JDK11 HttpClient with a slight wrinkle of checking against a white/black
 * list to have tighter control of where resources are retrieved from.
 */
public class HttpDocumentContentsLoader implements ExternalDocumentLoader {

    private final HttpClient httpClient = HttpClient.newBuilder().followRedirects(HttpClient.Redirect.NEVER).build();
    private final Set<String> whiteListDomains;
    private final Set<String> blackListDomains;

    public HttpDocumentContentsLoader(Set<String> whiteListDomains, Set<String> blackListDomains) {
        this.whiteListDomains = whiteListDomains;
        this.blackListDomains = blackListDomains;
    }

    @Override
    public DocumentResponse loadExternalDocument(URI resourceURI) throws ShapeTreeException {
        if (blackListDomains != null && blackListDomains.contains(resourceURI.getHost())) {
            throw new ShapeTreeException(426, "Provided URI is on the configured black-list");
        }

        if (whiteListDomains != null && !whiteListDomains.contains(resourceURI.getHost())) {
            throw new ShapeTreeException(426, "Provided URI is NOT on the configured white-list");
        }

        try {
            HttpRequest request = HttpRequest.newBuilder().GET().uri(resourceURI).build();
            HttpResponse<String> response = this.httpClient.send(request, HttpResponse.BodyHandlers.ofString());

            if (response.statusCode() != 200) { throw new IOException("Failed to load contents of document: " + resourceURI); }

            return new DocumentResponse(resourceURI, response.body(), response.headers().firstValue(HttpHeaders.CONTENT_TYPE.getValue()).orElse("text/turtle"));
        } catch (IOException | InterruptedException ex) {
            throw new ShapeTreeException(500, "Error retrieving resource " + ex.getMessage());
        }
    }
}
