package com.janeirodigital.shapetrees.javahttp;

import com.janeirodigital.shapetrees.core.contentloaders.BlackWhiteList;
import com.janeirodigital.shapetrees.core.contentloaders.ExternalDocumentLoader;
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
public class JavaHttpDocumentLoader implements ExternalDocumentLoader {

    private final HttpClient httpClient = HttpClient.newBuilder().followRedirects(HttpClient.Redirect.NEVER).build();
    private final BlackWhiteList blackWhiteList;

    public JavaHttpDocumentLoader(BlackWhiteList blackWhiteList) {
        this.blackWhiteList = blackWhiteList;
    }

    @Override
    public DocumentResponse loadExternalDocument(URI resourceURI) throws ShapeTreeException {
        blackWhiteList.check(resourceURI);

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
