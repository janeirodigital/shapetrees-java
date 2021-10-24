package com.janeirodigital.shapetrees.core.contentloaders;

import com.janeirodigital.shapetrees.core.DocumentResponse;
import com.janeirodigital.shapetrees.core.ResourceAttributes;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;

import java.io.IOException;
import java.net.URL;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;

/**
 * Simple HTTP implementation of ExternalDocumentLoader provided as an example
 * as well as for its utility in unit tests.
 */
public class HttpExternalDocumentLoader implements ExternalDocumentLoader {

    private final HttpClient httpClient = HttpClient.newBuilder().followRedirects(HttpClient.Redirect.NEVER).build();

    @Override
    public DocumentResponse loadExternalDocument(URL resourceURL) throws ShapeTreeException {

        try {

            HttpRequest request = HttpRequest.newBuilder().GET().uri(resourceURL).build();
            HttpResponse<String> response = this.httpClient.send(request, HttpResponse.BodyHandlers.ofString());

            if (response.statusCode() != 200) {
                throw new IOException("Failed to load contents of document: " + resourceURL);
            }

            ResourceAttributes attributes = new ResourceAttributes(response.headers().map());

            return new DocumentResponse(attributes, response.body(), response.statusCode());

        } catch (IOException ex) {
            throw new ShapeTreeException(500, "Error retrieving resource " + ex.getMessage());
        } catch (InterruptedException ex) {
            Thread.currentThread().interrupt();
            throw new ShapeTreeException(500, "Error retrieving resource " + ex.getMessage());
        }

    }

}
