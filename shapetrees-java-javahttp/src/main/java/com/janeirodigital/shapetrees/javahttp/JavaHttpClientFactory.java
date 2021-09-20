package com.janeirodigital.shapetrees.javahttp;

import com.janeirodigital.shapetrees.client.http.HttpClientFactory;
import com.janeirodigital.shapetrees.client.http.HttpRequest;
import com.janeirodigital.shapetrees.core.DocumentResponse;
import com.janeirodigital.shapetrees.core.contentloaders.BlackWhiteList;
import com.janeirodigital.shapetrees.core.contentloaders.ExternalDocumentLoader;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;

import java.net.URI;

/**
 * The ShapeTree library uses a generic interface (`HttpClient`) to execute HTTP queries on the POD and for external documents.
 * The JavaHttpClient uses the java.net.http library to implement `HttpClient`.
 * This factory generates variations of java.net.http those clients depending on the need for SSL validation and ShapeTree validation.
 */
public class JavaHttpClientFactory implements HttpClientFactory, ExternalDocumentLoader {
    boolean useSslValidation;
    private final BlackWhiteList blackWhiteList;

    /**
     * Construct a factory for JavaHttpClients
     *
     * @param useSslValidation
     * @param blackWhiteList
     */
    JavaHttpClientFactory(boolean useSslValidation, BlackWhiteList blackWhiteList) {
        this.useSslValidation = useSslValidation;
        this.blackWhiteList = blackWhiteList;
    }

    /**
     * Create a new java.net.http HttpClient.
     * This fulfils the HttpClientFactory interface, so this factory can be use in
     *   AbstractHttpClientFactory.setFactory(new JavaHttpClientFactory(...));
     *
     * @param useShapeTreeValidation
     * @return a new or existing java.net.http HttpClient
     * @throws ShapeTreeException if the JavaHttpClient constructor threw one
     */
    public JavaHttpClient get(boolean useShapeTreeValidation) throws ShapeTreeException {
        try {
            return new JavaHttpClient(this.useSslValidation, useShapeTreeValidation);
        } catch (Exception ex) {
            throw new ShapeTreeException(500, ex.getMessage());
        }
    }

    /**
     * Load a non-POD document
     * This fulfils the ExternalDocumentLoader interface, so this factory can be use in
     *   DocumentLoaderManager.setLoader(new JavaHttpClientFactory(...));
     *
     * @param resourceURI URI of resource to be retrieved
     * @return a DocumentResponse with the results of a successful GET
     * @throws ShapeTreeException if the GET was not successful
     */
    @Override
    public DocumentResponse loadExternalDocument(URI resourceURI) throws ShapeTreeException {
        if (this.blackWhiteList != null) {
            this.blackWhiteList.check(resourceURI); }

        DocumentResponse response = this.get(false).fetchShapeTreeResponse(new HttpRequest("GET", resourceURI, null, null, null));
        if (response.getStatusCode() != 200) { throw new ShapeTreeException(500, "Failed to load contents of document: " + resourceURI); }
        return response;
    }
}
