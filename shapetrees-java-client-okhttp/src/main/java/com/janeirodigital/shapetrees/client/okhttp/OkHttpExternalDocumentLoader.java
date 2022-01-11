package com.janeirodigital.shapetrees.client.okhttp;

import com.janeirodigital.shapetrees.core.DocumentResponse;
import com.janeirodigital.shapetrees.core.ResourceAttributes;
import com.janeirodigital.shapetrees.core.contentloaders.ExternalDocumentLoader;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;

import java.io.IOException;
import java.net.URL;

public class OkHttpExternalDocumentLoader implements ExternalDocumentLoader {

    /**
     * Load a non-POD document
     * This fulfils the ExternalDocumentLoader interface, so this factory can be use in
     *   DocumentLoaderManager.setLoader(new OkHttpShapeTreeClientFactory(...));
     *
     * @param resourceUrl URL of resource to be retrieved
     * @return a DocumentResponse with the results of a successful GET
     * @throws ShapeTreeException if the GET was not successful
     */
    @Override
    public DocumentResponse loadExternalDocument(URL resourceUrl) throws ShapeTreeException {

        OkHttpClient okHttpClient = OkHttpClientFactoryManager.getFactory().getOkHttpClient();

        Request.Builder requestBuilder = new Request.Builder();
        requestBuilder.url(resourceUrl);
        requestBuilder.method("GET", null);

        DocumentResponse documentResponse;
        try (Response response = okHttpClient.newCall(requestBuilder.build()).execute()) {
            ResourceAttributes attributes = new ResourceAttributes(response.headers().toMultimap());
            documentResponse = new DocumentResponse(attributes, response.body().string(), response.code());
        } catch (IOException ex) {
            throw new ShapeTreeException(500, "Failed to load contents of " + resourceUrl +": " + ex.getMessage());
        }

        if (documentResponse.getStatusCode() != 200) { throw new ShapeTreeException(500, "Failed to load contents of document: " + resourceUrl); }
        return documentResponse;

    }

}
