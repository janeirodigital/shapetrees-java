package com.janeirodigital.shapetrees.okhttp;

import com.janeirodigital.shapetrees.client.http.AbstractHttpClientFactory;
import com.janeirodigital.shapetrees.client.http.HttpClientFactory;
import com.janeirodigital.shapetrees.client.http.HttpRequest;
import com.janeirodigital.shapetrees.core.ShapeTreeResponse;
import com.janeirodigital.shapetrees.core.contentloaders.BlackWhiteList;
import com.janeirodigital.shapetrees.core.contentloaders.ExternalDocumentLoader;
import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.DocumentResponse;

import java.net.URI;

public class OkHttpClientFactory extends AbstractHttpClientFactory implements HttpClientFactory, ExternalDocumentLoader {
    boolean useSslValidation;
    static final int NON_VALIDATING = 0;
    static final int VALIDATING = 1;
    private static OkHttpClient[][] okHttpClients = {{null, null}, {null, null}};
    private final BlackWhiteList blackWhiteList;

    OkHttpClientFactory(boolean useSslValidation, BlackWhiteList blackWhiteList) {
        this.useSslValidation = useSslValidation;
        this.blackWhiteList = blackWhiteList;
    }

    public OkHttpClient get(boolean useClientShapeTreeValidation) throws ShapeTreeException {
        return getForOptions(this.useSslValidation, useClientShapeTreeValidation);
    }

    private static synchronized OkHttpClient getForOptions(boolean useSslValidation, boolean useClientShapeTreeValidation) throws ShapeTreeException {

        int ssl = useSslValidation ? VALIDATING : NON_VALIDATING;
        int shapeTrees = useClientShapeTreeValidation ? VALIDATING : NON_VALIDATING;

        if (okHttpClients[ssl][shapeTrees] != null) {
            return okHttpClients[ssl][shapeTrees];
        }
        try {
            OkHttpClient client = new OkHttpClient(useSslValidation, useClientShapeTreeValidation);
            okHttpClients[ssl][shapeTrees] = client;
            return client;
        } catch (Exception ex) {
            throw new ShapeTreeException(500, ex.getMessage());
        }

    }

    @Override
    public DocumentResponse loadExternalDocument(URI resourceURI) throws ShapeTreeException {
        if (blackWhiteList != null) { blackWhiteList.check(resourceURI); }

        ShapeTreeResponse response = this.get(false).fetchShapeTreeResponse(new HttpRequest("GET", resourceURI, null, null, null));
        if (response.getStatusCode() != 200) { throw new ShapeTreeException(500, "Failed to load contents of document: " + resourceURI); }
        return new DocumentResponse(resourceURI, response.getBody(), response.getHeaders().firstValue(HttpHeaders.CONTENT_TYPE.getValue()).orElse("text/turtle"));
    }
}
