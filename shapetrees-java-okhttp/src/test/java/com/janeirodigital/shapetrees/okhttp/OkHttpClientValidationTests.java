package com.janeirodigital.shapetrees.okhttp;

import com.janeirodigital.shapetrees.client.http.HttpClientFactoryManager;
import com.janeirodigital.shapetrees.core.contentloaders.DocumentLoaderManager;
import com.janeirodigital.shapetrees.core.contentloaders.ExternalDocumentLoader;
import com.janeirodigital.shapetrees.tests.clienthttp.AbstractHttpClientValidationTests;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.TestMethodOrder;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class OkHttpClientValidationTests extends AbstractHttpClientValidationTests {

    public OkHttpClientValidationTests() {

        // Call AbstractHttpClientValidationTests constructor
        // Which in turn calls the AbstractHttpClient constructor
        super();

        this.factory = new OkHttpClientFactory(false, new BlackWhiteList(null, null));
        HttpClientFactoryManager.setFactory(this.factory);
        DocumentLoaderManager.setLoader((ExternalDocumentLoader) this.factory);

        this.skipShapeTreeValidation(false);  // Get an OkHttpClient from the HttpClientFactory set above
    }

}