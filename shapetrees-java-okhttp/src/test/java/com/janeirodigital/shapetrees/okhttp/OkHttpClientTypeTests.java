package com.janeirodigital.shapetrees.okhttp;

import com.janeirodigital.shapetrees.client.http.AbstractHttpClientFactory;
import com.janeirodigital.shapetrees.core.contentloaders.DocumentLoaderManager;
import com.janeirodigital.shapetrees.core.contentloaders.ExternalDocumentLoader;
import com.janeirodigital.shapetrees.tests.clienthttp.AbstractHttpClientTypeTests;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.TestMethodOrder;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class OkHttpClientTypeTests extends AbstractHttpClientTypeTests {

    public OkHttpClientTypeTests() {

        // Call AbstractHttpClientTypeTests constructor
        // Which in turn calls the AbstractHttpClient constructor
        super();

        this.factory = new OkHttpClientFactory(false, new BlackWhiteList(null, null));
        AbstractHttpClientFactory.setFactory(this.factory);
        DocumentLoaderManager.setLoader((ExternalDocumentLoader) this.factory);

        this.skipShapeTreeValidation(false);  // Get an OkHttpClient from the HttpClientFactory set above

    }

}
