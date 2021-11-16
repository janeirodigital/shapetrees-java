package com.janeirodigital.shapetrees.javahttp;

import com.janeirodigital.shapetrees.client.http.HttpClientFactoryManager;
import com.janeirodigital.shapetrees.core.contentloaders.DocumentLoaderManager;
import com.janeirodigital.shapetrees.core.contentloaders.ExternalDocumentLoader;
import com.janeirodigital.shapetrees.tests.clienthttp.AbstractHttpClientProjectTests;
import com.janeirodigital.shapetrees.tests.fixtures.RequestMatchingFixtureDispatcher;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.TestMethodOrder;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class JavaHttpClientProjectTests extends AbstractHttpClientProjectTests {

    private static RequestMatchingFixtureDispatcher dispatcher = null;

    public JavaHttpClientProjectTests() {

        // Call AbstractHttpClientProjectTests constructor
        // Which in turn calls the AbstractHttpClient constructor
        super();

        this.factory = new JavaHttpClientFactory(false);
        HttpClientFactoryManager.setFactory(this.factory);
        DocumentLoaderManager.setLoader((ExternalDocumentLoader) this.factory);

        this.skipShapeTreeValidation(false);  // Get a JavaHttpClient from the HttpClientFactory set above

    }

}
