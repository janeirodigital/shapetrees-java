package com.janeirodigital.shapetrees.javahttp;

import com.janeirodigital.shapetrees.client.http.AbstractHttpClientFactory;
import com.janeirodigital.shapetrees.core.contentloaders.DocumentLoaderManager;
import com.janeirodigital.shapetrees.core.contentloaders.ExternalDocumentLoader;
import com.janeirodigital.shapetrees.tests.clienthttp.AbstractHttpClientProjectRecursiveTests;
import com.janeirodigital.shapetrees.tests.fixtures.RequestMatchingFixtureDispatcher;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.TestMethodOrder;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class JavaHttpClientProjectRecursiveTests extends AbstractHttpClientProjectRecursiveTests {

    private static RequestMatchingFixtureDispatcher dispatcher = null;

    public JavaHttpClientProjectRecursiveTests() {

        // Call AbstractHttpClientProjectRecursiveTests constructor
        // Which in turn calls the AbstractHttpClient constructor
        super();

        this.factory = new JavaHttpClientFactory(false);
        AbstractHttpClientFactory.setFactory(this.factory);
        DocumentLoaderManager.setLoader((ExternalDocumentLoader) this.factory);

        this.skipShapeTreeValidation(false);  // Get a JavaHttpClient from the HttpClientFactory set above

    }

}
