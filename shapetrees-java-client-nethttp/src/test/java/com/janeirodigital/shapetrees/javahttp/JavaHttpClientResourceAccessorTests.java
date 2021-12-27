package com.janeirodigital.shapetrees.javahttp;

import com.janeirodigital.shapetrees.client.http.HttpClientFactoryManager;
import com.janeirodigital.shapetrees.tests.clienthttp.AbstractHttpClientResourceAccessorTests;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.TestMethodOrder;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class JavaHttpClientResourceAccessorTests extends AbstractHttpClientResourceAccessorTests {

    public JavaHttpClientResourceAccessorTests() {

        // Call AbstractHttpClientResourceAccessorTests constructor
        // Which in turn calls the AbstractResourceAccessor constructor
        super();

        this.factory = new JavaHttpClientFactory(false);
        HttpClientFactoryManager.setFactory(this.factory);

        this.skipShapeTreeValidation(false);  // Get a JavaHttpClient from the HttpClientFactory set above
    }

}
