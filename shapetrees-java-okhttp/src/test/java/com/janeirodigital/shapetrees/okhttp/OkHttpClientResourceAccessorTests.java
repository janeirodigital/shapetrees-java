package com.janeirodigital.shapetrees.okhttp;

import com.janeirodigital.shapetrees.client.http.AbstractHttpClientFactory;
import com.janeirodigital.shapetrees.tests.clienthttp.AbstractHttpClientResourceAccessorTests;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.TestMethodOrder;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class OkHttpClientResourceAccessorTests extends AbstractHttpClientResourceAccessorTests {

    public OkHttpClientResourceAccessorTests() {

        // Call AbstractHttpClientResourceAccessorTests constructor
        // Which in turn calls the AbstractResourceAccessor constructor
        super();

        this.factory = new OkHttpClientFactory(false, new BlackWhiteList(null, null));
        AbstractHttpClientFactory.setFactory(this.factory);
        this.skipShapeTreeValidation(false);  // Get an OkHttpClient from the HttpClientFactory set above

    }

}
