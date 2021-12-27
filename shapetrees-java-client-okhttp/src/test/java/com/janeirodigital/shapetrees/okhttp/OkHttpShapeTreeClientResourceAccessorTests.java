package com.janeirodigital.shapetrees.okhttp;

import com.janeirodigital.shapetrees.client.http.HttpClient;
import com.janeirodigital.shapetrees.client.http.HttpClientFactory;
import com.janeirodigital.shapetrees.client.http.HttpClientFactoryManager;
import com.janeirodigital.shapetrees.client.http.HttpResourceAccessor;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.tests.AbstractResourceAccessorTests;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.TestMethodOrder;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class OkHttpShapeTreeClientResourceAccessorTests extends AbstractResourceAccessorTests {

    protected HttpResourceAccessor httpResourceAccessor = null;
    protected HttpClient fetcher = null;
    protected HttpClientFactory factory = null;

    public OkHttpShapeTreeClientResourceAccessorTests() {

        // Call AbstractHttpClientResourceAccessorTests constructor
        // Which in turn calls the AbstractResourceAccessor constructor
        super();

        this.resourceAccessor = new HttpResourceAccessor();
        this.factory = new OkHttpShapeTreeClientFactory(false, new BlackWhiteList(null, null));
        HttpClientFactoryManager.setFactory(this.factory);
        this.skipShapeTreeValidation(false);  // Get an OkHttpShapeTreeClient from the HttpClientFactory set above

    }

    protected void skipShapeTreeValidation(boolean b) {
        try {
            this.fetcher = this.factory.get(!b);
        } catch (ShapeTreeException e) {
            throw new Error(e);
        }
    }

}
