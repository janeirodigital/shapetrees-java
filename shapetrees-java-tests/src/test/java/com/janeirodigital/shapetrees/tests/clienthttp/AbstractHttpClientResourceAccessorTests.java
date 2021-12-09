package com.janeirodigital.shapetrees.tests.clienthttp;

import com.janeirodigital.shapetrees.client.http.HttpClient;
import com.janeirodigital.shapetrees.client.http.HttpClientFactory;
import com.janeirodigital.shapetrees.client.http.HttpResourceAccessor;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.tests.AbstractResourceAccessorTests;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.TestMethodOrder;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class AbstractHttpClientResourceAccessorTests extends AbstractResourceAccessorTests {

    protected HttpResourceAccessor httpResourceAccessor = null;
    protected HttpClient fetcher = null;
    protected HttpClientFactory factory = null;


    public AbstractHttpClientResourceAccessorTests() {

        super();

        this.resourceAccessor = new HttpResourceAccessor();

    }

    protected void skipShapeTreeValidation(boolean b) {
        try {
            this.fetcher = this.factory.get(!b);
        } catch (ShapeTreeException e) {
            throw new Error(e);
        }
    }

}
