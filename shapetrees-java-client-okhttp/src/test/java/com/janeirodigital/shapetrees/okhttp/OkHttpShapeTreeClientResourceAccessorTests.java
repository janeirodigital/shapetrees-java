package com.janeirodigital.shapetrees.okhttp;

import com.janeirodigital.shapetrees.tests.AbstractResourceAccessorTests;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.TestMethodOrder;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class OkHttpShapeTreeClientResourceAccessorTests extends AbstractResourceAccessorTests {

    public OkHttpShapeTreeClientResourceAccessorTests() {
        super();
        // Use the OkHttpClient-based resource accessor
        this.resourceAccessor = new OkHttpResourceAccessor();
    }

}
