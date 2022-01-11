package com.janeirodigital.shapetrees.client.okhttp;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import okhttp3.OkHttpClient;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class OkHttpShapeTreeClientInitializeTests {

    // Confirm that we get whatever is set by the client manager
    // Confirm that the validating factory is a derivative of the main factory

    @Test
    @DisplayName("Initialize basic client factory")
    void initializeBasicClientFactory() throws ShapeTreeException {
        OkHttpClientFactory factory = OkHttpClientFactoryManager.getFactory();
        assertNotNull(factory);
        assertTrue(factory instanceof OkHttpBasicClientFactory);

        OkHttpClient okHttpClient = factory.getOkHttpClient();
        assertNotNull(okHttpClient);
    }

    // TODO - Finish client initialization tests

    @Test
    @DisplayName("Confirm alternative factory can be assigned and used")
    void useAlternativeClientFactory() { }

    @Test
    @DisplayName("Confirm subsequent gets are the same client factory")
    void confirmClientReuseFromBasicFactory() { }

    @Test
    @DisplayName("Initialize validating factory")
    void initializeValidatingClientFactory() { }

    @Test
    @DisplayName("Confirm subsequent gets are the same client factory")
    void confirmClientReuseFromValidatingFactory() { }


}
