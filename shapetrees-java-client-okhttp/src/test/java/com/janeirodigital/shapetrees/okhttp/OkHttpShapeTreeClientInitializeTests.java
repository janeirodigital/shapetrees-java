package com.janeirodigital.shapetrees.okhttp;

import com.janeirodigital.shapetrees.client.http.HttpClient;
import com.janeirodigital.shapetrees.client.http.HttpClientFactory;
import com.janeirodigital.shapetrees.client.http.HttpClientFactoryManager;
import com.janeirodigital.shapetrees.core.contentloaders.DocumentLoaderManager;
import com.janeirodigital.shapetrees.core.contentloaders.ExternalDocumentLoader;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import lombok.SneakyThrows;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class OkHttpShapeTreeClientInitializeTests {

    private HttpClientFactory factory;
    private HttpClient fetcher;

    public OkHttpShapeTreeClientInitializeTests() {

        this.factory = new OkHttpShapeTreeClientFactory(false, new BlackWhiteList(null, null));
        HttpClientFactoryManager.setFactory(this.factory);
        DocumentLoaderManager.setLoader((ExternalDocumentLoader) this.factory);

        this.skipShapeTreeValidation(false);  // Get an OkHttpShapeTreeClient from the HttpClientFactory set above

    }

    private void skipShapeTreeValidation(boolean b) {
        try {
            this.fetcher = this.factory.get(!b);
        } catch (ShapeTreeException e) {
            throw new Error(e);
        }
    }

    @Test
    @SneakyThrows
    void testInsecureClientHandler() {
        OkHttpShapeTreeClient client = new OkHttpShapeTreeClientFactory(false, null).get(true);
        Assertions.assertNotNull(client);
    }

    @Test
    @SneakyThrows
    void testReusingClientsSameConfigInstance() {
        OkHttpShapeTreeClient client1 = new OkHttpShapeTreeClientFactory(true, null).get(false);
        OkHttpShapeTreeClient client2 = new OkHttpShapeTreeClientFactory(true, null).get(false);

        Assertions.assertEquals(client1, client2);
    }

    @Test
    @SneakyThrows
    void testReusingClientsSameConfigDifferenceInstances() {

        // TODO - double-check the accuracy of this test setup
        OkHttpShapeTreeClient client1 = new OkHttpShapeTreeClientFactory(true, null).get(true);
        OkHttpShapeTreeClient client2 = new OkHttpShapeTreeClientFactory(true, null).get(true);

        Assertions.assertEquals(client1, client2);
    }

    @Test
    @SneakyThrows
    void testReusingClientsDifferentConfigurations() {
        OkHttpShapeTreeClient client1 = new OkHttpShapeTreeClientFactory(true, null).get(true);
        OkHttpShapeTreeClient client2 = new OkHttpShapeTreeClientFactory(false, null).get(true);

        Assertions.assertNotEquals(client1, client2);
    }

}
