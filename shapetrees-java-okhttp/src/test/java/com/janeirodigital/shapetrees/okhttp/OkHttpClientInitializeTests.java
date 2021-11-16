package com.janeirodigital.shapetrees.okhttp;

import com.janeirodigital.shapetrees.client.http.HttpClientFactoryManager;
import com.janeirodigital.shapetrees.core.contentloaders.DocumentLoaderManager;
import com.janeirodigital.shapetrees.core.contentloaders.ExternalDocumentLoader;
import com.janeirodigital.shapetrees.tests.clienthttp.AbstractHttpClientInitializeTests;
import lombok.SneakyThrows;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class OkHttpClientInitializeTests extends AbstractHttpClientInitializeTests {

    public OkHttpClientInitializeTests() {

        // Call AbstractHttpClientInitializeTests constructor
        // Which in turn calls the AbstractHttpClient constructor
        super();

        this.factory = new OkHttpClientFactory(false, new BlackWhiteList(null, null));
        HttpClientFactoryManager.setFactory(this.factory);
        DocumentLoaderManager.setLoader((ExternalDocumentLoader) this.factory);

        this.skipShapeTreeValidation(false);  // Get an OkHttpClient from the HttpClientFactory set above

    }

    // TODO - Add tests for BlackWhiteList

    @Test
    @SneakyThrows
    void testInsecureClientHandler() {
        OkHttpClient client = new OkHttpClientFactory(false, null).get(true);
        Assertions.assertNotNull(client);
    }

    @Test
    @SneakyThrows
    void testReusingClientsSameConfigInstance() {
        OkHttpClient client1 = new OkHttpClientFactory(true, null).get(false);
        OkHttpClient client2 = new OkHttpClientFactory(true, null).get(false);

        Assertions.assertEquals(client1, client2);
    }

    @Test
    @SneakyThrows
    void testReusingClientsSameConfigDifferenceInstances() {

        // TODO - double-check the accuracy of this test setup
        OkHttpClient client1 = new OkHttpClientFactory(true, null).get(true);
        OkHttpClient client2 = new OkHttpClientFactory(true, null).get(true);

        Assertions.assertEquals(client1, client2);
    }

    @Test
    @SneakyThrows
    void testReusingClientsDifferentConfigurations() {
        OkHttpClient client1 = new OkHttpClientFactory(true, null).get(true);
        OkHttpClient client2 = new OkHttpClientFactory(false, null).get(true);

        Assertions.assertNotEquals(client1, client2);
    }

}
