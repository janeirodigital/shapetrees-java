package com.janeirodigital.shapetrees.okhttp;

import lombok.SneakyThrows;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class HttpClientTests {

    @Test
    @SneakyThrows
    void testNonValidatingHandler() {
        OkHttpClient client = new OkHttpClientFactory(true).get(false);
        Assertions.assertNotNull(client);
    }

    @Test
    @SneakyThrows
    void testValidatingHandler() {
        OkHttpClient client = new OkHttpClientFactory(true).get(true);
        Assertions.assertNotNull(client);
    }

    @Test
    @SneakyThrows
    void testInsecureClientHandler() {
        OkHttpClient client = new OkHttpClientFactory(false).get(true);
        Assertions.assertNotNull(client);
    }

    @Test
    @SneakyThrows
    void testReusingClientsSameConfigInstance() {
        OkHttpClient client1 = new OkHttpClientFactory(true).get(false);
        OkHttpClient client2 = new OkHttpClientFactory(true).get(false);

        Assertions.assertEquals(client1, client2);
    }

    @Test
    @SneakyThrows
    void testReusingClientsSameConfigDifferenceInstances() {
        OkHttpClient client1 = new OkHttpClientFactory(true).get(true);
        OkHttpClient client2 = new OkHttpClientFactory(true).get(true);

        Assertions.assertEquals(client1, client2);
    }

    @Test
    @SneakyThrows
    void testReusingClientsDifferentConfigurations() {
        OkHttpClient client1 = new OkHttpClientFactory(true).get(true);
        OkHttpClient client2 = new OkHttpClientFactory(false).get(true);

        Assertions.assertNotEquals(client1, client2);
    }

}
