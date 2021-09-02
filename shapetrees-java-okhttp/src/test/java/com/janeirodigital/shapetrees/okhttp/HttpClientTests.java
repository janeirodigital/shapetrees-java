package com.janeirodigital.shapetrees.okhttp;

import lombok.SneakyThrows;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class HttpClientTests {

    @Test
    @SneakyThrows
    void testNonValidatingHandler() {
        OkHttpClient client = new OkHttpClientFactory(true, null).get(false);
        Assertions.assertNotNull(client);
    }

    @Test
    @SneakyThrows
    void testValidatingHandler() {
        OkHttpClient client = new OkHttpClientFactory(true, null).get(true);
        Assertions.assertNotNull(client);
    }

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
