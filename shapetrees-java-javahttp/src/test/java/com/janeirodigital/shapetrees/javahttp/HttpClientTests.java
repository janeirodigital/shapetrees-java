package com.janeirodigital.shapetrees.javahttp;

import lombok.SneakyThrows;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class HttpClientTests {

    @Test
    @SneakyThrows
    void testNonValidatingHandler() {
        JavaHttpClient client = new JavaHttpClientFactory(true, null).get(false);
        Assertions.assertNotNull(client);
    }

    @Test
    @SneakyThrows
    void testValidatingHandler() {
        JavaHttpClient client = new JavaHttpClientFactory(true, null).get(true);
        Assertions.assertNotNull(client);
    }

    @Test
    @SneakyThrows
    void testInsecureClientHandler() {
        JavaHttpClient client = new JavaHttpClientFactory(false, null).get(true);
        Assertions.assertNotNull(client);
    }

    @Test
    @SneakyThrows
    void testReusingClientsSameConfigInstance() {
        JavaHttpClient client1 = new JavaHttpClientFactory(true, null).get(false);
        JavaHttpClient client2 = new JavaHttpClientFactory(true, null).get(false);

        Assertions.assertNotEquals(client1, client2);
    }

    @Test
    @SneakyThrows
    void testReusingClientsSameConfigDifferenceInstances() {
        JavaHttpClient client1 = new JavaHttpClientFactory(true, null).get(true);
        JavaHttpClient client2 = new JavaHttpClientFactory(true, null).get(true);

        Assertions.assertNotEquals(client1, client2);
    }

    @Test
    @SneakyThrows
    void testReusingClientsDifferentConfigurations() {
        JavaHttpClient client1 = new JavaHttpClientFactory(true, null).get(true);
        JavaHttpClient client2 = new JavaHttpClientFactory(false, null).get(true);

        Assertions.assertNotEquals(client1, client2);
    }

}
