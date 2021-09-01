package com.janeirodigital.shapetrees.javahttp;

import lombok.SneakyThrows;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class HttpClientTests {

    @Test
    @SneakyThrows
    void testNonValidatingHandler() {
        JavaHttpClient client = new JavaHttpClientFactory(true).get(false);
        Assertions.assertNotNull(client);
    }

    @Test
    @SneakyThrows
    void testValidatingHandler() {
        JavaHttpClient client = new JavaHttpClientFactory(true).get(true);
        Assertions.assertNotNull(client);
    }

    @Test
    @SneakyThrows
    void testInsecureClientHandler() {
        JavaHttpClient client = new JavaHttpClientFactory(false).get(true);
        Assertions.assertNotNull(client);
    }

    @Test
    @SneakyThrows
    void testReusingClientsSameConfigInstance() {
        JavaHttpClient client1 = new JavaHttpClientFactory(true).get(false);
        JavaHttpClient client2 = new JavaHttpClientFactory(true).get(false);

        Assertions.assertNotEquals(client1, client2);
    }

    @Test
    @SneakyThrows
    void testReusingClientsSameConfigDifferenceInstances() {
        JavaHttpClient client1 = new JavaHttpClientFactory(true).get(true);
        JavaHttpClient client2 = new JavaHttpClientFactory(true).get(true);

        Assertions.assertNotEquals(client1, client2);
    }

    @Test
    @SneakyThrows
    void testReusingClientsDifferentConfigurations() {
        JavaHttpClient client1 = new JavaHttpClientFactory(true).get(true);
        JavaHttpClient client2 = new JavaHttpClientFactory(false).get(true);

        Assertions.assertNotEquals(client1, client2);
    }

}
