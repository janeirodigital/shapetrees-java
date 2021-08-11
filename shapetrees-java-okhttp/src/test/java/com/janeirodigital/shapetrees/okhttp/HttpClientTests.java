package com.janeirodigital.shapetrees.okhttp;

import com.janeirodigital.shapetrees.client.http.ShapeTreeClientConfiguration;
import lombok.SneakyThrows;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class HttpClientTests {

    @Test
    @SneakyThrows
    void testNonValidatingHandler() {
        ShapeTreeClientConfiguration config = new ShapeTreeClientConfiguration(false, false);
        OkHttpClient client = new OkHttpClientFactory().getForConfig(config);
        Assertions.assertNotNull(client);
    }

    @Test
    @SneakyThrows
    void testValidatingHandler() {
        ShapeTreeClientConfiguration config = new ShapeTreeClientConfiguration(true, false);
        OkHttpClient client = new OkHttpClientFactory().getForConfig(config);
        Assertions.assertNotNull(client);
    }

    @Test
    @SneakyThrows
    void testInsecureClientHandler() {
        ShapeTreeClientConfiguration config = new ShapeTreeClientConfiguration(true, true);
        OkHttpClient client = new OkHttpClientFactory().getForConfig(config);
        Assertions.assertNotNull(client);
    }

    @Test
    @SneakyThrows
    void testReusingClientsSameConfigInstance() {
        ShapeTreeClientConfiguration config = new ShapeTreeClientConfiguration(false, false);
        OkHttpClient client1 = new OkHttpClientFactory().getForConfig(config);
        OkHttpClient client2 = new OkHttpClientFactory().getForConfig(config);

        Assertions.assertEquals(client1, client2);
    }

    @Test
    @SneakyThrows
    void testReusingClientsSameConfigDifferenceInstances() {
        ShapeTreeClientConfiguration config1 = new ShapeTreeClientConfiguration(true, false);
        ShapeTreeClientConfiguration config2 = new ShapeTreeClientConfiguration(true, false);
        OkHttpClient client1 = new OkHttpClientFactory().getForConfig(config1);
        OkHttpClient client2 = new OkHttpClientFactory().getForConfig(config2);

        Assertions.assertEquals(client1, client2);
    }

    @Test
    @SneakyThrows
    void testReusingClientsDifferentConfigurations() {
        ShapeTreeClientConfiguration config1 = new ShapeTreeClientConfiguration(true, false);
        ShapeTreeClientConfiguration config2 = new ShapeTreeClientConfiguration(true, true);
        OkHttpClient client1 = new OkHttpClientFactory().getForConfig(config1);
        OkHttpClient client2 = new OkHttpClientFactory().getForConfig(config2);

        Assertions.assertNotEquals(client1, client2);
    }

}
