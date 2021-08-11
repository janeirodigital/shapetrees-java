package com.janeirodigital.shapetrees.okhttp;

import com.janeirodigital.shapetrees.client.http.HttpShapeTreeClientConfiguration;
import lombok.SneakyThrows;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class HttpClientTests {

    @Test
    @SneakyThrows
    void testNonValidatingHandler() {
        HttpShapeTreeClientConfiguration config = new HttpShapeTreeClientConfiguration(false, false);
        OkHttpClient client = new OkHttpClientFactory().getForConfig(config);
        Assertions.assertNotNull(client);
    }

    @Test
    @SneakyThrows
    void testValidatingHandler() {
        HttpShapeTreeClientConfiguration config = new HttpShapeTreeClientConfiguration(true, false);
        OkHttpClient client = new OkHttpClientFactory().getForConfig(config);
        Assertions.assertNotNull(client);
    }

    @Test
    @SneakyThrows
    void testInsecureClientHandler() {
        HttpShapeTreeClientConfiguration config = new HttpShapeTreeClientConfiguration(true, true);
        OkHttpClient client = new OkHttpClientFactory().getForConfig(config);
        Assertions.assertNotNull(client);
    }

    @Test
    @SneakyThrows
    void testReusingClientsSameConfigInstance() {
        HttpShapeTreeClientConfiguration config = new HttpShapeTreeClientConfiguration(false, false);
        OkHttpClient client1 = new OkHttpClientFactory().getForConfig(config);
        OkHttpClient client2 = new OkHttpClientFactory().getForConfig(config);

        Assertions.assertEquals(client1, client2);
    }

    @Test
    @SneakyThrows
    void testReusingClientsSameConfigDifferenceInstances() {
        HttpShapeTreeClientConfiguration config1 = new HttpShapeTreeClientConfiguration(true, false);
        HttpShapeTreeClientConfiguration config2 = new HttpShapeTreeClientConfiguration(true, false);
        OkHttpClient client1 = new OkHttpClientFactory().getForConfig(config1);
        OkHttpClient client2 = new OkHttpClientFactory().getForConfig(config2);

        Assertions.assertEquals(client1, client2);
    }

    @Test
    @SneakyThrows
    void testReusingClientsDifferentConfigurations() {
        HttpShapeTreeClientConfiguration config1 = new HttpShapeTreeClientConfiguration(true, false);
        HttpShapeTreeClientConfiguration config2 = new HttpShapeTreeClientConfiguration(true, true);
        OkHttpClient client1 = new OkHttpClientFactory().getForConfig(config1);
        OkHttpClient client2 = new OkHttpClientFactory().getForConfig(config2);

        Assertions.assertNotEquals(client1, client2);
    }

}
