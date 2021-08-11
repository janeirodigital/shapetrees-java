package com.janeirodigital.shapetrees.okhttp;

import com.janeirodigital.shapetrees.okhttp.OkHttpFetcher;
import lombok.SneakyThrows;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class HttpClientTests {

    @Test
    @SneakyThrows
    void testNonValidatingHandler() {
        ShapeTreeClientConfiguration config = new ShapeTreeClientConfiguration(false, false);
        OkHttpFetcher client = OkHttpFetcher.getForConfig(config);
        Assertions.assertNotNull(client);
    }

    @Test
    @SneakyThrows
    void testValidatingHandler() {
        ShapeTreeClientConfiguration config = new ShapeTreeClientConfiguration(true, false);
        OkHttpFetcher client = OkHttpFetcher.getForConfig(config);
        Assertions.assertNotNull(client);
    }

    @Test
    @SneakyThrows
    void testInsecureClientHandler() {
        ShapeTreeClientConfiguration config = new ShapeTreeClientConfiguration(true, true);
        OkHttpFetcher client = OkHttpFetcher.getForConfig(config);
        Assertions.assertNotNull(client);
    }

    @Test
    @SneakyThrows
    void testReusingClientsSameConfigInstance() {
        ShapeTreeClientConfiguration config = new ShapeTreeClientConfiguration(false, false);
        OkHttpFetcher client1 = OkHttpFetcher.getForConfig(config);
        OkHttpFetcher client2 = OkHttpFetcher.getForConfig(config);

        Assertions.assertEquals(client1, client2);
    }

    @Test
    @SneakyThrows
    void testReusingClientsSameConfigDifferenceInstances() {
        ShapeTreeClientConfiguration config1 = new ShapeTreeClientConfiguration(true, false);
        ShapeTreeClientConfiguration config2 = new ShapeTreeClientConfiguration(true, false);
        OkHttpFetcher client1 = OkHttpFetcher.getForConfig(config1);
        OkHttpFetcher client2 = OkHttpFetcher.getForConfig(config2);

        Assertions.assertEquals(client1, client2);
    }

    @Test
    @SneakyThrows
    void testReusingClientsDifferentConfigurations() {
        ShapeTreeClientConfiguration config1 = new ShapeTreeClientConfiguration(true, false);
        ShapeTreeClientConfiguration config2 = new ShapeTreeClientConfiguration(true, true);
        OkHttpFetcher client1 = OkHttpFetcher.getForConfig(config1);
        OkHttpFetcher client2 = OkHttpFetcher.getForConfig(config2);

        Assertions.assertNotEquals(client1, client2);
    }

}
