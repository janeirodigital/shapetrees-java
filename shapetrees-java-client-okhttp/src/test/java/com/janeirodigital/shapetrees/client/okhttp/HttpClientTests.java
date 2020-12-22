package com.janeirodigital.shapetrees.client.okhttp;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.client.core.ShapeTreeClientConfiguration;
import lombok.SneakyThrows;
import okhttp3.OkHttpClient;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class HttpClientTests {

    @Test
    @SneakyThrows
    void testNonValidatingHandler() {
        ShapeTreeClientConfiguration config = new ShapeTreeClientConfiguration(false, false);
        OkHttpClient client = ShapeTreeHttpClientHolder.getForConfig(config);
        Assertions.assertNotNull(client);
    }

    @Test
    @SneakyThrows
    void testValidatingHandler() {
        ShapeTreeClientConfiguration config = new ShapeTreeClientConfiguration(true, false);
        OkHttpClient client = ShapeTreeHttpClientHolder.getForConfig(config);
        Assertions.assertNotNull(client);
    }

    @Test
    @SneakyThrows
    void testInsecureClientHandler() {
        ShapeTreeClientConfiguration config = new ShapeTreeClientConfiguration(true, true);
        OkHttpClient client = ShapeTreeHttpClientHolder.getForConfig(config);
        Assertions.assertNotNull(client);
    }

    @Test
    @SneakyThrows
    void testReusingClientsSameConfigInstance() {
        ShapeTreeClientConfiguration config = new ShapeTreeClientConfiguration(false, false);
        OkHttpClient client1 = ShapeTreeHttpClientHolder.getForConfig(config);
        OkHttpClient client2 = ShapeTreeHttpClientHolder.getForConfig(config);

        Assertions.assertEquals(client1, client2);
    }

    @Test
    @SneakyThrows
    void testReusingClientsSameConfigDifferenceInstances() {
        ShapeTreeClientConfiguration config1 = new ShapeTreeClientConfiguration(true, false);
        ShapeTreeClientConfiguration config2 = new ShapeTreeClientConfiguration(true, false);
        OkHttpClient client1 = ShapeTreeHttpClientHolder.getForConfig(config1);
        OkHttpClient client2 = ShapeTreeHttpClientHolder.getForConfig(config2);

        Assertions.assertEquals(client1, client2);
    }

    @Test
    @SneakyThrows
    void testReusingClientsDifferentConfigurations() {
        ShapeTreeClientConfiguration config1 = new ShapeTreeClientConfiguration(true, false);
        ShapeTreeClientConfiguration config2 = new ShapeTreeClientConfiguration(true, true);
        OkHttpClient client1 = ShapeTreeHttpClientHolder.getForConfig(config1);
        OkHttpClient client2 = ShapeTreeHttpClientHolder.getForConfig(config2);

        Assertions.assertNotEquals(client1, client2);
    }

}
