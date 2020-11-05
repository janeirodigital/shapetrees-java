package com.janeirodigital.shapetrees.test;

import com.janeirodigital.shapetrees.ShapeTreeException;
import com.janeirodigital.shapetrees.client.impl.ShapeTreeClientConfiguration;
import com.janeirodigital.shapetrees.client.impl.ShapeTreeHttpClientHolder;
import lombok.SneakyThrows;
import okhttp3.OkHttpClient;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class HttpClientTests {

    @Test
    @SneakyThrows
    void testNonValidatingHandler() {
        ShapeTreeClientConfiguration config = new ShapeTreeClientConfiguration(null, false, false);
        OkHttpClient client = ShapeTreeHttpClientHolder.getForConfig(config);
        assertNotNull(client);
    }

    @Test
    @SneakyThrows
    void testValidatingHandler() {
        ShapeTreeClientConfiguration config = new ShapeTreeClientConfiguration(new MockEcosystem(), true, false);
        OkHttpClient client = ShapeTreeHttpClientHolder.getForConfig(config);
        assertNotNull(client);
    }

    @Test
    @SneakyThrows
    void testInsecureClientHandler() {
        ShapeTreeClientConfiguration config = new ShapeTreeClientConfiguration(new MockEcosystem(), true, true);
        OkHttpClient client = ShapeTreeHttpClientHolder.getForConfig(config);
        assertNotNull(client);
    }

    @Test
    @SneakyThrows
    void testInvalidConfiguration() {
        // Must have an ecosystem if validation is enabled
        ShapeTreeClientConfiguration config = new ShapeTreeClientConfiguration(null, true, false);
        assertThrows(ShapeTreeException.class, () ->ShapeTreeHttpClientHolder.getForConfig(config));
    }

    @Test
    @SneakyThrows
    void testReusingClientsSameConfigInstance() {
        ShapeTreeClientConfiguration config = new ShapeTreeClientConfiguration(null, false, false);
        OkHttpClient client1 = ShapeTreeHttpClientHolder.getForConfig(config);
        OkHttpClient client2 = ShapeTreeHttpClientHolder.getForConfig(config);

        assertEquals(client1, client2);
    }

    @Test
    @SneakyThrows
    void testReusingClientsSameConfigDifferenceInstances() {
        ShapeTreeClientConfiguration config1 = new ShapeTreeClientConfiguration(new MockEcosystem(), true, false);
        ShapeTreeClientConfiguration config2 = new ShapeTreeClientConfiguration(new MockEcosystem(), true, false);
        OkHttpClient client1 = ShapeTreeHttpClientHolder.getForConfig(config1);
        OkHttpClient client2 = ShapeTreeHttpClientHolder.getForConfig(config2);

        assertEquals(client1, client2);
    }

    @Test
    @SneakyThrows
    void testReusingClientsDifferentConfigurations() {
        ShapeTreeClientConfiguration config1 = new ShapeTreeClientConfiguration(new MockEcosystem(), true, false);
        ShapeTreeClientConfiguration config2 = new ShapeTreeClientConfiguration(new MockEcosystem(), true, true);
        OkHttpClient client1 = ShapeTreeHttpClientHolder.getForConfig(config1);
        OkHttpClient client2 = ShapeTreeHttpClientHolder.getForConfig(config2);

        assertNotEquals(client1, client2);
    }

}
