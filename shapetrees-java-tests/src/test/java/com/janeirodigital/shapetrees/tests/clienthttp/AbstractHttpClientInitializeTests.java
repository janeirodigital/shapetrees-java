package com.janeirodigital.shapetrees.tests.clienthttp;

import com.janeirodigital.shapetrees.client.http.HttpClient;
import com.janeirodigital.shapetrees.client.http.HttpClientFactory;
import lombok.SneakyThrows;
import org.junit.jupiter.api.*;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class AbstractHttpClientInitializeTests {

    protected HttpClientFactory factory;

    public AbstractHttpClientInitializeTests() {
        // Call AbstractHttpClient constructor
        super();
    }

    @Test
    @SneakyThrows
    void testNonValidatingHandler() {
        HttpClient client = this.factory.get(false);
        Assertions.assertNotNull(client);
    }

    @Test
    @SneakyThrows
    void testValidatingHandler() {
        HttpClient client = this.factory.get(true);
        Assertions.assertNotNull(client);
    }

}
