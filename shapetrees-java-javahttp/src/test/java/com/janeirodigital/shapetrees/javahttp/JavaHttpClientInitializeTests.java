package com.janeirodigital.shapetrees.javahttp;

import com.janeirodigital.shapetrees.client.http.AbstractHttpClientFactory;
import com.janeirodigital.shapetrees.core.contentloaders.DocumentLoaderManager;
import com.janeirodigital.shapetrees.core.contentloaders.ExternalDocumentLoader;
import com.janeirodigital.shapetrees.tests.clienthttp.AbstractHttpClientInitializeTests;
import lombok.SneakyThrows;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class JavaHttpClientInitializeTests extends AbstractHttpClientInitializeTests {

    public JavaHttpClientInitializeTests() {

        // Call AbstractHttpClientInitializeTests constructor
        // Which in turn calls the AbstractHttpClient constructor
        super();

        this.factory = new JavaHttpClientFactory(false);
        AbstractHttpClientFactory.setFactory(this.factory);
        DocumentLoaderManager.setLoader((ExternalDocumentLoader) this.factory);

        this.skipShapeTreeValidation(false);  // Get an JavaHttpClient from the HttpClientFactory set above

    }

    @Test
    @SneakyThrows
    void testInsecureClientHandler() {
        JavaHttpClient client = new JavaHttpClientFactory(false).get(true);
        Assertions.assertNotNull(client);
    }

    @Test
    @SneakyThrows
    void testReusingClientsDifferentConfigurations() {
        JavaHttpClient client1 = new JavaHttpClientFactory(true).get(true);
        JavaHttpClient client2 = new JavaHttpClientFactory(false).get(true);

        Assertions.assertNotEquals(client1, client2);
    }

}
