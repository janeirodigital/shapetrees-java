package com.janeirodigital.shapetrees.tests;

import com.janeirodigital.shapetrees.core.DocumentResponse;
import com.janeirodigital.shapetrees.core.contentloaders.DocumentLoaderManager;
import com.janeirodigital.shapetrees.core.contentloaders.HttpExternalDocumentLoader;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.tests.fixtures.DispatcherEntry;
import com.janeirodigital.shapetrees.tests.fixtures.RequestMatchingFixtureDispatcher;
import lombok.SneakyThrows;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.jupiter.api.*;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;

class HttpDocumentLoaderTests {

    private static RequestMatchingFixtureDispatcher dispatcher = null;
    private static HttpExternalDocumentLoader httpExternalDocumentLoader;

    public HttpDocumentLoaderTests() {
        httpExternalDocumentLoader = new HttpExternalDocumentLoader();
        DocumentLoaderManager.setLoader(httpExternalDocumentLoader);
    }

    protected URI getURI(MockWebServer server, String path) throws URISyntaxException {
        return new URI(server.url(path).toString());
    }

    @BeforeAll
    static void beforeAll() {

        dispatcher = new RequestMatchingFixtureDispatcher(List.of(
                new DispatcherEntry(List.of("shapetrees/validation-shapetree-ttl"), "GET", "/static/shapetrees/validation/shapetree", null),
                new DispatcherEntry(List.of("http/404"), "GET", "/static/shex/missing", null)));

    }

    @AfterAll
    static void clearDocumentManager() {
        DocumentLoaderManager.setLoader(null);
    }

    @Test
    @DisplayName("Fail to load missing document over http")
    void failToLoadMissingHttpDocument() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        Assertions.assertThrows(ShapeTreeException.class, () -> {
            httpExternalDocumentLoader.loadExternalDocument(getURI(server, "/static/shex/missing"));
        });
    }

    @SneakyThrows
    @Test
    @DisplayName("Successfully load shape tree document over http")
    void loadHttpDocument() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        DocumentResponse shapeTreeDocument = httpExternalDocumentLoader.loadExternalDocument(getURI(server, "/static/shapetrees/validation/shapetree"));
        Assertions.assertNotNull(shapeTreeDocument);
        Assertions.assertEquals(200, shapeTreeDocument.getStatusCode());
        Assertions.assertTrue(shapeTreeDocument.isExists());
        Assertions.assertNotNull(shapeTreeDocument.getBody());
        Assertions.assertNotNull(shapeTreeDocument.getResourceAttributes());
    }

    @SneakyThrows
    @Test
    @DisplayName("Successfully handle thread interruption")
    void handleInterruptedThreadOnLoadHttpDocument() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        Thread.currentThread().interrupt();
        Assertions.assertThrows(ShapeTreeException.class, () -> {
            DocumentResponse shapeTreeDocument = httpExternalDocumentLoader.loadExternalDocument(getURI(server, "/static/shapetrees/validation/shapetree"));
        });
    }

}
