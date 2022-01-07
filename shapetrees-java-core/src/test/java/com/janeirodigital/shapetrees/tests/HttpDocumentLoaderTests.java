package com.janeirodigital.shapetrees.tests;

import com.janeirodigital.shapetrees.core.DocumentResponse;
import com.janeirodigital.shapetrees.core.contentloaders.DocumentLoaderManager;
import com.janeirodigital.shapetrees.core.contentloaders.HttpExternalDocumentLoader;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.tests.fixtures.RequestMatchingFixtureDispatcher;
import lombok.SneakyThrows;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.jupiter.api.*;

import static com.janeirodigital.shapetrees.tests.fixtures.DispatcherHelper.mockOnGet;
import static com.janeirodigital.shapetrees.tests.fixtures.MockWebServerHelper.toUrl;

class HttpDocumentLoaderTests {

    private static RequestMatchingFixtureDispatcher dispatcher;
    private static HttpExternalDocumentLoader httpExternalDocumentLoader;
    private static MockWebServer server;

    public HttpDocumentLoaderTests() {
        httpExternalDocumentLoader = new HttpExternalDocumentLoader();
        DocumentLoaderManager.setLoader(httpExternalDocumentLoader);
    }

    @BeforeAll
    static void beforeAll() {

        dispatcher = new RequestMatchingFixtureDispatcher();

        mockOnGet(dispatcher,"/static/shapetrees/validation/shapetree","shapetrees/validation-shapetree-ttl");
        mockOnGet(dispatcher, "/static/shex/missing", "http/404");

        server = new MockWebServer();
        server.setDispatcher(dispatcher);

    }

    @AfterAll
    static void clearDocumentManager() {
        DocumentLoaderManager.setLoader(null);
    }

    @Test
    @DisplayName("Fail to load missing document over http")
    void failToLoadMissingHttpDocument() {
        Assertions.assertThrows(ShapeTreeException.class, () -> {
            httpExternalDocumentLoader.loadExternalDocument(toUrl(server, "/static/shex/missing"));
        });
    }

    @SneakyThrows
    @Test
    @DisplayName("Successfully load shape tree document over http")
    void loadHttpDocument() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        DocumentResponse shapeTreeDocument = httpExternalDocumentLoader.loadExternalDocument(toUrl(server, "/static/shapetrees/validation/shapetree"));
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
            httpExternalDocumentLoader.loadExternalDocument(toUrl(server, "/static/shapetrees/validation/shapetree"));
        });
    }

}
