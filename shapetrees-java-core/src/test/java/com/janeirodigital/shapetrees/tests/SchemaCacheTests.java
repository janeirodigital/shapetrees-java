package com.janeirodigital.shapetrees.tests;

import com.janeirodigital.shapetrees.core.contentloaders.DocumentLoaderManager;
import com.janeirodigital.shapetrees.core.contentloaders.HttpExternalDocumentLoader;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.resources.DocumentResponse;
import com.janeirodigital.shapetrees.core.validation.SchemaCache;
import com.janeirodigital.shapetrees.tests.fixtures.MockWebServerHelper;
import com.janeirodigital.shapetrees.tests.fixtures.RequestMatchingFixtureDispatcher;
import lombok.extern.slf4j.Slf4j;
import okhttp3.mockwebserver.MockWebServer;
import org.apache.jena.shex.Shex;
import org.apache.jena.shex.ShexSchema;
import org.junit.jupiter.api.*;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static com.janeirodigital.shapetrees.tests.fixtures.DispatcherHelper.mockOnGet;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

@Slf4j
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class SchemaCacheTests {

    private static MockWebServer server;

    public SchemaCacheTests() {
        HttpExternalDocumentLoader httpExternalDocumentLoader = new HttpExternalDocumentLoader();
        DocumentLoaderManager.setLoader(httpExternalDocumentLoader);
    }

    @BeforeAll
    static void beforeAll() throws ShapeTreeException {
        RequestMatchingFixtureDispatcher dispatcher = new RequestMatchingFixtureDispatcher();
        mockOnGet(dispatcher, "/static/shex/project", "schemas/project-shex");
        server = new MockWebServer();
        server.setDispatcher(dispatcher);

        SchemaCache.unInitializeCache();
    }

    @Test
    @Order(1)
    void testFailToOperateOnUninitializedCache() throws MalformedURLException, ShapeTreeException {

        assertFalse(SchemaCache.isInitialized());

        // containsSchema
        Throwable containsException = Assertions.assertThrows(ShapeTreeException.class, () ->
                SchemaCache.containsSchema(new URL("http://schema.example"))
        );
        Assertions.assertEquals(SchemaCache.CACHE_IS_NOT_INITIALIZED, containsException.getMessage());

        // getSchema
        Throwable getException = Assertions.assertThrows(ShapeTreeException.class, () ->
                SchemaCache.getSchema(new URL("http://schema.example"))
        );
        Assertions.assertEquals(SchemaCache.CACHE_IS_NOT_INITIALIZED, getException.getMessage());

        // putSchema
        Throwable putException = Assertions.assertThrows(ShapeTreeException.class, () ->
                SchemaCache.putSchema(new URL("http://schema.example"), null)
        );
        Assertions.assertEquals(SchemaCache.CACHE_IS_NOT_INITIALIZED, putException.getMessage());

        // clearSchema
        Throwable clearException = Assertions.assertThrows(ShapeTreeException.class, () -> SchemaCache.clearCache());
        Assertions.assertEquals(SchemaCache.CACHE_IS_NOT_INITIALIZED, clearException.getMessage());

    }

    @Test
    @Order(2)
    void testInitializeCache() throws MalformedURLException, ShapeTreeException {
        SchemaCache.initializeCache();
        assertTrue(SchemaCache.isInitialized());
        assertFalse(SchemaCache.containsSchema(new URL("http://schema.example")));
    }

    @Test
    @Order(3)
    void testPreloadCache() throws MalformedURLException, ShapeTreeException {
        Map<URL, ShexSchema> schemas = buildSchemaCache(List.of(MockWebServerHelper.toUrl(server, "/static/shex/project").toString()));
        SchemaCache.initializeCache(schemas);
        assertTrue(SchemaCache.containsSchema(MockWebServerHelper.toUrl(server, "/static/shex/project")));
    }

    @Test
    @Order(4)
    void testClearPutGet() throws MalformedURLException, ShapeTreeException {
        SchemaCache.clearCache();
        Assertions.assertNull(SchemaCache.getSchema(MockWebServerHelper.toUrl(server, "/static/shex/project")));
        Map<URL, ShexSchema> schemas = buildSchemaCache(List.of(MockWebServerHelper.toUrl(server, "/static/shex/project").toString()));
        Map.Entry<URL, ShexSchema> firstEntry = schemas.entrySet().stream().findFirst().orElse(null);
        if (firstEntry == null) return;
        SchemaCache.putSchema(firstEntry.getKey(), firstEntry.getValue());
        Assertions.assertNotNull(SchemaCache.getSchema(MockWebServerHelper.toUrl(server, "/static/shex/project")));

    }

    @Test
    @Order(5)
    void testNullOnCacheContains() throws MalformedURLException, ShapeTreeException {
        SchemaCache.clearCache();

        Assertions.assertNull(SchemaCache.getSchema(MockWebServerHelper.toUrl(server, "/static/shex/project")));
        Map<URL, ShexSchema> schemas = buildSchemaCache(List.of(MockWebServerHelper.toUrl(server, "/static/shex/project").toString()));
        Map.Entry<URL, ShexSchema> firstEntry = schemas.entrySet().stream().findFirst().orElse(null);
        if (firstEntry == null) return;
        SchemaCache.putSchema(firstEntry.getKey(), firstEntry.getValue());
        Assertions.assertNotNull(SchemaCache.getSchema(MockWebServerHelper.toUrl(server, "/static/shex/project")));

    }

    public static Map<URL, ShexSchema> buildSchemaCache(List<String> schemasToCache) throws MalformedURLException, ShapeTreeException {
        Map<URL, ShexSchema> schemaCache = new HashMap<>();
        log.info("Building schema cache");
        for (String schemaUrl : schemasToCache) {
            log.debug("Caching schema {}", schemaUrl);
            DocumentResponse shexShapeSchema = DocumentLoaderManager.getLoader().loadExternalDocument(new URL(schemaUrl));
            if (Boolean.FALSE.equals(shexShapeSchema.isExists()) || shexShapeSchema.getBody() == null) {
                log.warn("Schema at {} doesn't exist or is empty", schemaUrl);
                continue;
            }

            String shapeBody = shexShapeSchema.getBody();
            try {
                ShexSchema schema = Shex.schemaFromString(shapeBody);
                schemaCache.put(new URL(schemaUrl), schema);
            } catch (Exception ex) {
                log.error("Error parsing schema {}", schemaUrl);
                log.error("Exception:", ex);
            }
        }
        return schemaCache;
    }

}
