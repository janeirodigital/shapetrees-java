package com.janeirodigital.shapetrees.tests;

import com.janeirodigital.shapetrees.core.DocumentResponse;
import com.janeirodigital.shapetrees.core.SchemaCache;
import com.janeirodigital.shapetrees.core.contentloaders.DocumentLoaderManager;
import com.janeirodigital.shapetrees.core.contentloaders.HttpExternalDocumentLoader;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.tests.fixtures.DispatcherEntry;
import com.janeirodigital.shapetrees.tests.fixtures.RequestMatchingFixtureDispatcher;
import fr.inria.lille.shexjava.GlobalFactory;
import fr.inria.lille.shexjava.schema.ShexSchema;
import fr.inria.lille.shexjava.schema.parsing.ShExCParser;
import lombok.extern.slf4j.Slf4j;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.jupiter.api.*;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.net.URL;
import java.net.MalformedURLException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

@Slf4j
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class SchemaCacheTests {

    private static RequestMatchingFixtureDispatcher dispatcher = null;
    private static HttpExternalDocumentLoader httpExternalDocumentLoader;

    public SchemaCacheTests() {
        httpExternalDocumentLoader = new HttpExternalDocumentLoader();
        DocumentLoaderManager.setLoader(httpExternalDocumentLoader);
    }

    protected URL getURL(MockWebServer server, String path) throws MalformedURLException {
        return new URL(server.url(path).toString());
    }

    @BeforeAll
    static void beforeAll() {
        dispatcher = new RequestMatchingFixtureDispatcher(List.of(
                new DispatcherEntry(List.of("schemas/project-shex"), "GET", "/static/shex/project", null)
        ));
    }


    @Test
    @Order(1)
    void testInitializeCache() throws MalformedURLException, ShapeTreeException {
        SchemaCache.initializeCache();
        assertTrue(SchemaCache.isInitialized());
        assertFalse(SchemaCache.containsSchema(new URL("http://schema.example")));
    }

    @Test
    @Order(2)
    void testPreloadCache() throws MalformedURLException, ShapeTreeException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        Map<URL, ShexSchema> schemas = buildSchemaCache(List.of(getURL(server, "/static/shex/project").toString()));
        SchemaCache.initializeCache(schemas);
        assertTrue(SchemaCache.containsSchema(getURL(server, "/static/shex/project")));
    }

    @Test
    @Order(3)
    void testClearPutGet() throws MalformedURLException, ShapeTreeException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        SchemaCache.clearCache();
        Assertions.assertNull(SchemaCache.getSchema(getURL(server, "/static/shex/project")));
        Map<URL, ShexSchema> schemas = buildSchemaCache(List.of(getURL(server, "/static/shex/project").toString()));
        Map.Entry<URL, ShexSchema> firstEntry = schemas.entrySet().stream().findFirst().orElse(null);
        if (firstEntry == null) return;
        SchemaCache.putSchema(firstEntry.getKey(), firstEntry.getValue());
        Assertions.assertNotNull(SchemaCache.getSchema(getURL(server, "/static/shex/project")));

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
            try (InputStream stream = new ByteArrayInputStream(shapeBody.getBytes())) {
                ShExCParser shexCParser = new ShExCParser();
                ShexSchema schema = new ShexSchema(GlobalFactory.RDFFactory, shexCParser.getRules(stream), shexCParser.getStart());
                schemaCache.put(new URL(schemaUrl), schema);
            } catch (Exception ex) {
                log.error("Error parsing schema {}", schemaUrl);
                log.error("Exception:", ex);
            }
        }
        return schemaCache;
    }

}
