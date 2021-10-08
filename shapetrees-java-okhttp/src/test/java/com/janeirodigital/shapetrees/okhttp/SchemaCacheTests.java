package com.janeirodigital.shapetrees.okhttp;

import com.janeirodigital.shapetrees.client.http.HttpRemoteResource;
import com.janeirodigital.shapetrees.core.SchemaCache;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.okhttp.fixtures.DispatcherEntry;
import com.janeirodigital.shapetrees.okhttp.fixtures.RequestMatchingFixtureDispatcher;
import fr.inria.lille.shexjava.GlobalFactory;
import fr.inria.lille.shexjava.schema.ShexSchema;
import fr.inria.lille.shexjava.schema.parsing.ShExCParser;
import lombok.extern.slf4j.Slf4j;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.jupiter.api.*;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

@Slf4j
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class SchemaCacheTests extends BaseShapeTreeTest {

    public SchemaCacheTests() {
        super();
    }

    private static RequestMatchingFixtureDispatcher dispatcher = null;

    @BeforeAll
    static void beforeAll() {
        dispatcher = new RequestMatchingFixtureDispatcher(List.of(
                new DispatcherEntry(List.of("schemas/project-shex"), "GET", "/static/shex/project", null)
        ));
    }


    @Test
    @Order(1)
    void testInitializeCache() throws URISyntaxException, ShapeTreeException {
        SchemaCache.initializeCache();
        assertTrue(SchemaCache.isInitialized());
        assertFalse(SchemaCache.containsSchema(new URI("http://schema.example")));
    }

    @Test
    @Order(2)
    void testPreloadCache() throws URISyntaxException, ShapeTreeException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        Map<URI, ShexSchema> schemas = buildSchemaCache(List.of(getURI(server, "/static/shex/project").toString()));
        SchemaCache.initializeCache(schemas);
        assertTrue(SchemaCache.containsSchema(getURI(server, "/static/shex/project")));
    }

    @Test
    @Order(3)
    void testClearPutGet() throws URISyntaxException, ShapeTreeException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        SchemaCache.clearCache();
        Assertions.assertNull(SchemaCache.getSchema(getURI(server, "/static/shex/project")));
        Map<URI, ShexSchema> schemas = buildSchemaCache(List.of(getURI(server, "/static/shex/project").toString()));
        Map.Entry<URI, ShexSchema> firstEntry = schemas.entrySet().stream().findFirst().orElse(null);
        if (firstEntry == null) return;
        SchemaCache.putSchema(firstEntry.getKey(), firstEntry.getValue());
        Assertions.assertNotNull(SchemaCache.getSchema(getURI(server, "/static/shex/project")));

    }

    public static Map<URI, ShexSchema> buildSchemaCache(List<String> schemasToCache) throws URISyntaxException, ShapeTreeException {
        Map<URI, ShexSchema> schemaCache = new HashMap<>();
        log.info("Building schema cache");
        for (String schemaUrl : schemasToCache) {
            log.debug("Caching schema {}", schemaUrl);
            HttpRemoteResource shexShapeSchema = new HttpRemoteResource(new URI(schemaUrl), null);
            if (Boolean.FALSE.equals(shexShapeSchema.isExists()) || shexShapeSchema.getBody() == null) {
                log.warn("Schema at {} doesn't exist or is empty", schemaUrl);
                continue;
            }

            String shapeBody = shexShapeSchema.getBody();
            try (InputStream stream = new ByteArrayInputStream(shapeBody.getBytes())) {
                ShExCParser shexCParser = new ShExCParser();
                ShexSchema schema = new ShexSchema(GlobalFactory.RDFFactory, shexCParser.getRules(stream), shexCParser.getStart());
                schemaCache.put(new URI(schemaUrl), schema);
            } catch (Exception ex) {
                log.error("Error parsing schema {}", schemaUrl);
                log.error("Exception:", ex);
            }
        }
        return schemaCache;
    }

}
