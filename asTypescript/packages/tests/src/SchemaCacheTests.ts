// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.tests
import { DocumentResponse } from '@shapetrees/core/src/DocumentResponse';
import { SchemaCache } from '@shapetrees/core/src/SchemaCache';
import { DocumentLoaderManager } from '@shapetrees/core/src/contentloaders/DocumentLoaderManager';
import { HttpExternalDocumentLoader } from '@shapetrees/core/src/contentloaders/HttpExternalDocumentLoader';
import { ShapeTreeException } from '@shapetrees/core/src/exceptions/ShapeTreeException';
import { DispatcherEntry } from './fixtures/DispatcherEntry';
import { RequestMatchingFixtureDispatcher } from './fixtures/RequestMatchingFixtureDispatcher';
import * as GlobalFactory from 'fr/inria/lille/shexjava';
import * as ShexSchema from 'fr/inria/lille/shexjava/schema';
import * as ShExCParser from 'fr/inria/lille/shexjava/schema/parsing';
import * as MockWebServer from 'okhttp3/mockwebserver';
import * as MalformedURLException from 'java/net';
import { toUrl } from './fixtures/MockWebServerHelper/toUrl';
import * as assertFalse from 'org/junit/jupiter/api/Assertions';
import * as assertTrue from 'org/junit/jupiter/api/Assertions';

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
export class SchemaCacheTests {

   private static dispatcher: RequestMatchingFixtureDispatcher = null;

   private static httpExternalDocumentLoader: HttpExternalDocumentLoader;

  public constructor() {
    httpExternalDocumentLoader = new HttpExternalDocumentLoader();
    DocumentLoaderManager.setLoader(httpExternalDocumentLoader);
  }

  // @BeforeAll
  static beforeAll(): void /* throws ShapeTreeException */ {
    dispatcher = new RequestMatchingFixtureDispatcher(List.of(new DispatcherEntry(List.of("schemas/project-shex"), "GET", "/static/shex/project", null)));
    SchemaCache.unInitializeCache();
  }

  // @Test, @Order(1)
  testFailToOperateOnUninitializedCache(): void /* throws MalformedURLException, ShapeTreeException */ {
    assertFalse(SchemaCache.isInitialized());
    // containsSchema
    let containsException: Throwable = Assertions.assertThrows(ShapeTreeException.class, () -> SchemaCache.containsSchema(new URL("http://schema.example")));
    Assertions.assertEquals(SchemaCache.CACHE_IS_NOT_INITIALIZED, containsException.getMessage());
    // getSchema
    let getException: Throwable = Assertions.assertThrows(ShapeTreeException.class, () -> SchemaCache.getSchema(new URL("http://schema.example")));
    Assertions.assertEquals(SchemaCache.CACHE_IS_NOT_INITIALIZED, getException.getMessage());
    // putSchema
    let putException: Throwable = Assertions.assertThrows(ShapeTreeException.class, () -> SchemaCache.putSchema(new URL("http://schema.example"), null));
    Assertions.assertEquals(SchemaCache.CACHE_IS_NOT_INITIALIZED, putException.getMessage());
    // clearSchema
    let clearException: Throwable = Assertions.assertThrows(ShapeTreeException.class, () -> SchemaCache.clearCache());
    Assertions.assertEquals(SchemaCache.CACHE_IS_NOT_INITIALIZED, clearException.getMessage());
  }

  // @Test, @Order(2)
  testInitializeCache(): void /* throws MalformedURLException, ShapeTreeException */ {
    SchemaCache.initializeCache();
    assertTrue(SchemaCache.isInitialized());
    assertFalse(SchemaCache.containsSchema(new URL("http://schema.example")));
  }

  // @Test, @Order(3)
  testPreloadCache(): void /* throws MalformedURLException, ShapeTreeException */ {
    let server: MockWebServer = new MockWebServer();
    server.setDispatcher(dispatcher);
    let schemas: Map<URL, ShexSchema> = buildSchemaCache(List.of(toUrl(server, "/static/shex/project").toString()));
    SchemaCache.initializeCache(schemas);
    assertTrue(SchemaCache.containsSchema(toUrl(server, "/static/shex/project")));
  }

  // @Test, @Order(4)
  testClearPutGet(): void /* throws MalformedURLException, ShapeTreeException */ {
    let server: MockWebServer = new MockWebServer();
    server.setDispatcher(dispatcher);
    SchemaCache.clearCache();
    Assertions.assertNull(SchemaCache.getSchema(toUrl(server, "/static/shex/project")));
    let schemas: Map<URL, ShexSchema> = buildSchemaCache(List.of(toUrl(server, "/static/shex/project").toString()));
    let firstEntry: Map.Entry<URL, ShexSchema> = schemas.entrySet().stream().findFirst().orElse(null);
    if (firstEntry === null)
      return;
    SchemaCache.putSchema(firstEntry.getKey(), firstEntry.getValue());
    Assertions.assertNotNull(SchemaCache.getSchema(toUrl(server, "/static/shex/project")));
  }

  // @Test, @Order(5)
  testNullOnCacheContains(): void /* throws MalformedURLException, ShapeTreeException */ {
    let server: MockWebServer = new MockWebServer();
    server.setDispatcher(dispatcher);
    SchemaCache.clearCache();
    Assertions.assertNull(SchemaCache.getSchema(toUrl(server, "/static/shex/project")));
    let schemas: Map<URL, ShexSchema> = buildSchemaCache(List.of(toUrl(server, "/static/shex/project").toString()));
    let firstEntry: Map.Entry<URL, ShexSchema> = schemas.entrySet().stream().findFirst().orElse(null);
    if (firstEntry === null)
      return;
    SchemaCache.putSchema(firstEntry.getKey(), firstEntry.getValue());
    Assertions.assertNotNull(SchemaCache.getSchema(toUrl(server, "/static/shex/project")));
  }

  public static buildSchemaCache(schemasToCache: Array<string>): Map<URL, ShexSchema> /* throws MalformedURLException, ShapeTreeException */ {
    let schemaCache: Map<URL, ShexSchema> = new Map<>();
    log.info("Building schema cache");
    for (const schemaUrl of schemasToCache) {
      log.debug("Caching schema {}", schemaUrl);
      let shexShapeSchema: DocumentResponse = DocumentLoaderManager.getLoader().loadExternalDocument(new URL(schemaUrl));
      if (Boolean.FALSE === shexShapeSchema.isExists() || shexShapeSchema.getBody() === null) {
        log.warn("Schema at {} doesn't exist or is empty", schemaUrl);
        continue;
      }
      let shapeBody: string = shexShapeSchema.getBody();
      try (let stream: InputStream = new ByteArrayInputStream(shapeBody.getBytes())) {
        let shexCParser: ShExCParser = new ShExCParser();
        let schema: ShexSchema = new ShexSchema(GlobalFactory.RDFFactory, shexCParser.getRules(stream), shexCParser.getStart());
        schemaCache.put(new URL(schemaUrl), schema);
      } catch (ex) {
 if (ex instanceof Exception) {
        log.error("Error parsing schema {}", schemaUrl);
        log.error("Exception:", ex);
      }
}
    }
    return schemaCache;
  }
}
