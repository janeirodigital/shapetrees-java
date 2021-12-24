// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.tests
import { DocumentResponse } from '@shapetrees/core/src/DocumentResponse';
import { DocumentLoaderManager } from '@shapetrees/core/src/contentloaders/DocumentLoaderManager';
import { HttpExternalDocumentLoader } from '@shapetrees/core/src/contentloaders/HttpExternalDocumentLoader';
import { ShapeTreeException } from '@shapetrees/core/src/exceptions/ShapeTreeException';
import { DispatcherEntry } from './fixtures/DispatcherEntry';
import { RequestMatchingFixtureDispatcher } from './fixtures/RequestMatchingFixtureDispatcher';
import * as MockWebServer from 'okhttp3/mockwebserver';
import * as MalformedURLException from 'java/net';

class HttpDocumentLoaderTests {

   private static dispatcher: RequestMatchingFixtureDispatcher = null;

   private static httpExternalDocumentLoader: HttpExternalDocumentLoader;

  public constructor() {
    httpExternalDocumentLoader = new HttpExternalDocumentLoader();
    DocumentLoaderManager.setLoader(httpExternalDocumentLoader);
  }

  protected getURL(server: MockWebServer, path: string): URL /* throws MalformedURLException */ {
    return new URL(server.url(path).toString());
  }

  // @BeforeAll
  static beforeAll(): void {
    dispatcher = new RequestMatchingFixtureDispatcher(List.of(new DispatcherEntry(List.of("shapetrees/validation-shapetree-ttl"), "GET", "/static/shapetrees/validation/shapetree", null), new DispatcherEntry(List.of("http/404"), "GET", "/static/shex/missing", null)));
  }

  // @AfterAll
  static clearDocumentManager(): void {
    DocumentLoaderManager.setLoader(null);
  }

  // @Test, @DisplayName("Fail to load missing document over http")
  failToLoadMissingHttpDocument(): void {
    let server: MockWebServer = new MockWebServer();
    server.setDispatcher(dispatcher);
    Assertions.assertThrows(ShapeTreeException.class, () -> {
      httpExternalDocumentLoader.loadExternalDocument(getURL(server, "/static/shex/missing"));
    });
  }

  // @SneakyThrows, @Test, @DisplayName("Successfully load shape tree document over http")
  loadHttpDocument(): void {
    let server: MockWebServer = new MockWebServer();
    server.setDispatcher(dispatcher);
    let shapeTreeDocument: DocumentResponse = httpExternalDocumentLoader.loadExternalDocument(getURL(server, "/static/shapetrees/validation/shapetree"));
    Assertions.assertNotNull(shapeTreeDocument);
    Assertions.assertEquals(200, shapeTreeDocument.getStatusCode());
    Assertions.assertTrue(shapeTreeDocument.isExists());
    Assertions.assertNotNull(shapeTreeDocument.getBody());
    Assertions.assertNotNull(shapeTreeDocument.getResourceAttributes());
  }

  // @SneakyThrows, @Test, @DisplayName("Successfully handle thread interruption")
  handleInterruptedThreadOnLoadHttpDocument(): void {
    let server: MockWebServer = new MockWebServer();
    server.setDispatcher(dispatcher);
    Thread.currentThread().interrupt();
    Assertions.assertThrows(ShapeTreeException.class, () -> {
      let shapeTreeDocument: DocumentResponse = httpExternalDocumentLoader.loadExternalDocument(getURL(server, "/static/shapetrees/validation/shapetree"));
    });
  }
}
