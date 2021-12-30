// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.tests
import { ShapeTreeFactory } from '@shapetrees/core/src/ShapeTreeFactory';
import { DocumentLoaderManager } from '@shapetrees/core/src/contentloaders/DocumentLoaderManager';
import { HttpExternalDocumentLoader } from '@shapetrees/core/src/contentloaders/HttpExternalDocumentLoader';
import { ShapeTree } from '@shapetrees/core/src/ShapeTree';
import { DispatcherEntry } from './fixtures/DispatcherEntry';
import { RequestMatchingFixtureDispatcher } from './fixtures/RequestMatchingFixtureDispatcher';
import * as MockWebServer from 'okhttp3/mockwebserver';
import * as Assertions from 'org/junit/jupiter/api';
import * as BeforeAll from 'org/junit/jupiter/api';
import * as DisplayName from 'org/junit/jupiter/api';
import * as Test from 'org/junit/jupiter/api';
import { toUrl } from './fixtures/MockWebServerHelper/toUrl';

class ShapeTreeContainsPriorityTests {

   private static dispatcher: RequestMatchingFixtureDispatcher = null;

   private static httpExternalDocumentLoader: HttpExternalDocumentLoader;

  public constructor() {
    httpExternalDocumentLoader = new HttpExternalDocumentLoader();
    DocumentLoaderManager.setLoader(httpExternalDocumentLoader);
  }

  // @BeforeAll
  static beforeAll(): void {
    dispatcher = new RequestMatchingFixtureDispatcher(List.of(new DispatcherEntry(List.of("shapetrees/contains-priority-shapetree-ttl"), "GET", "/static/shapetrees/contains-priority/shapetree", null)));
  }

  // @SneakyThrows, @Test, @DisplayName("Validate prioritized retrieval of all shape tree types")
  testContainsPriorityOrderOfAllTreeTypes(): void {
    let server: MockWebServer = new MockWebServer();
    server.setDispatcher(dispatcher);
    let containingShapeTree: ShapeTree = ShapeTreeFactory.getShapeTree(toUrl(server, "/static/shapetrees/contains-priority/shapetree#ContainsAllTypesTree"));
    // Ensure the ordered result is correct
    let prioritizedContains: Array<URL> = containingShapeTree.getPrioritizedContains();
    Assertions.assertEquals(3, prioritizedContains.size());
    Assertions.assertEquals(toUrl(server, "/static/shapetrees/contains-priority/shapetree#LabelShapeTypeTree"), prioritizedContains.get(0));
    Assertions.assertEquals(toUrl(server, "/static/shapetrees/contains-priority/shapetree#LabelTypeTree"), prioritizedContains.get(1));
    Assertions.assertEquals(toUrl(server, "/static/shapetrees/contains-priority/shapetree#TypeOnlyTree"), prioritizedContains.get(2));
  }

  // @SneakyThrows, @Test, @DisplayName("Validate prioritized retrieval of shape trees with shape and resource type validation")
  testContainsPriorityOrderOfShapeTypeTrees(): void {
    let server: MockWebServer = new MockWebServer();
    server.setDispatcher(dispatcher);
    let containingShapeTree: ShapeTree = ShapeTreeFactory.getShapeTree(toUrl(server, "/static/shapetrees/contains-priority/shapetree#ContainsShapeTypeTree"));
    // Ensure the ordered result is correct
    let prioritizedContains: Array<URL> = containingShapeTree.getPrioritizedContains();
    Assertions.assertEquals(2, prioritizedContains.size());
    Assertions.assertEquals(toUrl(server, "/static/shapetrees/contains-priority/shapetree#ShapeTypeTree"), prioritizedContains.get(0));
    Assertions.assertEquals(toUrl(server, "/static/shapetrees/contains-priority/shapetree#TypeOnlyTree"), prioritizedContains.get(1));
  }

  // @SneakyThrows, @Test, @DisplayName("Validate prioritized retrieval of shape tree trees with label and resource type validation")
  testContainsPriorityOrderOfLabelTypeTrees(): void {
    let server: MockWebServer = new MockWebServer();
    server.setDispatcher(dispatcher);
    let containingShapeTree: ShapeTree = ShapeTreeFactory.getShapeTree(toUrl(server, "/static/shapetrees/contains-priority/shapetree#ContainsLabelTypeTree"));
    // Ensure the ordered result is correct
    let prioritizedContains: Array<URL> = containingShapeTree.getPrioritizedContains();
    Assertions.assertEquals(2, prioritizedContains.size());
    Assertions.assertEquals(toUrl(server, "/static/shapetrees/contains-priority/shapetree#LabelTypeTree"), prioritizedContains.get(0));
    Assertions.assertEquals(toUrl(server, "/static/shapetrees/contains-priority/shapetree#TypeOnlyTree"), prioritizedContains.get(1));
  }
}
