// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.tests
import { ShapeTreeAssignment } from '@shapetrees/core/src/ShapeTreeAssignment';
import { ShapeTreeManager } from '@shapetrees/core/src/ShapeTreeManager';
import { DocumentLoaderManager } from '@shapetrees/core/src/contentloaders/DocumentLoaderManager';
import { HttpExternalDocumentLoader } from '@shapetrees/core/src/contentloaders/HttpExternalDocumentLoader';
import { ShapeTreeException } from '@shapetrees/core/src/exceptions/ShapeTreeException';
import { GraphHelper } from '@shapetrees/core/src/helpers/GraphHelper';
import { DispatcherEntry } from './fixtures/DispatcherEntry';
import { RequestMatchingFixtureDispatcher } from './fixtures/RequestMatchingFixtureDispatcher';
import * as MockWebServer from 'okhttp3/mockwebserver';
import * as Graph from 'org/apache/jena/graph';
import * as MalformedURLException from 'java/net';
import * as URI from 'java/net';
import { toUrl } from './fixtures/MockWebServerHelper/toUrl';

class ShapeTreeManagerTests {

   private static managerUrl: URL;

   private static manager: ShapeTreeManager;

   private static server: MockWebServer;

   private static assignment1: ShapeTreeAssignmentprivate static assignment2: ShapeTreeAssignmentprivate static assignment3: ShapeTreeAssignmentprivate static nonContainingAssignment1: ShapeTreeAssignmentprivate static nonContainingAssignment2: ShapeTreeAssignmentprivate static containingAssignment1: ShapeTreeAssignment;

   private static dispatcher: RequestMatchingFixtureDispatcher = null;

   private static httpExternalDocumentLoader: HttpExternalDocumentLoader;

  public constructor() {
    httpExternalDocumentLoader = new HttpExternalDocumentLoader();
    DocumentLoaderManager.setLoader(httpExternalDocumentLoader);
  }

  // @BeforeAll
  static beforeAll(): void /* throws MalformedURLException, ShapeTreeException */ {
    dispatcher = new RequestMatchingFixtureDispatcher(List.of(new DispatcherEntry(List.of("shapetrees/manager-shapetree-ttl"), "GET", "/static/shapetrees/managers/shapetree", null)));
    server = new MockWebServer();
    server.setDispatcher(dispatcher);
    managerUrl = new URL("https://site.example/resource.shapetree");
    assignment1 = new ShapeTreeAssignment(new URL("https://tree.example/tree#TreeOne"), new URL("https://site.example/resource"), new URL("https://site.example/resource.shapetree#ln1"), new URL("https://site.example/resource#node"), new URL("https://shapes.example/schema#ShapeOne"), new URL("https://site.example/resource.shapetree#ln1"));
    assignment2 = new ShapeTreeAssignment(new URL("https://tree.example/tree#TreeTwo"), new URL("https://site.example/resource"), new URL("https://site.example/resource.shapetree#ln2"), new URL("https://site.example/resource#node"), new URL("https://shapes.example/schema#ShapeTwo"), new URL("https://site.example/resource.shapetree#ln2"));
    assignment3 = new ShapeTreeAssignment(new URL("https://tree.example/tree#TreeThree"), new URL("https://site.example/resource"), new URL("https://site.example/resource.shapetree#ln3"), new URL("https://site.example/resource#node"), new URL("https://shapes.example/schema#ShapeThree"), new URL("https://site.example/resource.shapetree#ln3"));
    nonContainingAssignment1 = new ShapeTreeAssignment(toUrl(server, "/static/shapetrees/managers/shapetree#NonContainingTree"), toUrl(server, "/data/container/"), toUrl(server, "/data/container/.shapetree#ln1"), null, null, toUrl(server, "/data/container/.shapetree#ln1"));
    containingAssignment1 = new ShapeTreeAssignment(toUrl(server, "/static/shapetrees/managers/shapetree#ContainingTree"), toUrl(server, "/data/container/"), toUrl(server, "/data/container/.shapetree#ln2"), null, null, toUrl(server, "/data/container/.shapetree#ln2"));
    nonContainingAssignment2 = new ShapeTreeAssignment(toUrl(server, "/static/shapetrees/managers/shapetree#NonContainingTree2"), toUrl(server, "/data/container/"), toUrl(server, "/data/container/.shapetree#ln3"), null, null, toUrl(server, "/data/container/.shapetree#ln3"));
  }

  // @BeforeEach
  beforeEach(): void {
    manager = new ShapeTreeManager(managerUrl);
  }

  // @SneakyThrows, @Test, @DisplayName("Initialize a new manager")
  initializeShapeTreeManager(): void {
    let newManager: ShapeTreeManager = new ShapeTreeManager(managerUrl);
    Assertions.assertNotNull(newManager);
    Assertions.assertEquals(newManager.getId(), managerUrl);
  }

  // @SneakyThrows, @Test, @DisplayName("Add a new assignment")
  addNewShapeTreeAssignmentToManager(): void {
    Assertions.assertTrue(manager.getAssignments().isEmpty());
    manager.addAssignment(assignment1);
    Assertions.assertFalse(manager.getAssignments().isEmpty());
    Assertions.assertEquals(manager.getAssignments().size(), 1);
  }

  // @SneakyThrows, @Test, @DisplayName("Fail to add a null assignment")
  failToAddNullAssignmentToManager(): void {
    Assertions.assertThrows(ShapeTreeException.class, () -> {
      manager.addAssignment(null);
    });
  }

  // @SneakyThrows, @Test, @DisplayName("Fail to add a duplicate assignment")
  failToAddDuplicateAssignment(): void {
    manager.addAssignment(assignment1);
    Assertions.assertThrows(ShapeTreeException.class, () -> {
      manager.addAssignment(assignment1);
    });
  }

  // @Test, @DisplayName("Fail to add assignment with certain null values")
  failToAddAssignmentWithBadValues(): void {
    Assertions.assertThrows(ShapeTreeException.class, () -> {
      new ShapeTreeAssignment(null, new URL("https://site.example/resource"), null, new URL("https://site.example/resource#node"), new URL("https://shapes.example/schema#ShapeThree"), new URL("https://site.example/resource.shapetree#ln3"));
    });
    Assertions.assertThrows(ShapeTreeException.class, () -> {
      // focus node with no shape
      new ShapeTreeAssignment(new URL("https://tree.example/tree#TreeThree"), new URL("https://site.example/resource"), new URL("https://site.example/resource.shapetree#ln3"), new URL("https://site.example/resource#node"), null, new URL("https://site.example/resource.shapetree#ln3"));
    });
    Assertions.assertThrows(ShapeTreeException.class, () -> {
      // shape with no focus node
      new ShapeTreeAssignment(new URL("https://tree.example/tree#TreeThree"), new URL("https://site.example/resource"), new URL("https://site.example/resource.shapetree#ln3"), null, new URL("https://shapes.example/schema#ShapeThree"), new URL("https://site.example/resource.shapetree#ln3"));
    });
  }

  // @SneakyThrows, @Test, @DisplayName("Fail to mint the same assignment twice")
  failToMintDuplicateAssignment(): void {
    manager.addAssignment(assignment1);
    let adjustedUrl: URL = manager.mintAssignmentUrl(assignment1.getUrl());
    Assertions.assertNotEquals(assignment1.getUrl(), adjustedUrl);
  }

  // @SneakyThrows, @Test, @DisplayName("Get containing shape tree assignment from shape tree manager")
  getContainingShapeTreeAssignmentsFromManager(): void {
    manager.addAssignment(nonContainingAssignment1);
    manager.addAssignment(containingAssignment1);
    Assertions.assertEquals(1, manager.getContainingAssignments().size());
    Assertions.assertTrue(manager.getContainingAssignments().contains(containingAssignment1));
    Assertions.assertFalse(manager.getContainingAssignments().contains(nonContainingAssignment1));
  }

  // @SneakyThrows, @Test, @DisplayName("Get no containing shape tree assignment for shape tree manager")
  getNoContainingShapeTreeAssignmentFromManager(): void {
    manager.addAssignment(nonContainingAssignment1);
    manager.addAssignment(nonContainingAssignment2);
    Assertions.assertTrue(manager.getContainingAssignments().isEmpty());
  }

  // @SneakyThrows, @Test, @DisplayName("Get no shape tree assignment for shape tree from manager with no assignments")
  getNoShapeTreeAssignmentsFromEmptyManager(): void {
    Assertions.assertNull(manager.getAssignmentForShapeTree(new URL("https://tree.example/shapetree#ExampleTree")));
  }

  // @SneakyThrows, @Test, @DisplayName("Get shape tree assignment from manager for shape tree")
  getShapeTreeAssignmentFromManagerForShapeTree(): void {
    manager.addAssignment(nonContainingAssignment1);
    manager.addAssignment(nonContainingAssignment2);
    manager.addAssignment(containingAssignment1);
    Assertions.assertEquals(containingAssignment1, manager.getAssignmentForShapeTree(containingAssignment1.getShapeTree()));
  }

  // @SneakyThrows, @Test, @DisplayName("Get no shape tree assignment from manager without matching shape tree")
  getNoShapeTreeAssignmentForShapeTree(): void {
    manager.addAssignment(nonContainingAssignment1);
    manager.addAssignment(nonContainingAssignment2);
    manager.addAssignment(containingAssignment1);
    Assertions.assertNull(manager.getAssignmentForShapeTree(new URL("https://tree.example/shapetree#ExampleTree")));
  }

  // @SneakyThrows, @Test, @DisplayName("Fail to remove assignment from empty manager")
  failToRemoveAssignmentFromEmptyManager(): void {
    Assertions.assertThrows(IllegalStateException.class, () -> {
      manager.removeAssignment(containingAssignment1);
    });
  }

  // @SneakyThrows, @Test, @DisplayName("Fail to remove assignment from empty manager")
  failToRemoveAssignmentMissingFromManager(): void {
    manager.addAssignment(nonContainingAssignment1);
    manager.addAssignment(nonContainingAssignment2);
    Assertions.assertThrows(IllegalStateException.class, () -> {
      manager.removeAssignment(containingAssignment1);
    });
  }

  // @SneakyThrows, @Test, @DisplayName("Remove assignment from manager")
  removeAssignmentFromManager(): void {
    manager.addAssignment(nonContainingAssignment1);
    manager.addAssignment(nonContainingAssignment2);
    manager.addAssignment(containingAssignment1);
    Assertions.assertEquals(manager.getAssignmentForShapeTree(containingAssignment1.getShapeTree()), containingAssignment1);
    manager.removeAssignment(containingAssignment1);
    Assertions.assertNull(manager.getAssignmentForShapeTree(containingAssignment1.getShapeTree()));
  }

  // @SneakyThrows, @Test, @DisplayName("Get valid assignment from graph")
  getAssignmentFromGraph(): void {
    let managerUri: URI = URI.create("https://data.example/container.shapetree");
    let managerGraph: Graph = GraphHelper.readStringIntoGraph(managerUri, getValidManagerString(), "text/turtle");
    let manager: ShapeTreeManager = ShapeTreeManager.getFromGraph(managerUri.toURL(), managerGraph);
    Assertions.assertNotNull(manager);
    Assertions.assertNotNull(manager.getAssignmentForShapeTree(new URL("https://tree.example/#Tree1")));
  }

  // @SneakyThrows, @Test, @DisplayName("Fail to get assignment from graph due to missing triples")
  failToGetAssignmentFromGraphMissingTriples(): void {
    let managerUri: URI = URI.create("https://data.example/container.shapetree");
    let managerGraph: Graph = GraphHelper.readStringIntoGraph(managerUri, getInvalidManagerMissingTriplesString(), "text/turtle");
    Assertions.assertThrows(IllegalStateException.class, () -> {
      ShapeTreeManager.getFromGraph(managerUrl, managerGraph);
    });
  }

  // @SneakyThrows, @Test, @DisplayName("Fail to get assignment from graph due to unexpected values")
  failToGetAssignmentFromGraphUnexpectedValues(): void {
    let managerUri: URI = URI.create("https://data.example/container.shapetree");
    let managerGraph: Graph = GraphHelper.readStringIntoGraph(managerUri, getInvalidManagerUnexpectedTriplesString(), "text/turtle");
    Assertions.assertThrows(IllegalStateException.class, () -> {
      ShapeTreeManager.getFromGraph(managerUrl, managerGraph);
    });
  }

  private getValidManagerString(): string {
    return "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \n" + "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \n" + "PREFIX xml: <http://www.w3.org/XML/1998/namespace> \n" + "PREFIX st: <http://www.w3.org/ns/shapetrees#> \n \n" + "PREFIX ex: <http://www.example.com/ns/ex#> \n" + "\n" + "\n" + "<https://data.example/container.shapetree> \n" + "    a st:Manager ; \n" + "    st:hasAssignment <https://data.example/container.shapetree#ln1> . \n" + "\n" + "<https://data.example/container.shapetree#ln1> \n" + "    st:assigns <https://tree.example/#Tree1> ; \n" + "    st:hasRootAssignment <https://data.example/container.shapetree#ln1> ; \n" + "    st:manages <https://data.example/container> ; \n" + "    st:shape <https://shapes.example/#Shape1> ; \n" + "    st:focusNode <https://data.example/container#node> . \n" + "\n";
  }

  private getInvalidManagerMissingTriplesString(): string {
    return "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \n" + "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \n" + "PREFIX xml: <http://www.w3.org/XML/1998/namespace> \n" + "PREFIX st: <http://www.w3.org/ns/shapetrees#> \n \n" + "PREFIX ex: <http://www.example.com/ns/ex#> \n" + "\n" + "\n" + "<https://data.example/container.shapetree> \n" + "    a st:Manager ; \n" + "    st:hasAssignment <https://data.example/container.shapetree#ln1> . \n" + "\n" + "<https://data.example/container.shapetree#ln1> \n" + "    st:assigns <https://tree.example/#Tree1> ; \n" + "\n";
  }

  private getInvalidManagerUnexpectedTriplesString(): string {
    return "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \n" + "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \n" + "PREFIX xml: <http://www.w3.org/XML/1998/namespace> \n" + "PREFIX st: <http://www.w3.org/ns/shapetrees#> \n \n" + "PREFIX ex: <http://www.example.com/ns/ex#> \n" + "\n" + "\n" + "<https://data.example/container.shapetree> \n" + "    a st:Manager ; \n" + "    st:hasAssignment <https://data.example/container.shapetree#ln1> . \n" + "\n" + "<https://data.example/container.shapetree#ln1> \n" + "    st:assigns <https://tree.example/#Tree1> ; \n" + "    st:hasRootAssignment <https://data.example/container.shapetree#ln1> ; \n" + "    st:manages <https://data.example/container> ; \n" + "    st:shape <https://shapes.example/#Shape1> ; \n" + "    st:focusNode <https://data.example/container#node> ; \n" + "    st:unexpected \"why am i here\" . \n" + "\n";
  }
}
