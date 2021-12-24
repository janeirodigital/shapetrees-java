// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.tests.clienthttp
import { ShapeTreeException } from '@shapetrees/core/src/exceptions/ShapeTreeException';
import { ShapeTreeAssignment } from '@shapetrees/core/src/ShapeTreeAssignment';
import { ShapeTreeManager } from '@shapetrees/core/src/ShapeTreeManager';
import { DispatcherEntry } from '../fixtures/DispatcherEntry';
import { RequestMatchingFixtureDispatcher } from '../fixtures/RequestMatchingFixtureDispatcher';
import * as Label from 'jdk/jfr';
import * as MockWebServer from 'okhttp3/mockwebserver';
import { AbstractHttpClientTests } from './AbstractHttpClientTests';

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
export class AbstractHttpClientDiscoverTests extends AbstractHttpClientTests {

   private static dispatcher: RequestMatchingFixtureDispatcher = null;

  public constructor() {
    // Call AbstractHttpClient constructor
    super();
  }

  // @BeforeAll
  static beforeAll(): void {
    let dispatcherList: Array = new Array();
    dispatcherList.add(new DispatcherEntry(List.of("discover/unmanaged"), "GET", "/unmanaged", null));
    dispatcherList.add(new DispatcherEntry(List.of("discover/unmanaged-manager"), "GET", "/unmanaged.shapetree", null));
    dispatcherList.add(new DispatcherEntry(List.of("discover/managed"), "GET", "/managed", null));
    dispatcherList.add(new DispatcherEntry(List.of("discover/managed-manager"), "GET", "/managed.shapetree", null));
    dispatcherList.add(new DispatcherEntry(List.of("discover/managed-invalid-1"), "GET", "/managed-invalid-1", null));
    dispatcherList.add(new DispatcherEntry(List.of("discover/managed-invalid-1-manager"), "GET", "/managed-invalid-1.shapetree", null));
    dispatcherList.add(new DispatcherEntry(List.of("discover/managed-invalid-2"), "GET", "/managed-invalid-2", null));
    dispatcherList.add(new DispatcherEntry(List.of("discover/managed-invalid-2-manager"), "GET", "/managed-invalid-2.shapetree", null));
    dispatcherList.add(new DispatcherEntry(List.of("discover/no-manager"), "GET", "/no-manager", null));
    dispatcher = new RequestMatchingFixtureDispatcher(dispatcherList);
  }

  // @Order(1), @SneakyThrows, @Test, @Label("Discover unmanaged resource")
  discoverUnmanagedResource(): void {
    let server: MockWebServer = new MockWebServer();
    server.setDispatcher(dispatcher);
    let targetResource: URL = toUrl(server, "/unmanaged");
    // Use the discover operation to see if the root container is managed
    let manager: ShapeTreeManager = this.shapeTreeClient.discoverShapeTree(this.context, targetResource).orElse(null);
    // The root container isn't managed so check to ensure that a NULL value is returned
    Assertions.assertNull(manager);
  }

  // @Order(2), @SneakyThrows, @Test, @Label("Discover managed resource")
  discoverManagedResource(): void {
    let server: MockWebServer = new MockWebServer();
    server.setDispatcher(dispatcher);
    let targetResource: URL = toUrl(server, "/managed");
    // Perform a discover on a resource that has a shape tree manager already planted
    let manager: ShapeTreeManager = this.shapeTreeClient.discoverShapeTree(this.context, targetResource).orElse(null);
    // Ensure that it was planted successfully
    Assertions.assertNotNull(manager);
    Assertions.assertEquals(1, manager.getAssignments().size());
    let assignment: ShapeTreeAssignment = manager.getAssignments().get(0);
    Assertions.assertEquals(new URL("http://www.example.com/ns/ex#DataTree"), assignment.getShapeTree());
    Assertions.assertEquals(targetResource.toString(), assignment.getManagedResource().toString());
    Assertions.assertEquals(assignment.getUrl(), assignment.getRootAssignment());
    Assertions.assertEquals(toUrl(server, "/managed").toString() + "#set", assignment.getFocusNode().toString());
    Assertions.assertEquals("http://www.example.com/ns/ex#DataSetShape", assignment.getShape().toString());
  }

  // @Order(3), @SneakyThrows, @Test, @Label("Fail to discover managed resource with multiple managers")
  failToDiscoverDueToMultipleManagers(): void {
    let server: MockWebServer = new MockWebServer();
    server.setDispatcher(dispatcher);
    let targetResource: URL = toUrl(server, "/managed-invalid-1");
    // If a manager resource has multiple shapetree managers it is considered invalid
    Assertions.assertThrows(IllegalStateException.class, () -> {
      let manager: ShapeTreeManager | null = this.shapeTreeClient.discoverShapeTree(this.context, targetResource);
    });
  }

  // @Order(4), @SneakyThrows, @Test, @Label("Fail to discover managed resource with no managers")
  failToDiscoverDueToNoManagers(): void {
    let server: MockWebServer = new MockWebServer();
    server.setDispatcher(dispatcher);
    let targetResource: URL = toUrl(server, "/managed-invalid-2");
    // If a manager resource exists, but has no managers it is considered invalid
    Assertions.assertThrows(IllegalStateException.class, () -> {
      let manager: ShapeTreeManager | null = this.shapeTreeClient.discoverShapeTree(this.context, targetResource);
    });
  }

  // @Order(5), @SneakyThrows, @Test, @Label("Discover server doesn't support ShapeTrees")
  failToDiscoverDueToNoManagerLink(): void {
    let server: MockWebServer = new MockWebServer();
    server.setDispatcher(dispatcher);
    let targetResource: URL = toUrl(server, "/no-manager");
    // If a manager resource exists, but has no managers it is considered invalid
    Assertions.assertThrows(ShapeTreeException.class, () -> {
      let manager: ShapeTreeManager | null = this.shapeTreeClient.discoverShapeTree(this.context, targetResource);
    });
  }
}
