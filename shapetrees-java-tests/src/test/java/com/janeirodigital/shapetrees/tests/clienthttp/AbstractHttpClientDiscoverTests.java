package com.janeirodigital.shapetrees.tests.clienthttp;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.ShapeTreeAssignment;
import com.janeirodigital.shapetrees.core.models.ShapeTreeManager;
import com.janeirodigital.shapetrees.tests.fixtures.DispatcherEntry;
import com.janeirodigital.shapetrees.tests.fixtures.RequestMatchingFixtureDispatcher;
import jdk.jfr.Label;
import lombok.SneakyThrows;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.jupiter.api.*;

import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class AbstractHttpClientDiscoverTests extends AbstractHttpClientTests {

    private static RequestMatchingFixtureDispatcher dispatcher = null;

    public AbstractHttpClientDiscoverTests() {
        // Call AbstractHttpClient constructor
        super();
    }

    @BeforeAll
    static void beforeAll() {

        List dispatcherList = new ArrayList();

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

    @Order(1)
    @SneakyThrows
    @Test
    @Label("Discover unmanaged resource")
    void discoverUnmanagedResource() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        URL targetResource = toUrl(server, "/unmanaged");

        // Use the discover operation to see if the root container is managed
        ShapeTreeManager manager = this.shapeTreeClient.discoverShapeTree(this.context, targetResource).orElse(null);

        // The root container isn't managed so check to ensure that a NULL value is returned
        Assertions.assertNull(manager);
    }

    @Order(2)
    @SneakyThrows
    @Test
    @Label("Discover managed resource")
    void discoverManagedResource() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        URL targetResource = toUrl(server, "/managed");

        // Perform a discover on a resource that has a shape tree manager already planted
        ShapeTreeManager manager = this.shapeTreeClient.discoverShapeTree(this.context, targetResource).orElse(null);

        // Ensure that it was planted successfully
        Assertions.assertNotNull(manager);
        Assertions.assertEquals(1, manager.getAssignments().size());

        ShapeTreeAssignment assignment = manager.getAssignments().get(0);

        Assertions.assertEquals(new URL("http://www.example.com/ns/ex#DataTree"), assignment.getShapeTree());
        Assertions.assertEquals(targetResource.toString(), assignment.getManagedResource().toString());
        Assertions.assertEquals(assignment.getUrl(), assignment.getRootAssignment());
        Assertions.assertEquals(toUrl(server, "/managed").toString() + "#set", assignment.getFocusNode().toString());
        Assertions.assertEquals("http://www.example.com/ns/ex#DataSetShape", assignment.getShape().toString());

    }

    @Order(3)
    @SneakyThrows
    @Test
    @Label("Fail to discover managed resource with multiple managers")
    void failToDiscoverDueToMultipleManagers() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        URL targetResource = toUrl(server, "/managed-invalid-1");

        // If a manager resource has multiple shapetree managers it is considered invalid
        Assertions.assertThrows(IllegalStateException.class, () -> {
            Optional<ShapeTreeManager> manager = this.shapeTreeClient.discoverShapeTree(this.context, targetResource);
        });

    }

    @Order(4)
    @SneakyThrows
    @Test
    @Label("Fail to discover managed resource with no managers")
    void failToDiscoverDueToNoManagers() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        URL targetResource = toUrl(server, "/managed-invalid-2");

        // If a manager resource exists, but has no managers it is considered invalid
        Assertions.assertThrows(IllegalStateException.class, () -> {
            Optional<ShapeTreeManager> manager = this.shapeTreeClient.discoverShapeTree(this.context, targetResource);
        });

    }

    @Order(5)
    @SneakyThrows
    @Test
    @Label("Discover server doesn't support ShapeTrees")
    void failToDiscoverDueToNoManagerLink() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        URL targetResource = toUrl(server, "/no-manager");

        // If a manager resource exists, but has no managers it is considered invalid
        Assertions.assertThrows(ShapeTreeException.class, () -> {
            Optional<ShapeTreeManager> manager = this.shapeTreeClient.discoverShapeTree(this.context, targetResource);
        });
    }
}
