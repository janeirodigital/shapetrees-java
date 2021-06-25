package com.janeirodigital.shapetrees.client.okhttp;

import com.janeirodigital.shapetrees.client.okhttp.fixtures.DispatcherEntry;
import com.janeirodigital.shapetrees.client.okhttp.fixtures.RequestMatchingFixtureDispatcher;
import com.janeirodigital.shapetrees.core.models.ShapeTreeLocation;
import com.janeirodigital.shapetrees.core.models.ShapeTreeLocator;
import jdk.jfr.Label;
import lombok.SneakyThrows;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.jupiter.api.*;

import java.util.ArrayList;
import java.util.List;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class DiscoverTests extends BaseShapeTreeTest {

    private static RequestMatchingFixtureDispatcher dispatcher = null;

    public DiscoverTests() {
        // Call BaseShapeTreeTest constructor
        super();
    }

    @BeforeAll
    static void beforeAll() {

        List dispatcherList = new ArrayList();

        dispatcherList.add(new DispatcherEntry(List.of("discover/unmanaged"), "GET", "/unmanaged", null));
        dispatcherList.add(new DispatcherEntry(List.of("discover/unmanaged-locator"), "GET", "/unmanaged.shapetree", null));
        dispatcherList.add(new DispatcherEntry(List.of("discover/managed"), "GET", "/managed", null));
        dispatcherList.add(new DispatcherEntry(List.of("discover/managed-locator"), "GET", "/managed.shapetree", null));
        dispatcherList.add(new DispatcherEntry(List.of("discover/managed-invalid-1"), "GET", "/managed-invalid-1", null));
        dispatcherList.add(new DispatcherEntry(List.of("discover/managed-invalid-1-locator"), "GET", "/managed-invalid-1.shapetree", null));
        dispatcherList.add(new DispatcherEntry(List.of("discover/managed-invalid-2"), "GET", "/managed-invalid-2", null));
        dispatcherList.add(new DispatcherEntry(List.of("discover/managed-invalid-2-locator"), "GET", "/managed-invalid-2.shapetree", null));

        dispatcher = new RequestMatchingFixtureDispatcher(dispatcherList);
    }

    @Order(1)
    @SneakyThrows
    @Test
    @Label("Discover unmanaged resource")
    void discoverUnmanagedResource() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Use the discover operation to see if the root container is managed
        ShapeTreeLocator locator = this.shapeTreeClient.discoverShapeTree(this.context, getURI(server, "/unmanaged"));

        // The root container isn't managed so check to ensure that a NULL value is returned
        Assertions.assertNull(locator);
    }

    @Order(2)
    @SneakyThrows
    @Test
    @Label("Discover managed resource")
    void discoverManagedResource() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Perform a discover on a resource that has a shape tree locator already planted
        ShapeTreeLocator locator = this.shapeTreeClient.discoverShapeTree(this.context, getURI(server, "/managed"));

        // Ensure that it was planted successfully
        Assertions.assertNotNull(locator);
        Assertions.assertEquals(locator.getLocations().size(), 1);

        ShapeTreeLocation location = locator.getLocations().get(0);

        Assertions.assertEquals(location.getShapeTree(), "http://www.example/ns/ex#DataTree");
        Assertions.assertEquals(location.getRootShapeTree(), "http://www.example/ns/ex#DataTree");
        Assertions.assertEquals(location.getRootShapeTreeInstance(), getURI(server, "/managed").toString());
        Assertions.assertEquals(location.getFocusNode(), getURI(server, "/managed").toString() + "#set");
        Assertions.assertEquals(location.getShape(), "http://www.example/ns/ex#DataSetShape");

    }

    @Order(3)
    @SneakyThrows
    @Test
    @Label("Fail to discover managed resource with multiple locators")
    void failToDiscoverDueToMultipleLocators() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // If a locator resource has multiple shapetree locators it is considered invalid
        Assertions.assertThrows(IllegalStateException.class, () -> {
            ShapeTreeLocator locator = this.shapeTreeClient.discoverShapeTree(this.context, getURI(server, "/managed-invalid-1"));
        });

    }

    @Order(4)
    @SneakyThrows
    @Test
    @Label("Fail to discover managed resource with no locators")
    void failToDiscoverDueToNoLocators() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // If a locator resource exists, but has no locators it is considered invalid
        Assertions.assertThrows(IllegalStateException.class, () -> {
            ShapeTreeLocator locator = this.shapeTreeClient.discoverShapeTree(this.context, getURI(server, "/managed-invalid-2"));
        });

    }

}
