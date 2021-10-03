package com.janeirodigital.shapetrees.javahttp;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.ShapeTreeLocation;
import com.janeirodigital.shapetrees.core.models.ShapeTreeLocator;
import com.janeirodigital.shapetrees.javahttp.fixtures.DispatcherEntry;
import com.janeirodigital.shapetrees.javahttp.fixtures.RequestMatchingFixtureDispatcher;
import jdk.jfr.Label;
import lombok.SneakyThrows;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.jupiter.api.*;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class DiscoverTests extends com.janeirodigital.shapetrees.javahttp.BaseShapeTreeTest {

    private static com.janeirodigital.shapetrees.javahttp.fixtures.RequestMatchingFixtureDispatcher dispatcher = null;

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
        dispatcherList.add(new DispatcherEntry(List.of("discover/no-locator"), "GET", "/no-locator", null));

        dispatcher = new RequestMatchingFixtureDispatcher(dispatcherList);
    }

    @Order(1)
    @SneakyThrows
    @Test
    @Label("Discover unmanaged resource")
    void discoverUnmanagedResource() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        URI targetResource = getURI(server, "/unmanaged");

        // Use the discover operation to see if the root container is managed
        ShapeTreeLocator locator = this.shapeTreeClient.discoverShapeTree(this.context, targetResource).orElse(null);

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

        URI targetResource = getURI(server, "/managed");

        // Perform a discover on a resource that has a shape tree locator already planted
        ShapeTreeLocator locator = this.shapeTreeClient.discoverShapeTree(this.context, targetResource).orElse(null);

        // Ensure that it was planted successfully
        Assertions.assertNotNull(locator);
        Assertions.assertEquals(1, locator.getLocations().size());

        ShapeTreeLocation location = locator.getLocations().get(0);

        Assertions.assertEquals("http://www.example.com/ns/ex#DataTree", location.getShapeTree());
        Assertions.assertEquals(targetResource.toString(), location.getManagedResource());
        Assertions.assertEquals(location.getUri(), location.getRootShapeTreeLocation());
        Assertions.assertEquals(getURI(server, "/managed").toString() + "#set", location.getFocusNode());
        Assertions.assertEquals("http://www.example.com/ns/ex#DataSetShape", location.getShape());

    }

    @Order(3)
    @SneakyThrows
    @Test
    @Label("Fail to discover managed resource with multiple locators")
    void failToDiscoverDueToMultipleLocators() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        URI targetResource = getURI(server, "/managed-invalid-1");

        // If a locator resource has multiple shapetree locators it is considered invalid
        Assertions.assertThrows(IllegalStateException.class, () -> {
            ShapeTreeLocator locator = this.shapeTreeClient.discoverShapeTree(this.context, targetResource).orElse(null);
        });

    }

    @Order(4)
    @SneakyThrows
    @Test
    @Label("Fail to discover managed resource with no locators")
    void failToDiscoverDueToNoLocators() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        URI targetResource = getURI(server, "/managed-invalid-2");

        // If a locator resource exists, but has no locators it is considered invalid
        Assertions.assertThrows(IllegalStateException.class, () -> {
            ShapeTreeLocator locator = this.shapeTreeClient.discoverShapeTree(this.context, targetResource).orElse(null);
        });

    }

    @Order(5)
    @SneakyThrows
    @Test
    @Label("Discover server doesn't support ShapeTrees")
    void failToDiscoverDueToNoLocatorLink() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        URI targetResource = getURI(server, "/no-locator");

        // If a locator resource exists, but has no locators it is considered invalid
        Assertions.assertThrows(ShapeTreeException.class, () -> {
            ShapeTreeLocator locator = this.shapeTreeClient.discoverShapeTree(this.context, targetResource).orElse(null);
        });
    }
}
