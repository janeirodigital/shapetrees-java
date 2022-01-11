package com.janeirodigital.shapetrees.client.okhttp;

import com.janeirodigital.shapetrees.core.ShapeTreeAssignment;
import com.janeirodigital.shapetrees.core.ShapeTreeContext;
import com.janeirodigital.shapetrees.core.ShapeTreeManager;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.tests.fixtures.RequestMatchingFixtureDispatcher;
import okhttp3.OkHttpClient;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.jupiter.api.*;

import java.net.MalformedURLException;
import java.net.URL;

import static com.janeirodigital.shapetrees.client.okhttp.OkHttpShapeTreeClient.discover;
import static com.janeirodigital.shapetrees.tests.fixtures.DispatcherHelper.mockOnGet;
import static com.janeirodigital.shapetrees.tests.fixtures.MockWebServerHelper.toUrl;
import static org.junit.jupiter.api.Assertions.assertNull;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class OkHttpShapeTreeClientDiscoverTests {

    private static MockWebServer server;
    private static ShapeTreeContext context;
    private static OkHttpClient okHttpClient;
    private static RequestMatchingFixtureDispatcher dispatcher;

    @BeforeAll
    static void beforeAll() throws ShapeTreeException {

        dispatcher = new RequestMatchingFixtureDispatcher();
        server = new MockWebServer();
        server.setDispatcher(dispatcher);

        context = new ShapeTreeContext(null);
        okHttpClient = OkHttpValidatingClientFactory.get();

    }

    @Test
    @DisplayName("Discover unmanaged resource")
    void discoverUnmanagedResource() throws ShapeTreeException {
        mockOnGet(dispatcher, "/unmanaged", "discover/unmanaged");
        mockOnGet(dispatcher, "/unmanaged.shapetree", "http/404");
        // Use the discover operation to see if the root container is managed
        // The root container isn't managed so check to ensure that a NULL value is returned
        URL targetResource = toUrl(server, "/unmanaged");
        ShapeTreeManager manager = discover(okHttpClient, context, targetResource);
        assertNull(manager);
    }

    @Test
    @DisplayName("Discover managed resource")
    void discoverManagedResource() throws MalformedURLException, ShapeTreeException {
        mockOnGet(dispatcher, "/managed", "discover/managed");
        mockOnGet(dispatcher, "/managed.shapetree", "discover/managed-manager");

        URL targetResource = toUrl(server, "/managed");
        // Perform a discover on a resource that has a shape tree manager already planted
        ShapeTreeManager manager = discover(okHttpClient, context, targetResource);
        // Ensure that it was planted successfully
        Assertions.assertNotNull(manager);
        Assertions.assertEquals(1, manager.getAssignments().size());
        ShapeTreeAssignment assignment = manager.getAssignments().get(0);
        Assertions.assertEquals(new URL("http://www.example.com/ns/ex#DataTree"), assignment.getShapeTree());
        Assertions.assertEquals(targetResource.toString(), assignment.getManagedResource().toString());
        Assertions.assertEquals(assignment.getUrl(), assignment.getRootAssignment());
        Assertions.assertEquals(toUrl(server, "/managed") + "#set", assignment.getFocusNode().toString());
        Assertions.assertEquals("http://www.example.com/ns/ex#DataSetShape", assignment.getShape().toString());

    }

    @Test
    @DisplayName("Fail to discover managed resource with multiple managers")
    void failToDiscoverDueToMultipleManagers() {
        mockOnGet(dispatcher, "/managed-invalid-1", "discover/managed-invalid-1");
        mockOnGet(dispatcher, "/managed-invalid-1.shapetree", "discover/managed-invalid-1-manager");

        URL targetResource = toUrl(server, "/managed-invalid-1");
        // If a manager resource has multiple shapetree managers it is considered invalid
        Assertions.assertThrows(IllegalStateException.class, () -> { discover(okHttpClient, context, targetResource); });
    }

    @Test
    @DisplayName("Fail to discover managed resource with no managers")
    void failToDiscoverDueToNoManagers() {
        mockOnGet(dispatcher, "/managed-invalid-2", "discover/managed-invalid-2");
        mockOnGet(dispatcher, "/managed-invalid-2.shapetree", "discover/managed-invalid-2-manager");

        URL targetResource = toUrl(server, "/managed-invalid-2");
        // If a manager resource exists, but has no managers it is considered invalid
        Assertions.assertThrows(IllegalStateException.class, () -> { discover(okHttpClient, context, targetResource); });
    }

    @Test
    @DisplayName("Discover server doesn't support ShapeTrees")
    void failToDiscoverDueToNoManagerLink() {
        mockOnGet(dispatcher, "/no-manager", "discover/no-manager");

        URL targetResource = toUrl(server, "/no-manager");
        // If a manager resource exists, but has no managers it is considered invalid
        Assertions.assertThrows(ShapeTreeException.class, () -> { discover(okHttpClient, context, targetResource); });
    }

}
