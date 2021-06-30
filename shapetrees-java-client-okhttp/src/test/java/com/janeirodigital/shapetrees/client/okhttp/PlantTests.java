package com.janeirodigital.shapetrees.client.okhttp;

import com.janeirodigital.shapetrees.client.okhttp.fixtures.DispatcherEntry;
import com.janeirodigital.shapetrees.client.okhttp.fixtures.RequestMatchingFixtureDispatcher;
import jdk.jfr.Label;
import lombok.SneakyThrows;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.jupiter.api.*;

import java.util.ArrayList;
import java.util.List;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class PlantTests extends BaseShapeTreeTest {

    private static RequestMatchingFixtureDispatcher dispatcher = null;

    public PlantTests() {
        // Call BaseShapeTreeTest constructor
        super();
    }

    @BeforeAll
    static void beforeAll() {

        List dispatcherList = new ArrayList();

        dispatcherList.add(new DispatcherEntry(List.of("plant/unmanaged"), "GET", "/unmanaged", null));
        dispatcherList.add(new DispatcherEntry(List.of("plant/unmanaged-locator"), "GET", "/unmanaged", null));

        dispatcher = new RequestMatchingFixtureDispatcher(dispatcherList);

    }

    @Order(1)
    @SneakyThrows
    @Test
    @Label("Plant on an unmanaged resource")
    void plantShapeTreeOnUnmanagedResource() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

    }

    @Order(2)
    @SneakyThrows
    @Test
    @Label("Plant another shape tree on a managed resource")
    void plantAnotherShapeTreeOnManagedResource() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

    }

    @Order(3)
    @SneakyThrows
    @Test
    @Label("Plant shape on an unmanaged resource")
    void plantShapeOnUnmanagedResource() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

    }

    @Order(4)
    @SneakyThrows
    @Test
    @Label("Plant shape on a managed resource")
    void plantShapeOnManagedResource() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

    }

    @Order(5)
    @SneakyThrows
    @Test
    @Label("Fail to plant on a managed resource due to modifying existing shape tree location")
    void failToPlantDueToModifyingLocation() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

    }

    @Order(6)
    @SneakyThrows
    @Test
    @Label("Fail to plant malformed shape tree")
    void failToPlantDueToMalformedShapeTree() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

    }

    @Order(7)
    @SneakyThrows
    @Test
    @Label("Fail to plant missing shape tree")
    void failToPlantDueToMissingShapeTree() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

    }

    @Order(8)
    @SneakyThrows
    @Test
    @Label("Unplant managed resource")
    void unplantManagedResource() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

    }

    @Order(9)
    @SneakyThrows
    @Test
    @Label("Unplant managed resource with multiple planted shape trees")
    void unplantManagedResourceWithMultipleShapeTrees() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

    }

    @Order(10)
    @SneakyThrows
    @Test
    @Label("Unplant shape from managed resource")
    void unplantShapeFromManagedResource() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

    }

    @Order(11)
    @SneakyThrows
    @Test
    @Label("Fail to unplant unmanaged resource")
    void failToUnplantUnmanagedResource() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

    }

    @Order(12)
    @SneakyThrows
    @Test
    @Label("Fail to unplant non-root shape tree")
    void failToUnplantNonRootShapeTree() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

    }

}
