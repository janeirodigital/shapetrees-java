package com.janeirodigital.shapetrees.client.okhttp;

// TODO: Populate tests from primer examples

import com.janeirodigital.shapetrees.client.okhttp.fixtures.DispatcherEntry;
import com.janeirodigital.shapetrees.client.okhttp.fixtures.RequestMatchingFixtureDispatcher;
import jdk.jfr.Label;
import lombok.SneakyThrows;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.jupiter.api.*;

import java.util.List;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class TypeTests extends BaseShapeTreeTest {

    private static RequestMatchingFixtureDispatcher dispatcher = null;

    public TypeTests() {
        // Call BaseShapeTreeTest constructor
        super();
    }

    @BeforeAll
    static void beforeAll() {
        dispatcher = new RequestMatchingFixtureDispatcher(List.of(
                new DispatcherEntry(List.of("project/root-container"), "GET", "/", null),
                new DispatcherEntry(List.of("project/root-container-locator"), "GET", "/.shapetree", null)
        ));
    }

    @Order(1)
    @SneakyThrows
    @Test
    @Label("Create container when only containers are allowed")
    void createContainer() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Create a container (success)

    }

    @Order(2)
    @SneakyThrows
    @Test
    @Label("Fail to create resource when only containers are allowed")
    void failToCreateNonContainer() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Try to create a resource (fail)

    }

    @Order(3)
    @SneakyThrows
    @Test
    @Label("Create resource when only resources are allowed")
    void createResource() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Create a resource (success)

    }

    @Order(4)
    @SneakyThrows
    @Test
    @Label("Fail to create container when only resources are allowed")
    void failToCreateNonResource() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Try to create container (fail)

    }

    @Order(5)
    @SneakyThrows
    @Test
    @Label("Create non-rdf resource when only non-rdf resources are allowed")
    void createNonRDFResource() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Create a non-rdf resource (success)

    }

    @Order(6)
    @SneakyThrows
    @Test
    @Label("Fail to create resource when only non-rdf resources are allowed")
    void failToCreateNonRDFResource() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // create a resource (fail)

    }

    @Order(7)
    @SneakyThrows
    @Test
    @Label("Create resources when multiple types are allowed")
    void createAllowedResourceTypes() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // container with a shape tree specified for containment, plus container,
        // resource, and non-rdf allowed

    }

}
