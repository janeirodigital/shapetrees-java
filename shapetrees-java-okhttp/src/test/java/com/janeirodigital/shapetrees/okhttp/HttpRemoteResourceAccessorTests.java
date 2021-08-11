package com.janeirodigital.shapetrees.okhttp;

import com.janeirodigital.shapetrees.okhttp.fixtures.DispatcherEntry;
import com.janeirodigital.shapetrees.okhttp.fixtures.RequestMatchingFixtureDispatcher;
import jdk.jfr.Label;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.util.List;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class HttpRemoteResourceAccessorTests extends BaseShapeTreeTest {

    public HttpRemoteResourceAccessorTests() {
        super();
    }

    private static RequestMatchingFixtureDispatcher dispatcher = null;

    @BeforeAll
    static void beforeAll() {
        dispatcher = new RequestMatchingFixtureDispatcher(List.of(
                new DispatcherEntry(List.of("resourceAccessor/rdf-resource"), "GET", "/static/resource/rdf-resource", null),
                new DispatcherEntry(List.of("errors/404"), "GET", "/static/resource/notpresent", null)
        ));
    }

    @Test
    @Label("Get an existing resource")
    void getExistingResource() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        // Get resource
    }

    @Test
    @Label("Get a non-existent resource")
    void getNonExistentResource() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
    }

    @Test
    @Label("Get existing RDF Resource")
    void getRdfResource() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
    }

    @Test
    @Label("Get existing Non-RDF Resource")
    void getNonRdfResource() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
    }

    @Test
    @Label("Get Metadata Resource")
    void getMetadataResource() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
    }

    @Test
    @Label("Get Managed Resource")
    void getManagedResource() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
    }

    @Test
    @Label("Get existing Container Resource")
    void getContainerResource() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
    }

    @Test
    @Label("Get Contained Resources")
    void getContainedResources() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
    }

    @Test
    @Label("Create resource")
    void createResource() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
    }

    @Test
    @Label("Update resource")
    void updateResource() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
    }


}
