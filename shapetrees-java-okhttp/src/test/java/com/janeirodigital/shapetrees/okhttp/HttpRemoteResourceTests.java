package com.janeirodigital.shapetrees.okhttp;

import com.janeirodigital.shapetrees.client.http.HttpRemoteResource;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.okhttp.fixtures.DispatcherEntry;
import com.janeirodigital.shapetrees.okhttp.fixtures.RequestMatchingFixtureDispatcher;
import okhttp3.mockwebserver.MockWebServer;
import org.apache.jena.graph.Graph;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Triple;
import org.junit.jupiter.api.*;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class HttpRemoteResourceTests extends BaseShapeTreeTest {

    public HttpRemoteResourceTests() {
        super();
    }

    private static RequestMatchingFixtureDispatcher dispatcher = null;

    @BeforeAll
    static void beforeAll() {
        dispatcher = new RequestMatchingFixtureDispatcher(List.of(
                new DispatcherEntry(List.of("remoteResource/resource-no-link-headers"), "GET", "/static/resource/resource-no-link-headers", null),
                new DispatcherEntry(List.of("remoteResource/resource-empty-link-header"), "GET", "/static/resource/resource-empty-link-header", null),
                new DispatcherEntry(List.of("remoteResource/resource-container-link-header"), "GET", "/static/resource/resource-container-link-header", null),
                new DispatcherEntry(List.of("remoteResource/resource-container-link-header"), "GET", "/static/resource/resource-container-link-header/", null),
                new DispatcherEntry(List.of("errors/404"), "GET", "/static/resource/notpresent", null)
        ));
    }

    @Test
    void testRetrieveResourceNoLinkHeaders() throws URISyntaxException, IOException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        HttpRemoteResource resource = new HttpRemoteResource(getURI(server, "/static/resource/resource-no-link-headers"), null);
        assertTrue(resource.exists());
        Assertions.assertThrows(ShapeTreeException.class, resource::getMetadataURI);
    }

    @Test
    void testRetrieveResourceEmptyLinkHeader() throws URISyntaxException, IOException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        HttpRemoteResource resource = new HttpRemoteResource(getURI(server, "/static/resource/resource-empty-link-header"), null);
        assertTrue(resource.exists());
        Assertions.assertThrows(ShapeTreeException.class, resource::getMetadataURI);
    }

    @Test
    void testRetrieveInvalidURIString() throws URISyntaxException, IOException { // TODO: may as well deleted as it's only testing URI.create()
        Assertions.assertThrows(java.lang.IllegalArgumentException.class, () -> new HttpRemoteResource(URI.create(":invalid"), null));
    }

    @Test
    void testIsContainerNewResourceNoSlash() throws URISyntaxException, IOException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        HttpRemoteResource resource = new HttpRemoteResource(getURI(server, "/static/resource/not-existing-no-slash"), null);
        assertFalse(resource.exists());
        assertFalse(resource.isContainer());
    }

    @Test
    void testIsContainerNewResourceSlash() throws URISyntaxException, IOException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        HttpRemoteResource resource = new HttpRemoteResource(getURI(server, "/static/resource/not-existing-slash/"), null);
        assertFalse(resource.exists());
        assertTrue(resource.isContainer());
    }

    @Test
    void testIsContainerNewResourceSlashWithFragment() throws URISyntaxException, IOException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        HttpRemoteResource resource = new HttpRemoteResource(getURI(server, "/static/resource/not-existing-slash/#withfragment"), null);
        assertFalse(resource.exists());
        assertTrue(resource.isContainer());
    }

    @Test
    void testIsContainerExistingContainerNoSlash() throws URISyntaxException, IOException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        HttpRemoteResource resource = new HttpRemoteResource(getURI(server, "/static/resource/resource-container-link-header"), null);
        assertTrue(resource.exists());
        assertFalse(resource.isContainer());
    }

    @Test
    void testIsContainerExistingContainer() throws URISyntaxException, IOException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        HttpRemoteResource resource = new HttpRemoteResource(getURI(server, "/static/resource/resource-container-link-header/"), null);
        assertTrue(resource.exists());
        assertTrue(resource.isContainer());
    }


    @Test
    void testNonExistingHeader() throws URISyntaxException, IOException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        HttpRemoteResource resource = new HttpRemoteResource(getURI(server, "/static/resource/resource-container-link-header"), null);
        assertTrue(resource.exists());
        Assertions.assertNull(resource.getFirstHeaderByName("invalid"));
    }

    @Test
    void updateGraphTestInvalidatedRefresh() throws URISyntaxException, IOException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        HttpRemoteResource resource = new HttpRemoteResource(getURI(server, "/static/resource/resource-container-link-header"), null);
        Graph graph = resource.getGraph(getURI(server, "/static/resource/resource-container-link-header"));
        graph.add(new Triple(NodeFactory.createURI("<#b>"), NodeFactory.createURI("<#c>"), NodeFactory.createURI("<#d>")));
        resource.updateGraph(graph, true, null);
        assertTrue(resource.exists());
    }

    @Test
    void doubleUpdateGraphWithoutRefresh() throws URISyntaxException, IOException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        HttpRemoteResource resource = new HttpRemoteResource(getURI(server, "/static/resource/resource-container-link-header"), null);
        Graph graph = resource.getGraph(getURI(server, "/static/resource/resource-container-link-header"));
        graph.add(new Triple(NodeFactory.createURI("<#b>"), NodeFactory.createURI("<#c>"), NodeFactory.createURI("<#d>")));
        resource.updateGraph(graph, false, null);
        assertTrue(resource.exists());
        Assertions.assertThrows(ShapeTreeException.class, () -> resource.updateGraph(graph, false, null));
    }

    @Test
    void updateGraphWithoutRefreshGetBody() throws URISyntaxException, IOException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        HttpRemoteResource resource = new HttpRemoteResource(getURI(server, "/static/resource/resource-container-link-header"), null);
        Graph graph = resource.getGraph(getURI(server, "/static/resource/resource-container-link-header"));
        graph.add(new Triple(NodeFactory.createURI("<#b>"), NodeFactory.createURI("<#c>"), NodeFactory.createURI("<#d>")));
        resource.updateGraph(graph, false, null);
        assertTrue(resource.exists());
        Assertions.assertNotNull(resource.getBody());
    }

    @Test
    void updateGraphWithoutRefreshGetGraph() throws URISyntaxException, IOException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        HttpRemoteResource resource = new HttpRemoteResource(getURI(server, "/static/resource/resource-container-link-header"), null);
        Graph graph = resource.getGraph(getURI(server, "/static/resource/resource-container-link-header"));
        graph.add(new Triple(NodeFactory.createURI("<#b>"), NodeFactory.createURI("<#c>"), NodeFactory.createURI("<#d>")));
        resource.updateGraph(graph, false, null);
        assertTrue(resource.exists());
        Assertions.assertNotNull(resource.getGraph(getURI(server, "/static/resource/resource-container-link-header")));
    }

    @Test
    void updateGetHeaderForCoverage() throws URISyntaxException, IOException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        HttpRemoteResource resource = new HttpRemoteResource(getURI(server, "/static/resource/resource-container-link-header"), null);
        Graph graph = resource.getGraph(getURI(server, "/static/resource/resource-container-link-header"));
        graph.add(new Triple(NodeFactory.createURI("<#b>"), NodeFactory.createURI("<#c>"), NodeFactory.createURI("<#d>")));
        resource.updateGraph(graph, false, null);
        assertTrue(resource.exists());
        Assertions.assertNotNull(resource.getFirstHeaderByName("Link"));
    }

    @Test
    void getLinkHeaders() throws URISyntaxException, IOException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        HttpRemoteResource resource = new HttpRemoteResource(getURI(server, "/static/resource/resource-container-link-header"), null);
        assertTrue(resource.exists());
        Assertions.assertNotNull(resource.getLinkHeaders());
    }

    @Test
    void test404Target() throws URISyntaxException, IOException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        HttpRemoteResource resource = new HttpRemoteResource(getURI(server, "/static/resource/notpresent"), null);
        Assertions.assertNull(resource.getGraph(getURI(server, "/static/resource/notpresent")));
        Assertions.assertNull(resource.getBody());
    }


}
