package com.janeirodigital.shapetrees.okhttp;

import com.janeirodigital.shapetrees.client.http.HttpRemoteResource999;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.okhttp.fixtures.DispatcherEntry;
import com.janeirodigital.shapetrees.okhttp.fixtures.RequestMatchingFixtureDispatcher;
import okhttp3.mockwebserver.MockWebServer;
import org.apache.jena.graph.Graph;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Triple;
import org.junit.jupiter.api.*;

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
    void testRetrieveResourceNoLinkHeaders() throws URISyntaxException, ShapeTreeException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        HttpRemoteResource999 resource = new HttpRemoteResource999(getURI(server, "/static/resource/resource-no-link-headers"), null);
        assertTrue(resource.isExists());
        Assertions.assertTrue(resource.getAssociatedUri().isEmpty());
    }

    @Test
    void testRetrieveResourceEmptyLinkHeader() throws URISyntaxException, ShapeTreeException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        HttpRemoteResource999 resource = new HttpRemoteResource999(getURI(server, "/static/resource/resource-empty-link-header"), null);
        assertTrue(resource.isExists());
        Assertions.assertTrue(resource.getAssociatedUri().isEmpty());
    }

    @Test
    void testRetrieveInvalidURIString() throws URISyntaxException, ShapeTreeException { // TODO: Test: may as well deleted as it's only testing URI.create()
        Assertions.assertThrows(java.lang.IllegalArgumentException.class, () -> new HttpRemoteResource999(URI.create(":invalid"), null));
    }

    @Test
    void testIsContainerNewResourceNoSlash() throws URISyntaxException, ShapeTreeException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        HttpRemoteResource999 resource = new HttpRemoteResource999(getURI(server, "/static/resource/not-existing-no-slash"), null);
        assertFalse(resource.isExists());
        assertFalse(resource.isContainer());
    }

    @Test
    void testIsContainerNewResourceSlash() throws URISyntaxException, ShapeTreeException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        HttpRemoteResource999 resource = new HttpRemoteResource999(getURI(server, "/static/resource/not-existing-slash/"), null);
        assertFalse(resource.isExists());
        assertTrue(resource.isContainer());
    }

    @Test
    void testIsContainerNewResourceSlashWithFragment() throws URISyntaxException, ShapeTreeException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        HttpRemoteResource999 resource = new HttpRemoteResource999(getURI(server, "/static/resource/not-existing-slash/#withfragment"), null);
        assertFalse(resource.isExists());
        assertTrue(resource.isContainer());
    }

    @Test
    void testIsContainerExistingContainerNoSlash() throws URISyntaxException, ShapeTreeException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        HttpRemoteResource999 resource = new HttpRemoteResource999(getURI(server, "/static/resource/resource-container-link-header"), null);
        assertTrue(resource.isExists());
        assertTrue(resource.isContainer());
    }

    @Test
    void testIsContainerExistingContainer() throws URISyntaxException, ShapeTreeException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        HttpRemoteResource999 resource = new HttpRemoteResource999(getURI(server, "/static/resource/resource-container-link-header/"), null);
        assertTrue(resource.isExists());
        assertTrue(resource.isContainer());
    }


    @Test
    void testNonExistingHeader() throws URISyntaxException, ShapeTreeException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        HttpRemoteResource999 resource = new HttpRemoteResource999(getURI(server, "/static/resource/resource-container-link-header"), null);
        assertTrue(resource.isExists());
        Assertions.assertNull(resource.getAttributes().firstValue("invalid").orElse(null));
    }

    @Test
    void updateGraphTestInvalidatedRefresh() throws URISyntaxException, ShapeTreeException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        HttpRemoteResource999 resource = new HttpRemoteResource999(getURI(server, "/static/resource/resource-container-link-header"), null);
        Graph graph = resource.getGraph().get();
        graph.add(new Triple(NodeFactory.createURI("<#b>"), NodeFactory.createURI("<#c>"), NodeFactory.createURI("<#d>")));
        resource.updateGraph(graph, true, null);
        assertTrue(resource.isExists());
    }

    @Test
    void doubleUpdateGraphWithoutRefresh() throws URISyntaxException, ShapeTreeException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        HttpRemoteResource999 resource = new HttpRemoteResource999(getURI(server, "/static/resource/resource-container-link-header"), null);
        Graph graph = resource.getGraph().get();
        graph.add(new Triple(NodeFactory.createURI("<#b>"), NodeFactory.createURI("<#c>"), NodeFactory.createURI("<#d>")));
        resource.updateGraph(graph, false, null);
        assertTrue(resource.isExists());
//        Assertions.assertThrows(ShapeTreeException.class, () -> resource.updateGraph(graph, false, null)); HttpRemoteResource999.invalidated has been removed
    }

    @Test
    void updateGraphWithoutRefreshGetBody() throws URISyntaxException, ShapeTreeException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        HttpRemoteResource999 resource = new HttpRemoteResource999(getURI(server, "/static/resource/resource-container-link-header"), null);
        Graph graph = resource.getGraph().get();
        graph.add(new Triple(NodeFactory.createURI("<#b>"), NodeFactory.createURI("<#c>"), NodeFactory.createURI("<#d>")));
        resource.updateGraph(graph, false, null);
        assertTrue(resource.isExists());
        Assertions.assertNotNull(resource.getBody());
    }

    @Test
    void updateGraphWithoutRefreshGetGraph() throws URISyntaxException, ShapeTreeException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        HttpRemoteResource999 resource = new HttpRemoteResource999(getURI(server, "/static/resource/resource-container-link-header"), null);
        Graph graph = resource.getGraph().get();
        graph.add(new Triple(NodeFactory.createURI("<#b>"), NodeFactory.createURI("<#c>"), NodeFactory.createURI("<#d>")));
        resource.updateGraph(graph, false, null);
        assertTrue(resource.isExists());
        Assertions.assertNotNull(resource.getGraph());
    }

    @Test
    void updateGetHeaderForCoverage() throws URISyntaxException, ShapeTreeException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        HttpRemoteResource999 resource = new HttpRemoteResource999(getURI(server, "/static/resource/resource-container-link-header"), null);
        Graph graph = resource.getGraph().get();
        graph.add(new Triple(NodeFactory.createURI("<#b>"), NodeFactory.createURI("<#c>"), NodeFactory.createURI("<#d>")));
        resource.updateGraph(graph, false, null);
        assertTrue(resource.isExists());
        Assertions.assertNotNull(resource.getAttributes().firstValue("Link").orElse(null));
    }
/*
    @Test
    void getLinkHeaders() throws URISyntaxException, ShapeTreeException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        HttpRemoteResource999 resource = new HttpRemoteResource999(getURI(server, "/static/resource/resource-container-link-header"), null);
        assertTrue(resource.isExists());
        Assertions.assertNotNull(resource.getLinkHeaders());
    }
*/
    @Test
    void test404Target() throws URISyntaxException, ShapeTreeException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        HttpRemoteResource999 resource = new HttpRemoteResource999(getURI(server, "/static/resource/notpresent"), null);
        Assertions.assertEquals(resource.getBody(), "");
        assertTrue(resource.getGraph().isEmpty());
    }
}
