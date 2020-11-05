package com.janeirodigital.shapetrees.test;

import com.janeirodigital.shapetrees.RemoteResource;
import com.janeirodigital.shapetrees.ShapeTreeException;
import com.janeirodigital.shapetrees.test.fixtures.DispatcherEntry;
import com.janeirodigital.shapetrees.test.fixtures.RequestMatchingFixtureDispatcher;
import lombok.SneakyThrows;
import okhttp3.mockwebserver.MockWebServer;
import org.apache.jena.graph.Graph;
import org.apache.jena.graph.GraphUtil;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Triple;
import org.junit.jupiter.api.*;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class RemoteResourceTests extends BaseShapeTreeTest {

    public RemoteResourceTests() {
        super(new MockEcosystem());
    }

    private static RequestMatchingFixtureDispatcher dispatcher = null;

    @BeforeAll
    static void beforeAll() {
        dispatcher = new RequestMatchingFixtureDispatcher(List.of(
                new DispatcherEntry(List.of("remoteResource/resource-no-link-headers"), "GET", "/static/resource/resource-no-link-headers", null),
                new DispatcherEntry(List.of("remoteResource/resource-empty-link-header"), "GET", "/static/resource/resource-empty-link-header", null),
                new DispatcherEntry(List.of("remoteResource/resource-container-link-header"), "GET", "/static/resource/resource-container-link-header", null)
        ));
    }

    @Test
    void testRetrieveResourceNoLinkHeaders() throws URISyntaxException, IOException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        RemoteResource resource = new RemoteResource(getURI(server, "/static/resource/resource-no-link-headers"), null);
        assertTrue(resource.exists());
        assertThrows(ShapeTreeException.class, resource::getMetadataURI);
    }

    @Test
    void testRetrieveResourceEmptyLinkHeader() throws URISyntaxException, IOException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        RemoteResource resource = new RemoteResource(getURI(server, "/static/resource/resource-empty-link-header"), null);
        assertTrue(resource.exists());
        assertThrows(ShapeTreeException.class, resource::getMetadataURI);
    }

    @Test
    void testRetrieveInvalidURIString() throws URISyntaxException, IOException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        assertThrows(IOException.class, () -> new RemoteResource(":invalid", null));
    }

    @Test
    void testIsContainerNewResourceNoSlash() throws URISyntaxException, IOException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        RemoteResource resource = new RemoteResource(getURI(server, "/static/resource/not-existing-no-slash"), null);
        assertFalse(resource.exists());
        assertFalse(resource.isContainer());
    }

    @Test
    void testIsContainerNewResourceSlash() throws URISyntaxException, IOException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        RemoteResource resource = new RemoteResource(getURI(server, "/static/resource/not-existing-slash/"), null);
        assertFalse(resource.exists());
        assertTrue(resource.isContainer());
    }

    @Test
    void testIsContainerNewResourceSlashWithFragment() throws URISyntaxException, IOException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        RemoteResource resource = new RemoteResource(getURI(server, "/static/resource/not-existing-slash/#withfragment"), null);
        assertFalse(resource.exists());
        assertTrue(resource.isContainer());
    }

    @Test
    void testIsContainerExistingContainer() throws URISyntaxException, IOException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        RemoteResource resource = new RemoteResource(getURI(server, "/static/resource/resource-container-link-header"), null);
        assertTrue(resource.exists());
        assertTrue(resource.isContainer());
    }

    @Test
    void testNonExistingHeader() throws URISyntaxException, IOException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        RemoteResource resource = new RemoteResource(getURI(server, "/static/resource/resource-container-link-header"), null);
        assertTrue(resource.exists());
        assertNull(resource.getFirstHeaderByName("invalid"));
    }

    @Test
    void updateGraphTestInvalidatedRefresh() throws URISyntaxException, IOException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        RemoteResource resource = new RemoteResource(getURI(server, "/static/resource/resource-container-link-header"), null);
        Graph graph = resource.getGraph(getURI(server, "/static/resource/resource-container-link-header"));
        graph.add(new Triple(NodeFactory.createURI("<#b>"), NodeFactory.createURI("<#c>"), NodeFactory.createURI("<#d>")));
        resource.updateGraph(graph, true, null);
        assertTrue(resource.exists());
    }

    @Test
    void doubleUpdateGraphWithoutRefresh() throws URISyntaxException, IOException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        RemoteResource resource = new RemoteResource(getURI(server, "/static/resource/resource-container-link-header"), null);
        Graph graph = resource.getGraph(getURI(server, "/static/resource/resource-container-link-header"));
        graph.add(new Triple(NodeFactory.createURI("<#b>"), NodeFactory.createURI("<#c>"), NodeFactory.createURI("<#d>")));
        resource.updateGraph(graph, false, null);
        assertTrue(resource.exists());
        assertThrows(ShapeTreeException.class, () -> resource.updateGraph(graph, false, null));
    }

    @Test
    void updateGraphWithoutRefreshGetBody() throws URISyntaxException, IOException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        RemoteResource resource = new RemoteResource(getURI(server, "/static/resource/resource-container-link-header"), null);
        Graph graph = resource.getGraph(getURI(server, "/static/resource/resource-container-link-header"));
        graph.add(new Triple(NodeFactory.createURI("<#b>"), NodeFactory.createURI("<#c>"), NodeFactory.createURI("<#d>")));
        resource.updateGraph(graph, false, null);
        assertTrue(resource.exists());
        assertNotNull(resource.getBody());
    }

    @Test
    void updateGraphWithoutRefreshGetGraph() throws URISyntaxException, IOException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        RemoteResource resource = new RemoteResource(getURI(server, "/static/resource/resource-container-link-header"), null);
        Graph graph = resource.getGraph(getURI(server, "/static/resource/resource-container-link-header"));
        graph.add(new Triple(NodeFactory.createURI("<#b>"), NodeFactory.createURI("<#c>"), NodeFactory.createURI("<#d>")));
        resource.updateGraph(graph, false, null);
        assertTrue(resource.exists());
        assertNotNull(resource.getGraph(getURI(server, "/static/resource/resource-container-link-header")));
    }

    @Test
    void updateGetHeaderForCoverage() throws URISyntaxException, IOException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        RemoteResource resource = new RemoteResource(getURI(server, "/static/resource/resource-container-link-header"), null);
        Graph graph = resource.getGraph(getURI(server, "/static/resource/resource-container-link-header"));
        graph.add(new Triple(NodeFactory.createURI("<#b>"), NodeFactory.createURI("<#c>"), NodeFactory.createURI("<#d>")));
        resource.updateGraph(graph, false, null);
        assertTrue(resource.exists());
        assertNotNull(resource.getFirstHeaderByName("Link"));
    }

    @Test
    void getLinkHeaders() throws URISyntaxException, IOException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        RemoteResource resource = new RemoteResource(getURI(server, "/static/resource/resource-container-link-header"), null);
        assertTrue(resource.exists());
        assertNotNull(resource.getLinkHeaders());
    }


    /*
    @Test
    @Order(1)
    @SneakyThrows
    @DisplayName("RemoteResource - Create Resource")
    void createResource() {
        putContent(new URI(ROOT_PATH+"testing.ttl"), false, "target/test-classes/test-data/apps/gh-deep/ericprud-user.ttl", null);
        RemoteResource resource = new RemoteResource(new URI(ROOT_PATH+"testing.ttl"), AUTH_HEADER_VALUE);
        assertEquals(resource.exists(), true);
    }

    @Test
    @Order(2)
    @SneakyThrows
    @DisplayName("RemoteResource - UpdateGraph with refresh")
    void updateResourceWithRefresh() {
        RemoteResource resource = new RemoteResource(new URI(ROOT_PATH+"testing.ttl"), AUTH_HEADER_VALUE);
        Graph existingGraph = resource.getGraph(new URI(ROOT_PATH+"testing.ttl"));
        List<Triple> triplesToAdd = new ArrayList<>();
        triplesToAdd.add(new Triple(NodeFactory.createURI(resource.getURI().toString()), NodeFactory.createURI("http://example.com/predicate"), NodeFactory.createLiteral("testing1")));
        GraphUtil.add(existingGraph, triplesToAdd);

        resource.updateGraph(existingGraph, true, AUTH_HEADER_VALUE);
        assertEquals(true, resource.getBody().contains("testing1"));
    }

    @Test
    @Order(3)
    @SneakyThrows
    @DisplayName("RemoteResource - UpdateGraph without refresh")
    void updateResourceWithoutRefresh() {
        RemoteResource resource = new RemoteResource(new URI(ROOT_PATH+"testing.ttl"), AUTH_HEADER_VALUE);
        Graph existingGraph = resource.getGraph(new URI(ROOT_PATH+"testing.ttl"));
        List<Triple> triplesToAdd = new ArrayList<>();
        triplesToAdd.add(new Triple(NodeFactory.createURI(resource.getURI().toString()), NodeFactory.createURI("http://example.com/predicate"), NodeFactory.createLiteral("testing2")));
        GraphUtil.add(existingGraph, triplesToAdd);

        resource.updateGraph(existingGraph, false, AUTH_HEADER_VALUE);
        assertEquals(true, resource.getBody().contains("testing2"));
    }
*/

}
