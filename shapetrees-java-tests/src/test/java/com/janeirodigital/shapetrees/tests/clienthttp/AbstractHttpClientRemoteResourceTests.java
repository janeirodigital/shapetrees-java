package com.janeirodigital.shapetrees.tests.clienthttp;

import com.janeirodigital.shapetrees.client.http.HttpRemoteResourceAccessor;
import com.janeirodigital.shapetrees.core.ShapeTreeResource;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;
import com.janeirodigital.shapetrees.tests.fixtures.DispatcherEntry;
import com.janeirodigital.shapetrees.tests.fixtures.RequestMatchingFixtureDispatcher;
import okhttp3.mockwebserver.MockWebServer;
import org.apache.jena.graph.Graph;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Triple;
import org.junit.jupiter.api.*;

import java.net.URL;
import java.net.MalformedURLException;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class AbstractHttpClientRemoteResourceTests extends AbstractHttpClientTests {

    public AbstractHttpClientRemoteResourceTests() {
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
    void testRetrieveResourceNoLinkHeaders() throws MalformedURLException, ShapeTreeException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ShapeTreeResource.Fork resource = new HttpRemoteResourceAccessor().getResource(new ShapeTreeContext(null), getURL(server, "/static/resource/resource-no-link-headers"));
        assertTrue(resource.isExists());
        Assertions.assertTrue(((ShapeTreeResource.Primary) resource).getMetadataResourceUri().isEmpty());
    }

    @Test
    void testRetrieveResourceEmptyLinkHeader() throws MalformedURLException, ShapeTreeException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ShapeTreeResource.Fork resource = new HttpRemoteResourceAccessor().getResource(new ShapeTreeContext(null), getURL(server, "/static/resource/resource-empty-link-header"));;
        assertTrue(resource.isExists());
        Assertions.assertTrue(((ShapeTreeResource.Primary) resource).getMetadataResourceUri().isEmpty());
    }

    @Test
    void testRetrieveInvalidURIString() throws MalformedURLException, ShapeTreeException { // TODO: Test: may as well deleted as it's only testing URL.create()
        Assertions.assertThrows(java.lang.IllegalArgumentException.class, () -> new HttpRemoteResourceAccessor().getResource(new ShapeTreeContext(null), new URL(":invalid")));
    }

    @Test
    void testIsContainerNewResourceNoSlash() throws MalformedURLException, ShapeTreeException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ShapeTreeResource.Fork resource = new HttpRemoteResourceAccessor().getResource(new ShapeTreeContext(null), getURL(server, "/static/resource/not-existing-no-slash"));;
        assertFalse(resource.isExists());
        assertFalse(((ShapeTreeResource.Primary) resource).isContainer());
    }

    @Test
    void testIsContainerNewResourceSlash() throws MalformedURLException, ShapeTreeException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ShapeTreeResource.Fork resource = new HttpRemoteResourceAccessor().getResource(new ShapeTreeContext(null), getURL(server, "/static/resource/not-existing-slash/"));;
        assertFalse(resource.isExists());
        assertTrue(((ShapeTreeResource.Primary) resource).isContainer());
    }

    @Test
    void testIsContainerNewResourceSlashWithFragment() throws MalformedURLException, ShapeTreeException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ShapeTreeResource.Fork resource = new HttpRemoteResourceAccessor().getResource(new ShapeTreeContext(null), getURL(server, "/static/resource/not-existing-slash/#withfragment"));;
        assertFalse(resource.isExists());
        assertTrue(((ShapeTreeResource.Primary) resource).isContainer());
    }

    @Test
    void testIsContainerExistingContainerNoSlash() throws MalformedURLException, ShapeTreeException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ShapeTreeResource.Fork resource = new HttpRemoteResourceAccessor().getResource(new ShapeTreeContext(null), getURL(server, "/static/resource/resource-container-link-header"));;
        assertTrue(resource.isExists());
        assertTrue(((ShapeTreeResource.Primary) resource).isContainer());
    }

    @Test
    void testIsContainerExistingContainer() throws MalformedURLException, ShapeTreeException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ShapeTreeResource.Fork resource = new HttpRemoteResourceAccessor().getResource(new ShapeTreeContext(null), getURL(server, "/static/resource/resource-container-link-header/"));;
        assertTrue(resource.isExists());
        assertTrue(((ShapeTreeResource.Primary) resource).isContainer());
    }


    @Test
    void testNonExistingHeader() throws MalformedURLException, ShapeTreeException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ShapeTreeResource.Fork resource = new HttpRemoteResourceAccessor().getResource(new ShapeTreeContext(null), getURL(server, "/static/resource/resource-container-link-header"));;
        assertTrue(resource.isExists());
        Assertions.assertNull(resource.getAttributes().firstValue("invalid").orElse(null));
    }
/*
    @Test
    void updateGraphTestInvalidatedRefresh() throws MalformedURLException, ShapeTreeException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ShapeTreeResource.Fork resource = new HttpRemoteResourceAccessor().getResource(new ShapeTreeContext(null), getURL(server, "/static/resource/resource-container-link-header"));;
        Graph graph = resource.getGraph().get();
        graph.add(new Triple(NodeFactory.createURI("<#b>"), NodeFactory.createURI("<#c>"), NodeFactory.createURI("<#d>")));
        resource.updateGraph(graph, true, null);
        assertTrue(resource.isExists());
    }

    @Test
    void doubleUpdateGraphWithoutRefresh() throws MalformedURLException, ShapeTreeException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ShapeTreeResource.Fork resource = new HttpRemoteResourceAccessor().getResource(new ShapeTreeContext(null), getURL(server, "/static/resource/resource-container-link-header"));;
        Graph graph = resource.getGraph().get();
        graph.add(new Triple(NodeFactory.createURI("<#b>"), NodeFactory.createURI("<#c>"), NodeFactory.createURI("<#d>")));
        resource.updateGraph(graph, false, null);
        assertTrue(resource.isExists());
//        Assertions.assertThrows(ShapeTreeException.class, () -> resource.updateGraph(graph, false, null)); HttpRemoteResource999.invalidated has been removed
    }

    @Test
    void updateGraphWithoutRefreshGetBody() throws MalformedURLException, ShapeTreeException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ShapeTreeResource.Fork resource = new HttpRemoteResourceAccessor().getResource(new ShapeTreeContext(null), getURL(server, "/static/resource/resource-container-link-header"));;
        Graph graph = resource.getGraph().get();
        graph.add(new Triple(NodeFactory.createURI("<#b>"), NodeFactory.createURI("<#c>"), NodeFactory.createURI("<#d>")));
        resource.updateGraph(graph, false, null);
        assertTrue(resource.isExists());
        Assertions.assertNotNull(resource.getBody());
    }

    @Test
    void updateGraphWithoutRefreshGetGraph() throws MalformedURLException, ShapeTreeException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ShapeTreeResource.Fork resource = new HttpRemoteResourceAccessor().getResource(new ShapeTreeContext(null), getURL(server, "/static/resource/resource-container-link-header"));;
        Graph graph = resource.getGraph().get();
        graph.add(new Triple(NodeFactory.createURI("<#b>"), NodeFactory.createURI("<#c>"), NodeFactory.createURI("<#d>")));
        resource.updateGraph(graph, false, null);
        assertTrue(resource.isExists());
        Assertions.assertNotNull(resource.getGraph());
    }

    @Test
    void updateGetHeaderForCoverage() throws MalformedURLException, ShapeTreeException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ShapeTreeResource.Fork resource = new HttpRemoteResourceAccessor().getResource(new ShapeTreeContext(null), getURL(server, "/static/resource/resource-container-link-header"));;
        Graph graph = resource.getGraph().get();
        graph.add(new Triple(NodeFactory.createURI("<#b>"), NodeFactory.createURI("<#c>"), NodeFactory.createURI("<#d>")));
        resource.updateGraph(graph, false, null);
        assertTrue(resource.isExists());
        Assertions.assertNotNull(resource.getAttributes().firstValue("Link").orElse(null));
    }
*/
    /*
    @Test
    void getLinkHeaders() throws MalformedURLException, ShapeTreeException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        HttpRemoteResource999 resource = new ShapeTreeResource(getURL(server, "/static/resource/resource-container-link-header"), null);
        assertTrue(resource.isExists());
        Assertions.assertNotNull(resource.getLinkHeaders());
    }
*/
    @Test
    void test404Target() throws MalformedURLException, ShapeTreeException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ShapeTreeResource.Fork resource = new HttpRemoteResourceAccessor().getResource(new ShapeTreeContext(null), getURL(server, "/static/resource/notpresent"));;
        Assertions.assertEquals(resource.getBody(), "");
//        assertTrue(resource.getGraph().isEmpty());
    }
}
