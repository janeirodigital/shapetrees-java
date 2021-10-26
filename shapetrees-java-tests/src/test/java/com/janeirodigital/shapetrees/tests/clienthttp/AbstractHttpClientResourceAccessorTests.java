package com.janeirodigital.shapetrees.tests.clienthttp;

import com.janeirodigital.shapetrees.client.http.HttpRemoteResourceAccessor;
import com.janeirodigital.shapetrees.core.DocumentResponse;
import com.janeirodigital.shapetrees.core.ShapeTreeResource;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;
import com.janeirodigital.shapetrees.tests.fixtures.DispatcherEntry;
import com.janeirodigital.shapetrees.tests.fixtures.RequestMatchingFixtureDispatcher;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.jupiter.api.*;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class AbstractHttpClientResourceAccessorTests extends AbstractHttpClientTests {

    public AbstractHttpClientResourceAccessorTests() {
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
                new DispatcherEntry(List.of("errors/500"), "PUT", "/static/resource/resource-container-link-header/", null),
                new DispatcherEntry(List.of("remoteResource/resource-container-invalid-link-header"), "GET", "/static/resource/resource-container-invalid-link-header/", null),
                new DispatcherEntry(List.of("errors/404"), "GET", "/static/resource/notpresent", null)
        ));
    }

    @Test
    void testRetrieveResourceNoLinkHeaders() throws MalformedURLException, ShapeTreeException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ShapeTreeResource.Fork resource = new HttpRemoteResourceAccessor().getResource(new ShapeTreeContext(null), getURL(server, "/static/resource/resource-no-link-headers"));
        assertTrue(resource.isExists());
        Assertions.assertTrue(((ShapeTreeResource.Primary) resource).getMetadataResourceUrl().isEmpty());
    }

    @Test
    void testRetrieveResourceEmptyLinkHeader() throws MalformedURLException, ShapeTreeException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ShapeTreeResource.Fork resource = new HttpRemoteResourceAccessor().getResource(new ShapeTreeContext(null), getURL(server, "/static/resource/resource-empty-link-header"));;
        assertTrue(resource.isExists());
        Assertions.assertTrue(((ShapeTreeResource.Primary) resource).getMetadataResourceUrl().isEmpty());
    }

    @Test
    void testRetrieveInvalidURLString() throws MalformedURLException, ShapeTreeException { // TODO: Test: may as well deleted as it's only testing URL.create()
        Assertions.assertThrows(MalformedURLException.class, () -> new HttpRemoteResourceAccessor().getResource(new ShapeTreeContext(null), new URL(":invalid")));
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

    @Test
    void test404Target() throws MalformedURLException, ShapeTreeException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ShapeTreeResource.Fork resource = new HttpRemoteResourceAccessor().getResource(new ShapeTreeContext(null), getURL(server, "/static/resource/notpresent"));;
        Assertions.assertEquals(resource.getBody(), "");
    }

    @Test
    void failToParseInvalidLinkHeader() throws MalformedURLException, ShapeTreeException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ShapeTreeResource.Fork resource = new HttpRemoteResourceAccessor().getResource(new ShapeTreeContext(null), getURL(server, "/static/resource/resource-container-invalid-link-header/"));;
        assertTrue(resource.isExists());
        assertFalse(((ShapeTreeResource.Primary) resource).isContainer());
    }

    @Test
    void testServerFailureBasedOnResponseCode() throws MalformedURLException, ShapeTreeException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        // Succeed in getting a resource
        ShapeTreeResource.Fork resource =  new HttpRemoteResourceAccessor().getResource(new ShapeTreeContext(null), getURL(server, "/static/resource/resource-container-link-header/"));;
        // Fail to update it
        DocumentResponse response = new HttpRemoteResourceAccessor().updateResource(new ShapeTreeContext(null), "PUT", resource, "BODY");
        assertFalse(response.isExists());
    }

}
