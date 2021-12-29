package com.janeirodigital.shapetrees.okhttp;

import com.janeirodigital.shapetrees.core.ShapeTreeContext;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.tests.fixtures.DispatcherEntry;
import com.janeirodigital.shapetrees.tests.fixtures.RequestMatchingFixtureDispatcher;
import lombok.SneakyThrows;
import okhttp3.OkHttpClient;
import okhttp3.Response;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.jupiter.api.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static com.janeirodigital.shapetrees.core.enums.ContentType.OCTET_STREAM;
import static com.janeirodigital.shapetrees.core.enums.ContentType.TEXT_TURTLE;
import static com.janeirodigital.shapetrees.okhttp.OkHttpShapeTreeClient.post;
import static com.janeirodigital.shapetrees.tests.fixtures.MockWebServerHelper.toUrl;
import static org.junit.jupiter.api.Assertions.assertEquals;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class OkHttpShapeTreeClientTypeTests {

    private static RequestMatchingFixtureDispatcher dispatcher = null;
    private static MockWebServer server;
    private static ShapeTreeContext context;
    private static OkHttpClient okHttpClient;

    @BeforeAll
    static void beforeAll() throws ShapeTreeException {
        context = new ShapeTreeContext(null);
        okHttpClient = OkHttpValidatingClientFactory.get();
    }

    @BeforeEach
    void initializeDispatcher() {
        List dispatcherList = new ArrayList();

        dispatcherList.add(new DispatcherEntry(List.of("type/containers-container"), "GET", "/containers/", null));
        dispatcherList.add(new DispatcherEntry(List.of("type/containers-container-manager"), "GET", "/containers/.shapetree", null));
        dispatcherList.add(new DispatcherEntry(List.of("type/resources-container"), "GET", "/resources/", null));
        dispatcherList.add(new DispatcherEntry(List.of("type/resources-container-manager"), "GET", "/resources/.shapetree", null));
        dispatcherList.add(new DispatcherEntry(List.of("type/non-rdf-resources-container"), "GET", "/non-rdf-resources/", null));
        dispatcherList.add(new DispatcherEntry(List.of("type/non-rdf-resources-container-manager"), "GET", "/non-rdf-resources/.shapetree", null));
        dispatcherList.add(new DispatcherEntry(List.of("shapetrees/type-shapetree-ttl"), "GET", "/static/shapetrees/type/shapetree", null));

        server = new MockWebServer();
        dispatcher = new RequestMatchingFixtureDispatcher(dispatcherList);
        server.setDispatcher(dispatcher);
    }

    @Test
    @DisplayName("Create container when only containers are allowed")
    void createContainer() throws ShapeTreeException {

        // Add fixture to handle successful POST response
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("type/valid-container-create-response"), "POST", "/containers/valid-container/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/containers/valid-container/.shapetree", null));
        Response response;

        // Provide target shape tree
        response = post(okHttpClient, context, toUrl(server, "/containers/"), null, Arrays.asList(toUrl(server, "/static/shapetrees/type/shapetree#ContainerTree")), "valid-container", true, null, TEXT_TURTLE);
        assertEquals(201, response.code());

        // Do not provide target shape tree
        response = post(okHttpClient, context, toUrl(server, "/containers/"), null, null, "valid-container", true, null, TEXT_TURTLE);
        assertEquals(201, response.code());


    }

    @Test
    @DisplayName("Fail to create resource when only containers are allowed")
    void failToCreateNonContainer() throws ShapeTreeException {
        Response response;
        // Provide target shape tree for a resource when container shape tree is expected
        response = post(okHttpClient, context, toUrl(server, "/containers/"), null, Arrays.asList(toUrl(server, "/static/shapetrees/type/shapetree#ResourceTree")), "invalid-resource", false, null, TEXT_TURTLE);
        assertEquals(422, response.code());

        // Provide target shape tree for a container even though what's being sent is a resource
        response = post(okHttpClient, context, toUrl(server, "/containers/"), null, Arrays.asList(toUrl(server, "/static/shapetrees/type/shapetree#ContainerTree")), "invalid-resource", false, null, TEXT_TURTLE);
        assertEquals(422, response.code());

        // Don't provide a target shape tree at all
        response = post(okHttpClient, context, toUrl(server, "/containers/"), null, null, "invalid-resource", false, null, TEXT_TURTLE);
        assertEquals(422, response.code());
    }

    @Test
    @DisplayName("Create resource when only resources are allowed")
    void createResource() throws ShapeTreeException {
        // Add fixture to handle successful POST response
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("type/valid-resource-create-response"), "POST", "/resources/valid-resource", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/resources/valid-resource.shapetree", null));
        Response response;

        // Provide target shape tree
        response = post(okHttpClient, context, toUrl(server, "/resources/"), null, Arrays.asList(toUrl(server, "/static/shapetrees/type/shapetree#ResourceTree")), "valid-resource", false, null, TEXT_TURTLE);
        assertEquals(201, response.code());

        // Do not provide target shape tree
        response = post(okHttpClient, context, toUrl(server, "/resources/"), null, null, "valid-resource", false, null, TEXT_TURTLE);
        assertEquals(201, response.code());
    }

    @SneakyThrows
    @Test
    @DisplayName("Fail to create container when only resources are allowed")
    void failToCreateNonResource() {
        Response response;
        // Provide target shape tree for a container when resource shape tree is expected
        response = post(okHttpClient, context, toUrl(server, "/resources/"), null, Arrays.asList(toUrl(server, "/static/shapetrees/type/shapetree#ContainerTree")), "invalid-container", true, null, TEXT_TURTLE);
        assertEquals(422, response.code());

        // Provide target shape tree for a resource even though what's being sent is a container
        response = post(okHttpClient, context, toUrl(server, "/resources/"), null, Arrays.asList(toUrl(server, "/static/shapetrees/type/shapetree#ResourceTree")), "invalid-container", true, null, TEXT_TURTLE);
        assertEquals(422, response.code());

        // Don't provide a target shape tree at all
        response = post(okHttpClient, context, toUrl(server, "/resources/"), null, null, "invalid-container", true, null, TEXT_TURTLE);
        assertEquals(422, response.code());
    }

    @SneakyThrows
    @Test
    @DisplayName("Create non-rdf resource when only non-rdf resources are allowed")
    void createNonRDFResource() {
        // Add fixture to handle successful POST response
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("type/valid-non-rdf-resource-create-response"), "POST", "/non-rdf-resources/valid-non-rdf-resource", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/non-rdf-resources/valid-non-rdf-resource.shapetree", null)); // TODO: Test: should this fail? should it have already failed?
        Response response;

        response = post(okHttpClient, context, toUrl(server, "/non-rdf-resources/"), null, Arrays.asList(toUrl(server, "/static/shapetrees/type/shapetree#NonRDFResourceTree")), "valid-non-rdf-resource", false, null, OCTET_STREAM);
        assertEquals(201, response.code());

        response = post(okHttpClient, context, toUrl(server, "/non-rdf-resources/"), null, null, "valid-non-rdf-resource", false, null, OCTET_STREAM);
        assertEquals(201, response.code());
    }

    @SneakyThrows
    @Test
    @DisplayName("Fail to create resource when only non-rdf resources are allowed")
    void failToCreateNonRDFResource() {
        Response response;
        // Provide target shape tree for a resource when non-rdf-resource shape tree is expected
        response = post(okHttpClient, context, toUrl(server, "/non-rdf-resources/"), null, Arrays.asList(toUrl(server, "/static/shapetrees/type/shapetree#ResourceTree")), "invalid-non-rdf-resource", false, null, TEXT_TURTLE);
        assertEquals(422, response.code());

        // Provide target shape tree for a non-rdf-resource even though what's being sent is a resource
        response = post(okHttpClient, context, toUrl(server, "/non-rdf-resources/"), null, Arrays.asList(toUrl(server, "/static/shapetrees/type/shapetree#NonRDFResourceTree")), "invalid-non-rdf-resource", false, null, TEXT_TURTLE);
        assertEquals(422, response.code());

        // Don't provide a target shape tree at all
        response = post(okHttpClient, context, toUrl(server, "/non-rdf-resources/"), null, null, "invalid-non-rdf-resource", false, null, TEXT_TURTLE);
        assertEquals(422, response.code());
    }

}
