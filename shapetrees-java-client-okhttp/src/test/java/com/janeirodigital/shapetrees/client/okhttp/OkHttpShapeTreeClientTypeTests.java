package com.janeirodigital.shapetrees.client.okhttp;

import com.janeirodigital.shapetrees.core.validation.ShapeTreeContext;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.tests.fixtures.RequestMatchingFixtureDispatcher;
import okhttp3.OkHttpClient;
import okhttp3.Response;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.jupiter.api.*;

import java.util.Arrays;
import java.util.List;

import static com.janeirodigital.shapetrees.core.enums.ContentType.OCTET_STREAM;
import static com.janeirodigital.shapetrees.core.enums.ContentType.TEXT_TURTLE;
import static com.janeirodigital.shapetrees.client.okhttp.OkHttpShapeTreeClient.post;
import static com.janeirodigital.shapetrees.tests.fixtures.DispatcherHelper.mockOnGet;
import static com.janeirodigital.shapetrees.tests.fixtures.DispatcherHelper.mockOnPut;
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

        dispatcher = new RequestMatchingFixtureDispatcher();

        // Add managed container fixtures for container type tests
        mockOnGet(dispatcher, "/containers/","type/containers-container");
        mockOnGet(dispatcher, "/containers/.shapetree", "type/containers-container-manager");
        // Add managed container fixtures for resource type tests
        mockOnGet(dispatcher, "/resources/", "type/resources-container");
        mockOnGet(dispatcher, "/resources/.shapetree", "type/resources-container-manager");
        // Add managed container fixtures for non-rdf-resource type tests
        mockOnGet(dispatcher, "/non-rdf-resources/", "type/non-rdf-resources-container");
        mockOnGet(dispatcher, "/non-rdf-resources/.shapetree", "type/non-rdf-resources-container-manager");
        // Add fixture for shapetree resource providing type-specific shape trees for validation tests
        mockOnGet(dispatcher, "/static/shapetrees/type/shapetree", "shapetrees/type-shapetree-ttl");

        server = new MockWebServer();
        server.setDispatcher(dispatcher);
    }

    @Test
    @DisplayName("Create container with target shape tree when only containers are allowed")
    void createContainerWithTargetShapeTree() throws ShapeTreeException {

        mockOnPut(dispatcher, "/containers/valid-container/", "http/201");
        mockOnPut(dispatcher, "/containers/valid-container/.shapetree", "http/201");
        mockOnGet(dispatcher, "/containers/valid-container/", List.of("http/404", "type/valid-container"));
        
        Response response = post(okHttpClient, context, toUrl(server, "/containers/"), null, Arrays.asList(toUrl(server, "/static/shapetrees/type/shapetree#ContainerTree")), "valid-container", true, null, TEXT_TURTLE);
        assertEquals(201, response.code());

    }

    @Test
    @DisplayName("Create container without target shape tree when only containers are allowed")
    void createContainerWithoutTargetShapeTree() throws ShapeTreeException {

        mockOnPut(dispatcher, "/containers/valid-container/", "http/201");
        mockOnPut(dispatcher, "/containers/valid-container/.shapetree", "http/201");
        mockOnGet(dispatcher, "/containers/valid-container/", List.of("http/404", "type/valid-container"));
        
        Response response = post(okHttpClient, context, toUrl(server, "/containers/"), null, null, "valid-container", true, null, TEXT_TURTLE);
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
    @DisplayName("Create resource with target shape tree when only resources are allowed")
    void createResourceWithTargetShapeTree() throws ShapeTreeException {
        mockOnPut(dispatcher, "/resources/valid-resource", "http/201");
        mockOnPut(dispatcher, "/resources/valid-resource.shapetree", "http/201");
        mockOnGet(dispatcher, "/resources/valid-resource", List.of("http/404", "type/valid-resource"));

        Response response = post(okHttpClient, context, toUrl(server, "/resources/"), null, Arrays.asList(toUrl(server, "/static/shapetrees/type/shapetree#ResourceTree")), "valid-resource", false, null, TEXT_TURTLE);
        assertEquals(201, response.code());
    }

    @Test
    @DisplayName("Create resource without target shape tree when only resources are allowed")
    void createResourceWithoutTargetShapeTree() throws ShapeTreeException {
        mockOnPut(dispatcher, "/resources/valid-resource", "http/201");
        mockOnPut(dispatcher, "/resources/valid-resource.shapetree", "http/201");
        mockOnGet(dispatcher, "/resources/valid-resource", List.of("http/404", "type/valid-resource"));
        
        Response response = post(okHttpClient, context, toUrl(server, "/resources/"), null, null, "valid-resource", false, null, TEXT_TURTLE);
        assertEquals(201, response.code());
    }
    
    @Test
    @DisplayName("Fail to create container when only resources are allowed")
    void failToCreateNonResource() throws ShapeTreeException {
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

    
    @Test
    @DisplayName("Create non-rdf resource with target shape tree when only non-rdf resources are allowed")
    void createNonRDFResourceWithTargetShapeTree() throws ShapeTreeException {
        // Add fixture to handle successful POST response
        mockOnPut(dispatcher, "/non-rdf-resources/valid-non-rdf-resource", "http/201");
        mockOnPut(dispatcher, "/non-rdf-resources/valid-non-rdf-resource.shapetree", "http/201");
        mockOnGet(dispatcher, "/non-rdf-resources/valid-non-rdf-resource", List.of("http/404", "type/valid-non-rdf-resource"));


        Response response = post(okHttpClient, context, toUrl(server, "/non-rdf-resources/"), null, Arrays.asList(toUrl(server, "/static/shapetrees/type/shapetree#NonRDFResourceTree")), "valid-non-rdf-resource", false, null, OCTET_STREAM);
        assertEquals(201, response.code());
    }

    
    @Test
    @DisplayName("Create non-rdf resource without target shape tree when only non-rdf resources are allowed")
    void createNonRDFResourceWithoutTargetShapeTree() throws ShapeTreeException {
        // Add fixture to handle successful POST response
        mockOnPut(dispatcher, "/non-rdf-resources/valid-non-rdf-resource", "http/201");
        mockOnPut(dispatcher, "/non-rdf-resources/valid-non-rdf-resource.shapetree", "http/201");
        mockOnGet(dispatcher, "/non-rdf-resources/valid-non-rdf-resource", List.of("http/404", "type/valid-non-rdf-resource"));

        Response response = post(okHttpClient, context, toUrl(server, "/non-rdf-resources/"), null, null, "valid-non-rdf-resource", false, null, OCTET_STREAM);
        assertEquals(201, response.code());
    }

    
    @Test
    @DisplayName("Fail to create resource when only non-rdf resources are allowed")
    void failToCreateNonRDFResource() throws ShapeTreeException {
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
