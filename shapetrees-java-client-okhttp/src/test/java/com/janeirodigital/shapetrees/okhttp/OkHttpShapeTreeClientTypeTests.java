package com.janeirodigital.shapetrees.okhttp;

import com.janeirodigital.shapetrees.client.http.HttpClient;
import com.janeirodigital.shapetrees.client.http.HttpClientFactory;
import com.janeirodigital.shapetrees.client.http.HttpClientFactoryManager;
import com.janeirodigital.shapetrees.client.http.HttpShapeTreeClient;
import com.janeirodigital.shapetrees.core.DocumentResponse;
import com.janeirodigital.shapetrees.core.ShapeTreeContext;
import com.janeirodigital.shapetrees.core.contentloaders.DocumentLoaderManager;
import com.janeirodigital.shapetrees.core.contentloaders.ExternalDocumentLoader;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.tests.fixtures.DispatcherEntry;
import com.janeirodigital.shapetrees.tests.fixtures.RequestMatchingFixtureDispatcher;
import jdk.jfr.Label;
import lombok.SneakyThrows;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.jupiter.api.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static com.janeirodigital.shapetrees.tests.fixtures.MockWebServerHelper.toUrl;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class OkHttpShapeTreeClientTypeTests {

    private static RequestMatchingFixtureDispatcher dispatcher = null;
    private HttpClientFactory factory;
    private HttpClient fetcher;
    private HttpShapeTreeClient shapeTreeClient = new HttpShapeTreeClient();
    private final ShapeTreeContext context;
    private static final String TEXT_TURTLE = "text/turtle";

    public OkHttpShapeTreeClientTypeTests() {

        this.context = new ShapeTreeContext(null);
        this.factory = new OkHttpShapeTreeClientFactory(false, new BlackWhiteList(null, null));
        HttpClientFactoryManager.setFactory(this.factory);
        DocumentLoaderManager.setLoader((ExternalDocumentLoader) this.factory);

        this.skipShapeTreeValidation(false);  // Get an OkHttpShapeTreeClient from the HttpClientFactory set above

    }

    private void skipShapeTreeValidation(boolean b) {
        try {
            this.fetcher = this.factory.get(!b);
        } catch (ShapeTreeException e) {
            throw new Error(e);
        }
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

        dispatcher = new RequestMatchingFixtureDispatcher(dispatcherList);
    }

    @SneakyThrows
    @Test
    @Label("Create container when only containers are allowed")
    void createContainer() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Add fixture to handle successful POST response
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("type/valid-container-create-response"), "POST", "/containers/valid-container/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/containers/valid-container/.shapetree", null));
        DocumentResponse response;

        // Provide target shape tree
        response = shapeTreeClient.postManagedInstance(context, toUrl(server, "/containers/"), null, Arrays.asList(toUrl(server, "/static/shapetrees/type/shapetree#ContainerTree")), "valid-container", true, null, TEXT_TURTLE);
        Assertions.assertEquals(201, response.getStatusCode());

        // Do not provide target shape tree
        response = shapeTreeClient.postManagedInstance(context, toUrl(server, "/containers/"), null, null, "valid-container", true, null, TEXT_TURTLE);
        Assertions.assertEquals(201, response.getStatusCode());


    }

    @SneakyThrows
    @Test
    @Label("Fail to create resource when only containers are allowed")
    void failToCreateNonContainer() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        DocumentResponse response;

        // Provide target shape tree for a resource when container shape tree is expected
        response = shapeTreeClient.postManagedInstance(context, toUrl(server, "/containers/"), null, Arrays.asList(toUrl(server, "/static/shapetrees/type/shapetree#ResourceTree")), "invalid-resource", false, null, TEXT_TURTLE);
        Assertions.assertEquals(422, response.getStatusCode());

        // Provide target shape tree for a container even though what's being sent is a resource
        response = shapeTreeClient.postManagedInstance(context, toUrl(server, "/containers/"), null, Arrays.asList(toUrl(server, "/static/shapetrees/type/shapetree#ContainerTree")), "invalid-resource", false, null, TEXT_TURTLE);
        Assertions.assertEquals(422, response.getStatusCode());

        // Don't provide a target shape tree at all
        response = shapeTreeClient.postManagedInstance(context, toUrl(server, "/containers/"), null, null, "invalid-resource", false, null, TEXT_TURTLE);
        Assertions.assertEquals(422, response.getStatusCode());

    }

    @SneakyThrows
    @Test
    @Label("Create resource when only resources are allowed")
    void createResource() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Add fixture to handle successful POST response
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("type/valid-resource-create-response"), "POST", "/resources/valid-resource", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/resources/valid-resource.shapetree", null));
        DocumentResponse response;

        // Provide target shape tree
        response = shapeTreeClient.postManagedInstance(context, toUrl(server, "/resources/"), null, Arrays.asList(toUrl(server, "/static/shapetrees/type/shapetree#ResourceTree")), "valid-resource", false, null, TEXT_TURTLE);
        Assertions.assertEquals(201, response.getStatusCode());

        // Do not provide target shape tree
        response = shapeTreeClient.postManagedInstance(context, toUrl(server, "/resources/"), null, null, "valid-resource", false, null, TEXT_TURTLE);
        Assertions.assertEquals(201, response.getStatusCode());

    }

    @SneakyThrows
    @Test
    @Label("Fail to create container when only resources are allowed")
    void failToCreateNonResource() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        DocumentResponse response;

        // Provide target shape tree for a container when resource shape tree is expected
        response = shapeTreeClient.postManagedInstance(context, toUrl(server, "/resources/"), null, Arrays.asList(toUrl(server, "/static/shapetrees/type/shapetree#ContainerTree")), "invalid-container", true, null, TEXT_TURTLE);
        Assertions.assertEquals(422, response.getStatusCode());

        // Provide target shape tree for a resource even though what's being sent is a container
        response = shapeTreeClient.postManagedInstance(context, toUrl(server, "/resources/"), null, Arrays.asList(toUrl(server, "/static/shapetrees/type/shapetree#ResourceTree")), "invalid-container", true, null, TEXT_TURTLE);
        Assertions.assertEquals(422, response.getStatusCode());

        // Don't provide a target shape tree at all
        response = shapeTreeClient.postManagedInstance(context, toUrl(server, "/resources/"), null, null, "invalid-container", true, null, TEXT_TURTLE);
        Assertions.assertEquals(422, response.getStatusCode());

    }

    @SneakyThrows
    @Test
    @Label("Create non-rdf resource when only non-rdf resources are allowed")
    void createNonRDFResource() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Add fixture to handle successful POST response
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("type/valid-non-rdf-resource-create-response"), "POST", "/non-rdf-resources/valid-non-rdf-resource", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/non-rdf-resources/valid-non-rdf-resource.shapetree", null)); // TODO: Test: should this fail? should it have already failed?
        DocumentResponse response;

        response = shapeTreeClient.postManagedInstance(context, toUrl(server, "/non-rdf-resources/"), null, Arrays.asList(toUrl(server, "/static/shapetrees/type/shapetree#NonRDFResourceTree")), "valid-non-rdf-resource", false, null, "application/octet-stream");
        Assertions.assertEquals(201, response.getStatusCode());

        response = shapeTreeClient.postManagedInstance(context, toUrl(server, "/non-rdf-resources/"), null, null, "valid-non-rdf-resource", false, null, "application/octet-stream");
        Assertions.assertEquals(201, response.getStatusCode());
    }

    @SneakyThrows
    @Test
    @Label("Fail to create resource when only non-rdf resources are allowed")
    void failToCreateNonRDFResource() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        DocumentResponse response;

        // Provide target shape tree for a resource when non-rdf-resource shape tree is expected
        response = shapeTreeClient.postManagedInstance(context, toUrl(server, "/non-rdf-resources/"), null, Arrays.asList(toUrl(server, "/static/shapetrees/type/shapetree#ResourceTree")), "invalid-non-rdf-resource", false, null, TEXT_TURTLE);
        Assertions.assertEquals(422, response.getStatusCode());

        // Provide target shape tree for a non-rdf-resource even though what's being sent is a resource
        response = shapeTreeClient.postManagedInstance(context, toUrl(server, "/non-rdf-resources/"), null, Arrays.asList(toUrl(server, "/static/shapetrees/type/shapetree#NonRDFResourceTree")), "invalid-non-rdf-resource", false, null, TEXT_TURTLE);
        Assertions.assertEquals(422, response.getStatusCode());

        // Don't provide a target shape tree at all
        response = shapeTreeClient.postManagedInstance(context, toUrl(server, "/non-rdf-resources/"), null, null, "invalid-non-rdf-resource", false, null, TEXT_TURTLE);
        Assertions.assertEquals(422, response.getStatusCode());

    }

}
