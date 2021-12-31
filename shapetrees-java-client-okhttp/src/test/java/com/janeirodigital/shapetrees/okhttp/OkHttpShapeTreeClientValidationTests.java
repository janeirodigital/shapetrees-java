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

import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static com.janeirodigital.shapetrees.core.enums.ContentType.TEXT_TURTLE;
import static com.janeirodigital.shapetrees.okhttp.OkHttpShapeTreeClient.post;
import static com.janeirodigital.shapetrees.tests.fixtures.MockWebServerHelper.toUrl;
import static org.junit.jupiter.api.Assertions.assertEquals;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class OkHttpShapeTreeClientValidationTests {

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

        // For this set of tests, we reinitialize the dispatcher set for every test, because almost every test needs a
        // slightly different context. Consequently, we could either modify the state from test to test (which felt a
        // little dirty as we couldn't run tests standalone, or set the context for each test (which we're doing)

        List dispatcherList = new ArrayList();

        dispatcherList.add(new DispatcherEntry(List.of("validation/container-1"), "GET", "/data/container-1/", null));
        dispatcherList.add(new DispatcherEntry(List.of("shapetrees/containment-shapetree-ttl"), "GET", "/static/shapetrees/validation/shapetree", null));
        dispatcherList.add(new DispatcherEntry(List.of("schemas/containment-shex"), "GET", "/static/shex/validation/shex", null));

        server = new MockWebServer();
        dispatcher = new RequestMatchingFixtureDispatcher(dispatcherList);
        server.setDispatcher(dispatcher);
    }

    @Test
    @DisplayName("Create resource - two containing trees, two shapes, two nodes")
    void validateTwoContainsTwoShapesTwoNodes() throws ShapeTreeException {

        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("validation/container-1-twocontains-manager"), "GET", "/data/container-1/.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("validation/resource-1-create-response"), "POST", "/data/container-1/resource-1", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/data/container-1/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/data/container-1/resource-1.shapetree", null));

        URL targetResource = toUrl(server, "/data/container-1/");
        List<URL> targetShapeTrees = Arrays.asList(toUrl(server, "/static/shapetrees/validation/shapetree#AttributeTree"),
                toUrl(server, "/static/shapetrees/validation/shapetree#ElementTree"));
        List<URL> focusNodes = Arrays.asList(toUrl(server, "/data/container-1/resource-1#resource"),
                toUrl(server, "/data/container-1/resource-1#element"));

        // Plant the data repository on newly created data container
        Response response = post(okHttpClient, context, targetResource, focusNodes, targetShapeTrees, "resource-1", false, getResource1BodyString(), TEXT_TURTLE);
        assertEquals(201, response.code());

    }

    @Test
    @DisplayName("Create resource - two containing trees of same tree")
    void validateTwoContainsSameContainingTree() throws ShapeTreeException {
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("validation/container-1-samecontains-manager"), "GET", "/data/container-1/.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("validation/resource-1-create-response"), "POST", "/data/container-1/resource-1", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/data/container-1/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/data/container-1/resource-1.shapetree", null));

        // Validate multiple contains, same shape tree, same node
        URL targetResource = toUrl(server, "/data/container-1/");
        List<URL> targetShapeTrees = Arrays.asList(toUrl(server, "/static/shapetrees/validation/shapetree#ChildTree"));
        List<URL> focusNodes = Arrays.asList(toUrl(server, "/data/container-1/resource-1#resource"));

        // Plant the data repository on newly created data container
        Response response = post(okHttpClient, context, targetResource, focusNodes, targetShapeTrees, "resource-1", false, getResource1BodyString(), TEXT_TURTLE);
        assertEquals(201, response.code());

    }

    @Test
    @DisplayName("Fail to create - two containing trees and focus node issues")
    void failToValidateTwoContainsWithBadFocusNodes() throws ShapeTreeException {
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("validation/container-1-twocontains-manager"), "GET", "/data/container-1/.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("validation/resource-1-create-response"), "POST", "/data/container-1/resource-1", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/data/container-1/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/data/container-1/resource-1.shapetree", null));

        URL targetResource = toUrl(server, "/data/container-1/");
        List<URL> targetShapeTrees = Arrays.asList(toUrl(server, "/static/shapetrees/validation/shapetree#AttributeTree"),
                toUrl(server, "/static/shapetrees/validation/shapetree#ElementTree"));

        // Only one matching target focus node is provided
        List<URL> focusNodes = Arrays.asList(toUrl(server, "/super/bad#node"));
        Response response = post(okHttpClient, context, targetResource, focusNodes, targetShapeTrees, "resource-1", false, getResource1BodyString(), TEXT_TURTLE);
        assertEquals(422, response.code());

        // Multiple non-matching target focus nodes are provided
        focusNodes = Arrays.asList(toUrl(server, "/super/bad#node"),
                toUrl(server, "/data/container-1/resource-1#badnode"),
                toUrl(server, "/data/container-1/#badnode"));
        response = post(okHttpClient, context, targetResource, focusNodes, targetShapeTrees, "resource-1", false, getResource1BodyString(), TEXT_TURTLE);
        assertEquals(422, response.code());

        // Only one matching target focus node is provided when two are needed
        focusNodes = Arrays.asList(toUrl(server, "/data/container-1/resource-1#resource"));
        response = post(okHttpClient, context, targetResource, focusNodes, targetShapeTrees, "resource-1", false, getResource1BodyString(), TEXT_TURTLE);
        assertEquals(422, response.code());
    }

    /* TODO - Cannot execute this test predicatably as constituted when passing focus nodes from client. Need to test closer to shape tree validation
    @SneakyThrows
    @Test
    @DisplayName("Fail to validate created resource - two containing trees, target node unused")
    void failToValidateTwoContainsTargetNodeUnused() {
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("validation/container-1-twocontains-onenode-manager"), "GET", "/data/container-1/.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("validation/resource-1-create-response"), "POST", "/data/container-1/resource-1", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/data/container-1/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/data/container-1/resource-1.shapetree", null));

        URL targetResource = toUrl(server, "/data/container-1/");
        List<URL> targetShapeTrees = Arrays.asList(toUrl(server, "/static/shapetrees/validation/shapetree#AttributeTree"),
                toUrl(server, "/static/shapetrees/validation/shapetree#ElementTree"));
        // Two target nodes are provided, but one of the nodes is matched twice, and the other isn't matched at all
        List<URL> focusNodes = Arrays.asList(toUrl(server, "/data/container-1/resource-1#resource"));

        DocumentResponse response = this.shapeTreeClient.postManagedInstance(context, targetResource, focusNodes, targetShapeTrees, "resource-1", false, getResource1BodyString(), TEXT_TURTLE);
        assertEquals(422, response.code());

    }
    */

    @SneakyThrows
    @Test
    @DisplayName("Fail to create - two containing trees, bad target shape trees")
    void failToValidateTwoContainsWithBadTargetShapeTrees() {
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("validation/container-1-twocontains-manager"), "GET", "/data/container-1/.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("validation/resource-1-create-response"), "POST", "/data/container-1/resource-1", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/data/container-1/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/data/container-1/resource-1.shapetree", null));

        URL targetResource = toUrl(server, "/data/container-1/");
        List<URL> focusNodes = Arrays.asList(toUrl(server, "/data/container-1/resource-1#resource"),
                toUrl(server, "/data/container-1/resource-1#element"));

        // Only one matching target shape tree is provided
        List<URL> targetShapeTrees = Arrays.asList(toUrl(server, "/static/shapetrees/validation/shapetree#AttributeTree"));
        Response response = post(okHttpClient, context, targetResource, focusNodes, targetShapeTrees, "resource-1", false, getResource1BodyString(), TEXT_TURTLE);
        assertEquals(422, response.code());

        // Multiple non-matching target focus nodes are provided
        targetShapeTrees = Arrays.asList(toUrl(server, "/static/shapetrees/validation/shapetree#OtherAttributeTree"),
                toUrl(server, "/static/shapetrees/validation/shapetree#OtherElementTree"));
        response = post(okHttpClient, context, targetResource, focusNodes, targetShapeTrees, "resource-1", false, getResource1BodyString(), TEXT_TURTLE);
        assertEquals(422, response.code());

        // One tree provided that isn't in either st:contains lists
        targetShapeTrees = Arrays.asList(toUrl(server, "/static/shapetrees/validation/shapetree#AttributeTree"),
                toUrl(server, "/static/shapetrees/validation/shapetree#StandaloneTree"));
        response = post(okHttpClient, context, targetResource, focusNodes, targetShapeTrees, "resource-1", false, getResource1BodyString(), TEXT_TURTLE);
        assertEquals(422, response.code());
    }

    private String
    getResource1BodyString() {
        return "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \n" +
                "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \n" +
                "PREFIX xml: <http://www.w3.org/XML/1998/namespace> \n" +
                "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> \n" +
                "PREFIX ex: <http://www.example.com/ns/ex#> \n" +
                "\n" +
                "\n" +
                "<#resource> \n" +
                "    ex:name \"Some Development Task\" ; \n" +
                "    ex:created_at \"2021-04-04T20:15:47.000Z\"^^xsd:dateTime . \n" +
                "\n" +
                "<#element> \n" +
                "    ex:name \"Some element\" ; \n" +
                "    ex:description \"This is a description of an element\" ; \n" +
                "    ex:created_at \"2021-04-04T20:15:47.000Z\"^^xsd:dateTime . \n" ;

    }

}
