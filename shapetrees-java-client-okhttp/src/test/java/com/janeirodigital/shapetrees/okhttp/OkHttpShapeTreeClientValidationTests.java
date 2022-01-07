package com.janeirodigital.shapetrees.okhttp;

import com.janeirodigital.shapetrees.core.ShapeTreeContext;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.tests.fixtures.RequestMatchingFixtureDispatcher;
import lombok.SneakyThrows;
import okhttp3.OkHttpClient;
import okhttp3.Response;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.jupiter.api.*;

import java.net.URL;
import java.util.Arrays;
import java.util.List;

import static com.janeirodigital.shapetrees.core.enums.ContentType.TEXT_TURTLE;
import static com.janeirodigital.shapetrees.okhttp.OkHttpShapeTreeClient.post;
import static com.janeirodigital.shapetrees.tests.fixtures.DispatcherHelper.mockOnGet;
import static com.janeirodigital.shapetrees.tests.fixtures.DispatcherHelper.mockOnPut;
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
        dispatcher = new RequestMatchingFixtureDispatcher();
        // Add fixture for shapetree resource containing shapetrees used by these tests for validation
        mockOnGet(dispatcher, "/static/shapetrees/validation/shapetree", "shapetrees/containment-shapetree-ttl");
        // Add fixture for shape resource containing shapes used by the validating shape trees
        mockOnGet(dispatcher, "/static/shex/validation/shex", "schemas/containment-shex");
        server = new MockWebServer();
        server.setDispatcher(dispatcher);
    }

    @Test
    @DisplayName("Create resource - two containing trees, two shapes, two nodes")
    void validateTwoContainsTwoShapesTwoNodes() throws ShapeTreeException {

        mockOnGet(dispatcher, "/data/container-1/", "validation/container-1");
        mockOnGet(dispatcher, "/data/container-1/.shapetree", "validation/container-1-twocontains-manager");
        mockOnGet(dispatcher, "/data/container-1/resource-1", List.of("http/404", "validation/resource-1"));
        mockOnPut(dispatcher, "/data/container-1/resource-1", "http/201");
        mockOnPut(dispatcher, "/data/container-1/resource-1.shapetree", "http/201");

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
        mockOnGet(dispatcher, "/data/container-1/", "validation/container-1");
        mockOnGet(dispatcher, "/data/container-1/.shapetree", "validation/container-1-samecontains-manager");
        mockOnGet(dispatcher, "/data/container-1/resource-1", List.of("http/404", "validation/resource-1"));
        mockOnPut(dispatcher, "/data/container-1/resource-1", "http/201");
        mockOnPut(dispatcher, "/data/container-1/resource-1.shapetree", "http/201");

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
        mockOnGet(dispatcher, "/data/container-1/", "validation/container-1");
        mockOnGet(dispatcher, "/data/container-1/.shapetree", "validation/container-1-twocontains-manager");
        mockOnGet(dispatcher, "/data/container-1/resource-1", "http/404");
        mockOnPut(dispatcher, "/data/container-1/resource-1", "http/201");
        mockOnPut(dispatcher, "/data/container-1/resource-1.shapetree", "http/201");

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

    @SneakyThrows
    @Test
    @DisplayName("Fail to create - two containing trees, bad target shape trees")
    void failToValidateTwoContainsWithBadTargetShapeTrees() {
        mockOnGet(dispatcher, "/data/container-1/", "validation/container-1");
        mockOnGet(dispatcher, "/data/container-1/.shapetree", "validation/container-1-twocontains-manager");
        mockOnGet(dispatcher, "/data/container-1/resource-1", "http/404");

        mockOnPut(dispatcher, "/data/container-1/resource-1", "http/201");
        mockOnPut(dispatcher, "/data/container-1/resource-1.shapetree", "http/201");

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
