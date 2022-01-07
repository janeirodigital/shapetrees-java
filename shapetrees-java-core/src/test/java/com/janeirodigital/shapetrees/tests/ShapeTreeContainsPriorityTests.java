package com.janeirodigital.shapetrees.tests;

import com.janeirodigital.shapetrees.core.ShapeTree;
import com.janeirodigital.shapetrees.core.ShapeTreeFactory;
import com.janeirodigital.shapetrees.core.contentloaders.DocumentLoaderManager;
import com.janeirodigital.shapetrees.core.contentloaders.HttpExternalDocumentLoader;
import com.janeirodigital.shapetrees.tests.fixtures.MockWebServerHelper;
import com.janeirodigital.shapetrees.tests.fixtures.RequestMatchingFixtureDispatcher;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.net.URL;
import java.util.List;

import static com.janeirodigital.shapetrees.tests.fixtures.DispatcherHelper.mockOnGet;

@Slf4j
class ShapeTreeContainsPriorityTests {

    private static MockWebServer server;

    public ShapeTreeContainsPriorityTests() {
        HttpExternalDocumentLoader httpExternalDocumentLoader = new HttpExternalDocumentLoader();
        DocumentLoaderManager.setLoader(httpExternalDocumentLoader);
    }

    @BeforeAll
    static void beforeAll() {
        RequestMatchingFixtureDispatcher dispatcher = new RequestMatchingFixtureDispatcher();
        mockOnGet(dispatcher, "/static/shapetrees/contains-priority/shapetree", "shapetrees/contains-priority-shapetree-ttl");
        server = new MockWebServer();
        server.setDispatcher(dispatcher);
    }

    @SneakyThrows
    @Test
    @DisplayName("Validate prioritized retrieval of all shape tree types")
    void testContainsPriorityOrderOfAllTreeTypes() {
        ShapeTree containingShapeTree = ShapeTreeFactory.getShapeTree(MockWebServerHelper.toUrl(server,"/static/shapetrees/contains-priority/shapetree#ContainsAllTypesTree"));

        // Ensure the ordered result is correct
        List<URL> prioritizedContains = containingShapeTree.getPrioritizedContains();

        Assertions.assertEquals(3, prioritizedContains.size());
        Assertions.assertEquals(MockWebServerHelper.toUrl(server, "/static/shapetrees/contains-priority/shapetree#LabelShapeTypeTree"), prioritizedContains.get(0));
        Assertions.assertEquals(MockWebServerHelper.toUrl(server, "/static/shapetrees/contains-priority/shapetree#LabelTypeTree"), prioritizedContains.get(1));
        Assertions.assertEquals(MockWebServerHelper.toUrl(server, "/static/shapetrees/contains-priority/shapetree#TypeOnlyTree"), prioritizedContains.get(2));
    }

    @SneakyThrows
    @Test
    @DisplayName("Validate prioritized retrieval of shape trees with shape and resource type validation")
    void testContainsPriorityOrderOfShapeTypeTrees() {
        ShapeTree containingShapeTree = ShapeTreeFactory.getShapeTree(MockWebServerHelper.toUrl(server,"/static/shapetrees/contains-priority/shapetree#ContainsShapeTypeTree"));

        // Ensure the ordered result is correct
        List<URL> prioritizedContains = containingShapeTree.getPrioritizedContains();

        Assertions.assertEquals(2, prioritizedContains.size());
        Assertions.assertEquals(MockWebServerHelper.toUrl(server, "/static/shapetrees/contains-priority/shapetree#ShapeTypeTree"), prioritizedContains.get(0));
        Assertions.assertEquals(MockWebServerHelper.toUrl(server, "/static/shapetrees/contains-priority/shapetree#TypeOnlyTree"), prioritizedContains.get(1));
    }

    @SneakyThrows
    @Test
    @DisplayName("Validate prioritized retrieval of shape tree trees with label and resource type validation")
    void testContainsPriorityOrderOfLabelTypeTrees() {
        ShapeTree containingShapeTree = ShapeTreeFactory.getShapeTree(MockWebServerHelper.toUrl(server,"/static/shapetrees/contains-priority/shapetree#ContainsLabelTypeTree"));

        // Ensure the ordered result is correct
        List<URL> prioritizedContains = containingShapeTree.getPrioritizedContains();

        Assertions.assertEquals(2, prioritizedContains.size());
        Assertions.assertEquals(MockWebServerHelper.toUrl(server, "/static/shapetrees/contains-priority/shapetree#LabelTypeTree"), prioritizedContains.get(0));
        Assertions.assertEquals(MockWebServerHelper.toUrl(server, "/static/shapetrees/contains-priority/shapetree#TypeOnlyTree"), prioritizedContains.get(1));
    }

}
