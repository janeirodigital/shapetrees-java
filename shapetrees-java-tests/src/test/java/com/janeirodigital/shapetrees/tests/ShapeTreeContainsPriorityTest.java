package com.janeirodigital.shapetrees.tests;

import com.janeirodigital.shapetrees.core.ShapeTreeFactory;
import com.janeirodigital.shapetrees.core.contentloaders.DocumentLoaderManager;
import com.janeirodigital.shapetrees.core.contentloaders.HttpExternalDocumentLoader;
import com.janeirodigital.shapetrees.core.models.ShapeTree;
import com.janeirodigital.shapetrees.tests.fixtures.DispatcherEntry;
import com.janeirodigital.shapetrees.tests.fixtures.RequestMatchingFixtureDispatcher;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.net.URL;
import java.net.MalformedURLException;
import java.util.List;

@Slf4j
class ShapeTreeContainsPriorityTest {

    private static RequestMatchingFixtureDispatcher dispatcher = null;
    private static HttpExternalDocumentLoader httpExternalDocumentLoader;

    public ShapeTreeContainsPriorityTest() {
        httpExternalDocumentLoader = new HttpExternalDocumentLoader();
        DocumentLoaderManager.setLoader(httpExternalDocumentLoader);
    }

    protected URL getURL(MockWebServer server, String path) throws MalformedURLException {
        return new URL(server.url(path).toString());
    }

    @BeforeAll
    static void beforeAll() {
        dispatcher = new RequestMatchingFixtureDispatcher(List.of(
                new DispatcherEntry(List.of("shapetrees/contains-priority-shapetree-ttl"), "GET", "/static/shapetrees/contains-priority/shapetree", null)
        ));
    }

    @SneakyThrows
    @Test
    @DisplayName("Validate prioritized retrieval of all shape tree types")
    void testContainsPriorityOrderOfAllTreeTypes() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        ShapeTree containingShapeTree = ShapeTreeFactory.getShapeTree(getURL(server,"/static/shapetrees/contains-priority/shapetree#ContainsAllTypesTree"));

        // Ensure the ordered result is correct
        List<URL> prioritizedContains = containingShapeTree.getPrioritizedContains();

        Assertions.assertEquals(3, prioritizedContains.size());
        Assertions.assertEquals(getURL(server, "/static/shapetrees/contains-priority/shapetree#LabelShapeTypeTree"), prioritizedContains.get(0));
        Assertions.assertEquals(getURL(server, "/static/shapetrees/contains-priority/shapetree#LabelTypeTree"), prioritizedContains.get(1));
        Assertions.assertEquals(getURL(server, "/static/shapetrees/contains-priority/shapetree#TypeOnlyTree"), prioritizedContains.get(2));
    }

    @SneakyThrows
    @Test
    @DisplayName("Validate prioritized retrieval of shape trees with shape and resource type validation")
    void testContainsPriorityOrderOfShapeTypeTrees() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        ShapeTree containingShapeTree = ShapeTreeFactory.getShapeTree(getURL(server,"/static/shapetrees/contains-priority/shapetree#ContainsShapeTypeTree"));

        // Ensure the ordered result is correct
        List<URL> prioritizedContains = containingShapeTree.getPrioritizedContains();

        Assertions.assertEquals(2, prioritizedContains.size());
        Assertions.assertEquals(getURL(server, "/static/shapetrees/contains-priority/shapetree#ShapeTypeTree"), prioritizedContains.get(0));
        Assertions.assertEquals(getURL(server, "/static/shapetrees/contains-priority/shapetree#TypeOnlyTree"), prioritizedContains.get(1));
    }

    @SneakyThrows
    @Test
    @DisplayName("Validate prioritized retrieval of shape tree trees with label and resource type validation")
    void testContainsPriorityOrderOfLabelTypeTrees() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        ShapeTree containingShapeTree = ShapeTreeFactory.getShapeTree(getURL(server,"/static/shapetrees/contains-priority/shapetree#ContainsLabelTypeTree"));

        // Ensure the ordered result is correct
        List<URL> prioritizedContains = containingShapeTree.getPrioritizedContains();

        Assertions.assertEquals(2, prioritizedContains.size());
        Assertions.assertEquals(getURL(server, "/static/shapetrees/contains-priority/shapetree#LabelTypeTree"), prioritizedContains.get(0));
        Assertions.assertEquals(getURL(server, "/static/shapetrees/contains-priority/shapetree#TypeOnlyTree"), prioritizedContains.get(1));

    }

}
