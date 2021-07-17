package com.janeirodigital.shapetrees.client.okhttp;

import com.janeirodigital.shapetrees.client.okhttp.fixtures.DispatcherEntry;
import com.janeirodigital.shapetrees.client.okhttp.fixtures.RequestMatchingFixtureDispatcher;
import com.janeirodigital.shapetrees.core.ShapeTreeFactory;
import com.janeirodigital.shapetrees.core.models.ShapeTree;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.util.List;

@Slf4j
public class ShapeTreeContainsPriorityTest extends BaseShapeTreeTest {

    public ShapeTreeContainsPriorityTest() {
        super();
    }

    private static RequestMatchingFixtureDispatcher dispatcher = null;

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

        ShapeTree containingShapeTree = ShapeTreeFactory.getShapeTree(getURI(server,"/static/shapetrees/contains-priority/shapetree#ContainsAllTypesTree"));

        // Ensure the ordered result is correct
        List<URI> prioritizedContains = containingShapeTree.getPrioritizedContains();

        Assertions.assertEquals(prioritizedContains.size(), 3);
        Assertions.assertEquals(prioritizedContains.get(0), getURI(server, "/static/shapetrees/contains-priority/shapetree#LabelShapeTypeTree"));
        Assertions.assertEquals(prioritizedContains.get(1), getURI(server, "/static/shapetrees/contains-priority/shapetree#LabelTypeTree"));
        Assertions.assertEquals(prioritizedContains.get(2), getURI(server, "/static/shapetrees/contains-priority/shapetree#TypeOnlyTree"));
    }

    @SneakyThrows
    @Test
    @DisplayName("Validate prioritized retrieval of shape trees with shape and resource type validation")
    void testContainsPriorityOrderOfShapeTypeTrees() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        ShapeTree containingShapeTree = ShapeTreeFactory.getShapeTree(getURI(server,"/static/shapetrees/contains-priority/shapetree#ContainsShapeTypeTree"));

        // Ensure the ordered result is correct
        List<URI> prioritizedContains = containingShapeTree.getPrioritizedContains();

        Assertions.assertEquals(prioritizedContains.size(), 2);
        Assertions.assertEquals(prioritizedContains.get(0), getURI(server, "/static/shapetrees/contains-priority/shapetree#ShapeTypeTree"));
        Assertions.assertEquals(prioritizedContains.get(1), getURI(server, "/static/shapetrees/contains-priority/shapetree#TypeOnlyTree"));
    }

    @SneakyThrows
    @Test
    @DisplayName("Validate prioritized retrieval of shape tree trees with label and resource type validation")
    void testContainsPriorityOrderOfLabelTypeTrees() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        ShapeTree containingShapeTree = ShapeTreeFactory.getShapeTree(getURI(server,"/static/shapetrees/contains-priority/shapetree#ContainsLabelTypeTree"));

        // Ensure the ordered result is correct
        List<URI> prioritizedContains = containingShapeTree.getPrioritizedContains();

        Assertions.assertEquals(prioritizedContains.size(), 2);
        Assertions.assertEquals(prioritizedContains.get(0), getURI(server, "/static/shapetrees/contains-priority/shapetree#LabelTypeTree"));
        Assertions.assertEquals(prioritizedContains.get(1), getURI(server, "/static/shapetrees/contains-priority/shapetree#TypeOnlyTree"));

    }

}
