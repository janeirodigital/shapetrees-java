package com.janeirodigital.shapetrees.client.okhttp;

import com.janeirodigital.shapetrees.client.okhttp.fixtures.DispatcherEntry;
import com.janeirodigital.shapetrees.client.okhttp.fixtures.RequestMatchingFixtureDispatcher;
import com.janeirodigital.shapetrees.core.ShapeTreeResponse;
import jdk.jfr.Label;
import lombok.SneakyThrows;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.jupiter.api.*;

import java.util.ArrayList;
import java.util.List;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class ProjectRecursiveTests extends BaseShapeTreeTest {

    private static RequestMatchingFixtureDispatcher dispatcher = null;

    public ProjectRecursiveTests() {
        // Call BaseShapeTreeTest constructor
        super();
    }

    @BeforeAll
    static void beforeAll() {

        List dispatcherList = new ArrayList();

        dispatcherList.add(new DispatcherEntry(List.of("project/data-container"), "GET", "/data/", null));
        dispatcherList.add(new DispatcherEntry(List.of("project/projects-container"), "GET", "/data/projects/", null));
        dispatcherList.add(new DispatcherEntry(List.of("project/project-1-container"), "GET", "/data/projects/project-1/", null));
        dispatcherList.add(new DispatcherEntry(List.of("project/milestone-3-container"), "GET", "/data/projects/project-1/milestone-3/", null));
        dispatcherList.add(new DispatcherEntry(List.of("project/task-48-container"), "GET", "/data/projects/project-1/milestone-3/task-48/", null));
        dispatcherList.add(new DispatcherEntry(List.of("project/issue-2"), "GET", "/data/projects/project-1/milestone-3/issue-2", null));
        dispatcherList.add(new DispatcherEntry(List.of("project/issue-3"), "GET", "/data/projects/project-1/milestone-3/issue-3", null));
        dispatcherList.add(new DispatcherEntry(List.of("project/attachment-48"), "GET", "/data/projects/project-1/milestone-3/task-48/attachment-48", null));
        dispatcherList.add(new DispatcherEntry(List.of("project/random-png"), "GET", "/data/projects/project-1/milestone-3/task-48/random.png", null));
        dispatcherList.add(new DispatcherEntry(List.of("shapetrees/project-shapetree-ttl"), "GET", "/static/shapetrees/project/shapetree", null));
        dispatcherList.add(new DispatcherEntry(List.of("schemas/project-shex"), "GET", "/static/shex/project/shex", null));

        dispatcher = new RequestMatchingFixtureDispatcher(dispatcherList);
    }

    @Order(1)
    @SneakyThrows
    @Test
    @Label("Recursively Plant Data Set")
    void plantDataRecursively() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Plant the data collection recursively on already existing hierarchy
        ShapeTreeResponse response = this.shapeTreeClient.plantShapeTree(this.context, getURI(server, "/data/"), getURI(server, "/static/shapetrees/project/shapetree#DataRepositoryTree"), getURI(server, "/data/#repository").toString(), true);
        Assertions.assertEquals(201, response.getStatusCode());

    }

    @Order(2)
    @SneakyThrows
    @Test
    @Label("Recursively Plant Projects Collection")
    void plantProjectsRecursively() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Add planted data set
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-locator"), "GET", "/data/.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container-locator"), "GET", "/data/projects/.shapetree", null));

        // Plant the projects collection recursively on already existing hierarchy
        ShapeTreeResponse response = this.shapeTreeClient.plantShapeTree(this.context, getURI(server, "/data/projects/"), getURI(server, "/static/shapetrees/project/shapetree#ProjectCollectionTree"), null, true);
        Assertions.assertEquals(201, response.getStatusCode());

    }

}
