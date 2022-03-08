package com.janeirodigital.shapetrees.tests.clienthttp;

import com.janeirodigital.shapetrees.core.DocumentResponse;
import com.janeirodigital.shapetrees.tests.fixtures.DispatcherEntry;
import com.janeirodigital.shapetrees.tests.fixtures.RequestMatchingFixtureDispatcher;
import jdk.jfr.Label;
import lombok.SneakyThrows;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.jupiter.api.*;

import java.net.URL;
import java.util.ArrayList;
import java.util.List;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class AbstractHttpClientProjectRecursiveTests extends AbstractHttpClientTests {

    private static RequestMatchingFixtureDispatcher dispatcher = null;

    public AbstractHttpClientProjectRecursiveTests() {
        // Call AbstractHttpClient constructor
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
        dispatcherList.add(new DispatcherEntry(List.of("project/task-6-container-no-contains"), "GET", "/data/projects/project-1/milestone-3/task-6/", null));
        dispatcherList.add(new DispatcherEntry(List.of("project/issue-2"), "GET", "/data/projects/project-1/milestone-3/issue-2", null));
        dispatcherList.add(new DispatcherEntry(List.of("project/issue-3"), "GET", "/data/projects/project-1/milestone-3/issue-3", null));
        dispatcherList.add(new DispatcherEntry(List.of("project/attachment-48"), "GET", "/data/projects/project-1/milestone-3/task-48/attachment-48", null));
        dispatcherList.add(new DispatcherEntry(List.of("project/random-png"), "GET", "/data/projects/project-1/milestone-3/task-48/random.png", null));
        dispatcherList.add(new DispatcherEntry(List.of("shapetrees/project-shapetree-ttl"), "GET", "/static/shapetrees/project/shapetree", null));
        dispatcherList.add(new DispatcherEntry(List.of("schemas/project-shex"), "GET", "/static/shex/project/shex", null));

        dispatcher = new RequestMatchingFixtureDispatcher(dispatcherList);
    }

/*
    @Order(1)
    @SneakyThrows
    @Test
    @Label("Recursively Plant Data Set")
    void plantDataRecursively() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/data/.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/data/projects/.shapetree", null));

        URL targetResource = toUrl(server, "/data/");
        URL targetShapeTree = toUrl(server, "/static/shapetrees/project/shapetree#DataRepositoryTree");
        URL focusNode = toUrl(server, "/data/#repository");

        // Plant the data collection recursively on already existing hierarchy
        DocumentResponse response = this.shapeTreeClient.plantShapeTree(this.context, targetResource, targetShapeTree, focusNode);
        Assertions.assertEquals(201, response.getStatusCode());

    }
*/

    @Order(2)
    @SneakyThrows
    @Test
    @Label("Recursively Plant Projects Collection")
    void plantProjectsRecursively() {
        MockWebServer server = new MockWebServer();
        server.start(8001);
        server.setDispatcher(dispatcher);

        // Add planted data set
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-manager"), "GET", "/data/.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container-manager"), "GET", "/data/projects/.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/data/projects/project-1/milestone-3/task-48/attachment-48.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/data/projects/project-1/milestone-3/task-48/random.png.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/data/projects/project-1/milestone-3/task-48/.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/data/projects/project-1/milestone-3/task-6/.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/data/projects/project-1/milestone-3/issue-3.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/data/projects/project-1/milestone-3/issue-2.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/data/projects/project-1/milestone-3/.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/data/projects/project-1/.shapetree", null));

        URL targetResource = toUrl(server, "/data/projects/");
        URL targetShapeTree = toUrl(server, "/static/shapetrees/project/shapetree#ProjectCollectionTree");

        // Plant the projects collection recursively on already existing hierarchy
        DocumentResponse response = this.shapeTreeClient.plantShapeTree(this.context, targetResource, targetShapeTree, null);
        Assertions.assertEquals(201, response.getStatusCode());

    }

}
