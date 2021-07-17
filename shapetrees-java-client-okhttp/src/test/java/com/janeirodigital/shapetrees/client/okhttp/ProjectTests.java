package com.janeirodigital.shapetrees.client.okhttp;

import com.janeirodigital.shapetrees.client.okhttp.fixtures.DispatcherEntry;
import com.janeirodigital.shapetrees.client.okhttp.fixtures.RequestMatchingFixtureDispatcher;
import com.janeirodigital.shapetrees.core.models.ShapeTreeLocator;
import jdk.jfr.Label;
import lombok.SneakyThrows;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.jupiter.api.*;

import java.util.ArrayList;
import java.util.List;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class ProjectTests extends BaseShapeTreeTest {

    private static RequestMatchingFixtureDispatcher dispatcher = null;

    public ProjectTests() {
        // Call BaseShapeTreeTest constructor
        super();
    }

    @BeforeAll
    static void beforeAll() {

        List dispatcherList = new ArrayList();

        dispatcherList.add(new DispatcherEntry(List.of("project/root-container"), "GET", "/", null));
        dispatcherList.add(new DispatcherEntry(List.of("project/root-container-locator"), "GET", "/.shapetree", null));
        dispatcherList.add(new DispatcherEntry(List.of("shapetrees/project-shapetree-ttl"), "GET", "/static/shapetrees/project/shapetree", null));
        dispatcherList.add(new DispatcherEntry(List.of("schemas/project-shex"), "GET", "/static/shex/project/shex", null));

        dispatcher = new RequestMatchingFixtureDispatcher(dispatcherList);
    }

    @Order(1)
    @SneakyThrows
    @Test
    @Label("Discover unmanaged root resource")
    void discoverUnmanagedRoot() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Use the discover operation to see if the root container is managed
        ShapeTreeLocator locator = this.shapeTreeClient.discoverShapeTree(this.context, getURI(server,"/"));

        // The root container isn't managed so check to ensure that a NULL value is returned
        Assertions.assertNull(locator);
    }

    @Order(2)
    @SneakyThrows
    @Test
    @Label("Fail to plant on a non-existent data container")
    void failPlantOnMissingDataContainer() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Perform plant on /data container that doesn't exist yet (fails)

        // Ensure that all of the attributes are valid

    }

    @Order(3)
    @SneakyThrows
    @Test
    @Label("Create Then Plant Data Container")
    void createThenPlantData() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Create the data container
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container"),
                                                                            "GET",
                                                                            "/data/",
                                                                            null));

        // Call the plant shape tree operation, which will find nothing planted
        // Ensure that the plant was successful

        // Plant a shape tree to manage it
        // TODO - Setting the locator for the time-being
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-locator"),
                "GET",
                "/data/.shapetree",
                null));

    }

    @Order(4)
    @SneakyThrows
    @Test
    @Label("Create Project Container and Plant Data Collection")
    void createProjectsAndPlantTrees() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Create projects container
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container"),
                "GET",
                "/data/projects/",
                null));

        // plant the data collection shape tree (success)

    }

    @Order(5)
    @SneakyThrows
    @Test
    @Label("Plant Second Shape Tree on Project Container")
    void plantSecondShapeTreeOnProject() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Create an intersecting shape tree (essentially two locations) - success
        // Combine this with prior?

    }

    @Order(6)
    @SneakyThrows
    @Test
    @Label("Create Project in the Projects Collection")
    void createProjectInProjects() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // create project-1 in the project. this should be successful

    }

    @Order(7)
    @SneakyThrows
    @Test
    @Label("Fail to Create a Malformed Project in the Projects Collection")
    void failToCreateMalformedProject() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // create a malformed project-2 (shape validation fails)
        // should get a validation failure

    }

    @Order(8)
    @SneakyThrows
    @Test
    @Label("Fail to Update a Project to be Malformed in the Projects Collection")
    void failToUpdateMalformedProject() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // try to update an existing project-1 to be malformed and fail validation

    }

    @Order(9)
    @SneakyThrows
    @Test
    @Label("Create First Task in Project Without Focus Node")
    void createFirstTaskInProjectWithoutFocusNode() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // create task-48 in project-1 - supply a shape tree, but not a focus node

    }

    @Order(10)
    @SneakyThrows
    @Test
    @Label("Create Second Task in Project Without Target Shape Tree or Focus Node")
    void createSecondTaskInProjectWithoutAnyContext() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // create task-43 in project-1 - don't support a shape tree or a focus node

    }

    @Order(11)
    @SneakyThrows
    @Test
    @Label("Create Attachment in Task")
    void createAttachmentInTask() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // create an attachment in task-48 (success)

    }

    @Order(12)
    @SneakyThrows
    @Test
    @Label("Create Second Attachment in Task")
    void createSecondAttachmentInTask() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // create a second attachment in task-48 (success)

    }

    @Order(13)
    @SneakyThrows
    @Test
    @Label("Fail to Unplant Non-Root Task")
    void failToUnplantNonRootTask() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Fail to unplant at the non-root task level (fail)

    }

    @Order(14)
    @SneakyThrows
    @Test
    @Label("Unplant Projects Collection")
    void unplantProjects() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Unplant the project collection, recursing down the tree (success)

    }

    @Order(15)
    @SneakyThrows
    @Test
    @Label("Unplant Data Set")
    void unplantData() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Unplant the data collection, recursing down the tree (only two levels) (success)

    }

}
