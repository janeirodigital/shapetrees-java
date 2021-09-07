package com.janeirodigital.shapetrees.okhttp;

import com.janeirodigital.shapetrees.core.DocumentResponse;
import com.janeirodigital.shapetrees.okhttp.fixtures.DispatcherEntry;
import com.janeirodigital.shapetrees.okhttp.fixtures.RequestMatchingFixtureDispatcher;
import com.janeirodigital.shapetrees.core.models.ShapeTreeLocator;
import jdk.jfr.Label;
import lombok.SneakyThrows;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.jupiter.api.*;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class ProjectTests extends BaseShapeTreeTest {

    private static RequestMatchingFixtureDispatcher dispatcher = null;

    public ProjectTests() {
        // Call BaseShapeTreeTest constructor
        super();
    }

    @BeforeEach
    void initializeDispatcher() {

        // For this set of tests, we reinitialize the dispatcher set for every test, because almost every test needs a
        // slightly different context. Consequently, we could either modify the state from test to test (which felt a
        // little dirty as we couldn't run tests standalone, or set the context for each test (which we're doing)

        List dispatcherList = new ArrayList();

        dispatcherList.add(new DispatcherEntry(List.of("project/root-container"), "GET", "/", null));
        dispatcherList.add(new DispatcherEntry(List.of("project/root-container-locator"), "GET", "/.shapetree", null));
        dispatcherList.add(new DispatcherEntry(List.of("shapetrees/project-shapetree-ttl"), "GET", "/static/shapetrees/project/shapetree", null));
        dispatcherList.add(new DispatcherEntry(List.of("schemas/project-shex"), "GET", "/static/shex/project/shex", null));

        dispatcher = new RequestMatchingFixtureDispatcher(dispatcherList);
    }

    @SneakyThrows
    @Test
    @Label("Discover unmanaged root resource")
    void discoverUnmanagedRoot() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        URI targetResource = getURI(server,"/");

        // Use the discover operation to see if the root container is managed
        ShapeTreeLocator locator = this.shapeTreeClient.discoverShapeTree(this.context, targetResource);

        // The root container isn't managed so check to ensure that a NULL value is returned
        Assertions.assertNull(locator);
    }

    @SneakyThrows
    @Test
    @Label("Fail to plant on a non-existent data container")
    void failPlantOnMissingDataContainer() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        URI targetResource = getURI(server,"/data/");
        URI targetShapeTree = getURI(server, "/static/shapetrees/project/shapetree#DataRepositoryTree");

        // Perform plant on /data container that doesn't exist yet (fails)
        DocumentResponse response = this.shapeTreeClient.plantShapeTree(this.context, targetResource, targetShapeTree, null);
        // Look for 404 because /data doesn't exist
        Assertions.assertEquals(404, response.getStatusCode());

    }

    @SneakyThrows
    @Test
    @Label("Plant Data Repository")
    void plantDataRepository() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Create the data container
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-no-contains"), "GET", "/data/", null));

        URI targetResource = getURI(server, "/data/");
        URI targetShapeTree = getURI(server, "/static/shapetrees/project/shapetree#DataRepositoryTree");
        URI focusNode = getURI(server, "/data/#repository");

        // Plant the data repository on newly created data container
        DocumentResponse response = this.shapeTreeClient.plantShapeTree(this.context, targetResource, targetShapeTree, focusNode);
        Assertions.assertEquals(201, response.getStatusCode());

    }

    @SneakyThrows
    @Test
    @Label("Fail to plant on missing shape tree")
    void failPlantOnMissingShapeTree() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Create the data container
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-no-contains"), "GET", "/data/", null));

        URI targetResource = getURI(server, "/data/");
        URI targetShapeTree = getURI(server, "/static/shapetrees/missing/shapetree#NonExistentTree");
        URI focusNode = getURI(server, "/data/#repository");

        // Plant the data repository on newly created data container
        DocumentResponse response = this.shapeTreeClient.plantShapeTree(this.context, targetResource, targetShapeTree, focusNode);
        Assertions.assertEquals(500, response.getStatusCode());

    }

    @SneakyThrows
    @Test
    @Label("Create Projects Container and Validate DataCollectionTree")
    void createAndValidateProjects() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Setup initial fixtures for /data/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container"), "GET", "/data/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-locator"), "GET", "/data/.shapetree", null));
        // Add fixture for /projects/ to handle the POST response
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container-no-contains"), "POST", "/data/projects/", null));

        URI parentContainer = getURI(server, "/data/");
        URI focusNode = getURI(server, "/data/projects/#collection");
        URI targetShapeTree = getURI(server, "/static/shapetrees/project/shapetree#DataCollectionTree");

        // Create the projects container as a shape tree instance.
        // 1. Will be validated by the parent DataRepositoryTree planted on /data
        // 2. Will have a locator/location created for it as an instance of DataCollectionTree
        DocumentResponse response = shapeTreeClient.postShapeTreeInstance(context, parentContainer, focusNode, targetShapeTree, "projects", true, getProjectsBodyGraph(), TEXT_TURTLE);
        Assertions.assertEquals(201, response.getStatusCode());

    }

    @SneakyThrows
    @Test
    @Label("Plant ProjectCollectionTree on Projects Container")
    void plantSecondShapeTreeOnProjects() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Add fixtures for /data/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container"), "GET", "/data/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-locator"), "GET", "/data/.shapetree", null));
        // Add fixtures for /projects/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container-no-contains"), "GET", "/data/projects/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container-locator"), "GET", "/data/projects/.shapetree", null));

        URI targetResource = getURI(server, "/data/projects/");
        URI targetShapeTree = getURI(server, "/static/shapetrees/project/shapetree#ProjectCollectionTree");
        URI focusNode = getURI(server, "/data/projects/#collection");

        // Plant the second shape tree (ProjectCollectionTree) on /data/projects/
        DocumentResponse response = this.shapeTreeClient.plantShapeTree(this.context, targetResource, targetShapeTree, focusNode);
        Assertions.assertEquals(201, response.getStatusCode());


    }

    @SneakyThrows
    @Test
    @Label("Create Project in the Projects Collection")
    void createProjectInProjects() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Add fixtures for /data/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container"), "GET", "/data/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-locator"), "GET", "/data/.shapetree", null));
        // Add fixtures for /projects/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container-no-contains"), "GET", "/data/projects/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container-locator-two-locations"), "GET", "/data/projects/.shapetree", null));
        // Add fixture for /projects/project-1/ to handle the POST response
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-no-contains"), "POST", "/data/projects/project-1/", null));

        URI parentContainer = getURI(server, "/data/projects/");
        URI focusNode = getURI(server, "/data/projects/project-1/#project");
        URI targetShapeTree = getURI(server, "/static/shapetrees/project/shapetree#ProjectTree");

        // Create the project-1 container as a shape tree instance.
        // 1. Will be validated by the parent ProjectCollectionTree planted on /data/projects/
        // 2. Will have a locator/location created for it as an instance of ProjectTree
        DocumentResponse response = shapeTreeClient.postShapeTreeInstance(context, parentContainer, focusNode, targetShapeTree, "project-1", true, getProjectOneBodyGraph(), TEXT_TURTLE);
        Assertions.assertEquals(201, response.getStatusCode());

    }

    @SneakyThrows
    @Test
    @Label("Update Project in the Projects Collection")
    void updateProjectInProjects() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Add fixtures for /data/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container"), "GET", "/data/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-locator"), "GET", "/data/.shapetree", null));
        // Add fixtures for /projects/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container"), "GET", "/data/projects/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container-locator-two-locations"), "GET", "/data/projects/.shapetree", null));
        // Add fixture for /projects/project-1/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-no-contains"), "GET", "/data/projects/project-1/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-locator"), "GET", "/data/projects/project-1/.shapetree", null));
        // Add fixture for updated project-1
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-no-contains-updated"), "PUT", "/data/projects/project-1/", null));

        URI targetResource = getURI(server, "/data/projects/project-1/");
        URI focusNode = getURI(server, "/data/projects/project-1/#project");

        // Update the project-1 container as a shape tree instance.
        // 1. Will be validated by the parent ProjectCollectionTree planted on /data/projects/
        // 2. Will have a locator/location created for it as an instance of ProjectTree
        DocumentResponse response = shapeTreeClient.putShapeTreeInstance(context, targetResource, focusNode, getProjectOneUpdatedBodyGraph(), TEXT_TURTLE);
        Assertions.assertEquals(200, response.getStatusCode());

    }

    @SneakyThrows
    @Test
    @Label("Fail to Create a Malformed Project in the Projects Collection")
    void failToCreateMalformedProject() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Add fixtures for /data/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container"), "GET", "/data/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-locator"), "GET", "/data/.shapetree", null));
        // Add fixtures for /projects/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container"), "GET", "/data/projects/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container-locator-two-locations"), "GET", "/data/projects/.shapetree", null));

        URI targetResource = getURI(server, "/data/projects/project-1/");
        URI focusNode = getURI(server, "/data/projects/project-1/#project");
        URI targetShapeTree = getURI(server, "/static/shapetrees/project/shapetree#ProjectTree");

        // Create the project-1 container as a shape tree instance via PUT
        // 1. Will be validated by the parent ProjectCollectionTree planted on /data/projects/
        DocumentResponse response = shapeTreeClient.putShapeTreeInstance(context, targetResource, focusNode, targetShapeTree, true, getProjectOneMalformedBodyGraph(), TEXT_TURTLE);
        // 2. Will fail validation because the body content doesn't validate against the assigned shape
        Assertions.assertEquals(422, response.getStatusCode());

    }

    @SneakyThrows
    @Test
    @Label("Fail to Update a Project to be Malformed in the Projects Collection")
    void failToUpdateMalformedProject() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // try to update an existing project-1 to be malformed and fail validation
        // Add fixtures for /data/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container"), "GET", "/data/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-locator"), "GET", "/data/.shapetree", null));
        // Add fixtures for /projects/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container"), "GET", "/data/projects/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container-locator-two-locations"), "GET", "/data/projects/.shapetree", null));
        // Add fixture for /projects/project-1/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-no-contains"), "GET", "/data/projects/project-1/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-locator"), "GET", "/data/projects/project-1/.shapetree", null));

        URI targetResource = getURI(server, "/data/projects/project-1/");
        URI focusNode = getURI(server, "/data/projects/project-1/#project");

        // Update the project-1 container as a shape tree instance via PUT
        // 1. Will be validated by the parent ProjectCollectionTree planted on /data/projects/
        DocumentResponse response = shapeTreeClient.putShapeTreeInstance(context, targetResource, focusNode, getProjectOneMalformedBodyGraph(), TEXT_TURTLE);
        // 2. Will fail validation because the body content doesn't validate against the assigned shape
        Assertions.assertEquals(422, response.getStatusCode());

    }

    @SneakyThrows
    @Test
    @Label("Create Milestone in Project With Put")
    void createMilestoneInProjectWithPut() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Add fixtures for /data/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container"), "GET", "/data/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-locator"), "GET", "/data/.shapetree", null));
        // Add fixtures for /projects/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container"), "GET", "/data/projects/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container-locator-two-locations"), "GET", "/data/projects/.shapetree", null));
        // Add fixture for /projects/project-1/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-no-contains"), "GET", "/data/projects/project-1/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-locator"), "GET", "/data/projects/project-1/.shapetree", null));
        // Add fixture for /projects/project-1/milestone-3 to handle response to create via PUT
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/milestone-3-container-no-contains"), "PUT", "/data/projects/project-1/milestone-3/", null));

        URI targetResource = getURI(server, "/data/projects/project-1/milestone-3/");
        URI focusNode = getURI(server, "/data/projects/project-1/milestone-3/#milestone");
        URI targetShapeTree = getURI(server, "/static/shapetrees/project/shapetree#MilestoneTree");

        // Create the milestone-3 container in /projects/project-1/ as a shape tree instance using PUT to create
        // 1. Will be validated by the parent ProjectTree planted on /data/projects/project-1/
        // 2. Will have a locator/location created for it as an instance of MilestoneTree
        DocumentResponse response = shapeTreeClient.putShapeTreeInstance(context, targetResource, focusNode, targetShapeTree, true, getMilestoneThreeBodyGraph(), TEXT_TURTLE);
        Assertions.assertEquals(201, response.getStatusCode());

    }

    @SneakyThrows
    @Test
    @Label("Update Milestone in Project With Patch")
    void updateMilestoneInProjectWithPatch() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Add fixtures for /data/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container"), "GET", "/data/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-locator"), "GET", "/data/.shapetree", null));
        // Add fixtures for /projects/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container"), "GET", "/data/projects/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container-locator-two-locations"), "GET", "/data/projects/.shapetree", null));
        // Add fixture for /projects/project-1/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-no-contains"), "GET", "/data/projects/project-1/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-locator"), "GET", "/data/projects/project-1/.shapetree", null));
        // Add fixture for /projects/project-1/milestone-3/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/milestone-3-container-no-contains"), "GET", "/data/projects/project-1/milestone-3/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/milestone-3-container-locator"), "GET", "/data/projects/project-1/milestone-3/.shapetree", null));
        // Add fixture for /projects/project-1/milestone-3 to handle response to update via PATCH
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/milestone-3-container-no-contains-updated"), "PATCH", "/data/projects/project-1/milestone-3/", null));

        URI targetResource = getURI(server, "/data/projects/project-1/milestone-3/");
        URI focusNode = getURI(server, "/data/projects/project-1/milestone-3/#milestone");

        // Update the milestone-3 container in /projects/project-1/ using PATCH
        // 1. Will be validated by the MilestoneTree planted on /data/projects/project-1/milestone-3/
        DocumentResponse response = shapeTreeClient.patchShapeTreeInstance(context, targetResource, focusNode, getMilestoneThreeSparqlPatch());
        Assertions.assertEquals(201, response.getStatusCode());

    }

    @SneakyThrows
    @Test
    @Label("Create First Task in Project With Patch")
    void createFirstTaskInProjectWithPatch() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Add fixtures for /data/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container"), "GET", "/data/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-locator"), "GET", "/data/.shapetree", null));
        // Add fixtures for /projects/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container"), "GET", "/data/projects/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container-locator-two-locations"), "GET", "/data/projects/.shapetree", null));
        // Add fixture for /projects/project-1/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-no-contains"), "GET", "/data/projects/project-1/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-locator"), "GET", "/data/projects/project-1/.shapetree", null));
        // Add fixture for /projects/project-1/milestone-3/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/milestone-3-container-no-contains"), "GET", "/data/projects/project-1/milestone-3/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/milestone-3-container-locator"), "GET", "/data/projects/project-1/milestone-3/.shapetree", null));
        // Add fixture for /projects/project-1/milestone-3/task-6/ to handle response to update via PATCH
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/task-6-container-no-contains-updated"), "PATCH", "/data/projects/project-1/milestone-3/task-6/", null));

        URI targetResource = getURI(server, "/data/projects/project-1/milestone-3/task-6/");
        URI focusNode = getURI(server, "/data/projects/project-1/milestone-3/task-6/#task");

        // Create the task-6 container in /projects/project-1/milestone-3/ using PATCH
        // 1. Will be validated by the parent MilestoneTree planted on /data/projects/project-1/milestone-3/
        DocumentResponse response = shapeTreeClient.patchShapeTreeInstance(context, targetResource, focusNode, getTaskSixSparqlPatch());
        Assertions.assertEquals(201, response.getStatusCode());

    }

    @SneakyThrows
    @Test
    @Label("Create Second Task in Project Without Focus Node")
    void createSecondTaskInProjectWithoutFocusNode() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Add fixtures for /data/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container"), "GET", "/data/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-locator"), "GET", "/data/.shapetree", null));
        // Add fixtures for /projects/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container"), "GET", "/data/projects/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container-locator-two-locations"), "GET", "/data/projects/.shapetree", null));
        // Add fixture for /projects/project-1/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-no-contains"), "GET", "/data/projects/project-1/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-locator"), "GET", "/data/projects/project-1/.shapetree", null));
        // Add fixture for /projects/project-1/milestone-3/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/milestone-3-container-no-contains"), "GET", "/data/projects/project-1/milestone-3/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/milestone-3-container-locator"), "GET", "/data/projects/project-1/milestone-3/.shapetree", null));
        // Add fixture for /projects/project-1/milestone-3/task-6/ to handle response to create via POST
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/task-48-container-no-contains"), "POST", "/data/projects/project-1/milestone-3/task-48/", null));

        URI targetContainer = getURI(server, "/data/projects/project-1/milestone-3/");
        URI targetShapeTree = getURI(server, "/static/shapetrees/project/shapetree#TaskTree");

        // create task-48 in milestone-3 - supply a target shape tree, but not a focus node
        DocumentResponse response = shapeTreeClient.postShapeTreeInstance(context, targetContainer, null, targetShapeTree, "task-48", true, getTaskFortyEightBodyGraph(), TEXT_TURTLE);
        Assertions.assertEquals(201, response.getStatusCode());

    }

    @SneakyThrows
    @Test
    @Label("Create Second Task in Project Without Target Shape Tree or Focus Node")
    void createThirdTaskInProjectWithoutAnyContext() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Add fixtures for /data/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container"), "GET", "/data/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-locator"), "GET", "/data/.shapetree", null));
        // Add fixtures for /projects/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container"), "GET", "/data/projects/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container-locator-two-locations"), "GET", "/data/projects/.shapetree", null));
        // Add fixture for /projects/project-1/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-no-contains"), "GET", "/data/projects/project-1/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-locator"), "GET", "/data/projects/project-1/.shapetree", null));
        // Add fixture for /projects/project-1/milestone-3/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/milestone-3-container-no-contains"), "GET", "/data/projects/project-1/milestone-3/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/milestone-3-container-locator"), "GET", "/data/projects/project-1/milestone-3/.shapetree", null));
        // Add fixture for /projects/project-1/milestone-3/task-6/ to handle response to create via POST
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/task-48-container-no-contains"), "POST", "/data/projects/project-1/milestone-3/task-48/", null));

        URI targetContainer = getURI(server, "/data/projects/project-1/milestone-3/");

        // create task-48 in milestone-3 - don't supply a target shape tree or focus node
        DocumentResponse response = shapeTreeClient.postShapeTreeInstance(context, targetContainer, null, null, "task-48", true, getTaskFortyEightBodyGraph(), TEXT_TURTLE);
        Assertions.assertEquals(201, response.getStatusCode());

    }

    @SneakyThrows
    @Test
    @Label("Create Second Task in Project With Focus Node Without Target Shape Tree")
    void createSecondTaskInProjectWithFocusNodeWithoutTargetShapeTree() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Add fixtures for /data/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container"), "GET", "/data/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-locator"), "GET", "/data/.shapetree", null));
        // Add fixtures for /projects/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container"), "GET", "/data/projects/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container-locator-two-locations"), "GET", "/data/projects/.shapetree", null));
        // Add fixture for /projects/project-1/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-no-contains"), "GET", "/data/projects/project-1/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-locator"), "GET", "/data/projects/project-1/.shapetree", null));
        // Add fixture for /projects/project-1/milestone-3/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/milestone-3-container-no-contains"), "GET", "/data/projects/project-1/milestone-3/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/milestone-3-container-locator"), "GET", "/data/projects/project-1/milestone-3/.shapetree", null));
        // Add fixture for /projects/project-1/milestone-3/task-6/ to handle response to create via POST
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/task-48-container-no-contains"), "POST", "/data/projects/project-1/milestone-3/task-48/", null));

        URI targetContainer = getURI(server, "/data/projects/project-1/milestone-3/");
        URI focusNode = getURI(server, "/data/projects/project-1/milestone-3/task-48/#task");

        // create task-48 in milestone-3 - supply a focus node but no target shape tree
        DocumentResponse response = shapeTreeClient.postShapeTreeInstance(context, targetContainer, focusNode, null, "task-48", true, getTaskFortyEightBodyGraph(), TEXT_TURTLE);
        Assertions.assertEquals(201, response.getStatusCode());

    }

    @SneakyThrows
    @Test
    @Label("Create Attachment in Task")
    void createAttachmentInTask() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // create an attachment in task-48 (success)
        // Add fixtures for /data/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container"), "GET", "/data/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-locator"), "GET", "/data/.shapetree", null));
        // Add fixtures for /projects/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container"), "GET", "/data/projects/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container-locator-two-locations"), "GET", "/data/projects/.shapetree", null));
        // Add fixture for /projects/project-1/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-no-contains"), "GET", "/data/projects/project-1/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-locator"), "GET", "/data/projects/project-1/.shapetree", null));
        // Add fixture for /projects/project-1/milestone-3/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/milestone-3-container-no-contains"), "GET", "/data/projects/project-1/milestone-3/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/milestone-3-container-locator"), "GET", "/data/projects/project-1/milestone-3/.shapetree", null));
        // Add fixture for /projects/project-1/milestone-3/task-6/ to handle response to create via POST
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/task-48-container-no-contains"), "GET", "/data/projects/project-1/milestone-3/task-48/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/task-48-container-locator"), "GET", "/data/projects/project-1/milestone-3/task-48/.shapetree", null));
        // Add fixture to handle PUT response and follow-up request
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/attachment-48"), "PUT", "/data/projects/project-1/milestone-3/task-48/attachment-48", null));

        URI targetResource = getURI(server, "/data/projects/project-1/milestone-3/task-48/attachment-48");
        URI targetShapeTree = getURI(server, "/static/shapetrees/project/shapetree#AttachmentTree");

        DocumentResponse response = shapeTreeClient.putShapeTreeInstance(context, targetResource, null, targetShapeTree, false, null, "application/octet-stream");
        Assertions.assertEquals(201, response.getStatusCode());

    }

    @SneakyThrows
    @Test
    @Label("Create Second Attachment in Task")
    void createSecondAttachmentInTask() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // create an attachment in task-48 (success)
        // Add fixtures for /data/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container"), "GET", "/data/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-locator"), "GET", "/data/.shapetree", null));
        // Add fixtures for /projects/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container"), "GET", "/data/projects/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container-locator-two-locations"), "GET", "/data/projects/.shapetree", null));
        // Add fixture for /projects/project-1/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-no-contains"), "GET", "/data/projects/project-1/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-locator"), "GET", "/data/projects/project-1/.shapetree", null));
        // Add fixture for /projects/project-1/milestone-3/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/milestone-3-container-no-contains"), "GET", "/data/projects/project-1/milestone-3/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/milestone-3-container-locator"), "GET", "/data/projects/project-1/milestone-3/.shapetree", null));
        // Add fixture for /projects/project-1/milestone-3/task-6/ to handle response to create via POST
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/task-48-container-no-contains"), "GET", "/data/projects/project-1/milestone-3/task-48/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/task-48-container-locator"), "GET", "/data/projects/project-1/milestone-3/task-48/.shapetree", null));
        // Add fixture to handle PUT response and follow-up request
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/random-png"), "PUT", "/data/projects/project-1/milestone-3/task-48/random.png", null));

        URI targetResource = getURI(server, "/data/projects/project-1/milestone-3/task-48/random.png");
        URI targetShapeTree = getURI(server, "/static/shapetrees/project/shapetree#AttachmentTree");

        DocumentResponse response = shapeTreeClient.putShapeTreeInstance(context, targetResource, null, targetShapeTree, false, null, "application/octet-stream");
        Assertions.assertEquals(201, response.getStatusCode());

    }

    @SneakyThrows
    @Test
    @Label("Fail to Unplant Non-Root Task")
    void failToUnplantNonRootTask() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Add fixture for /data/projects/project-1/milestone-3/, which is not the root of the project hierarchy according to its locator
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/milestone-3-container"), "GET", "/data/projects/project-1/milestone-3/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/milestone-3-container-locator"), "GET", "/data/projects/project-1/milestone-3/.shapetree", null));

        URI targetResource = getURI(server, "/data/projects/project-1/milestone-3/");
        URI targetShapeTreeOne = getURI(server, "/static/shapetrees/project/shapetree#MilestoneTree");
        URI targetShapeTreeTwo = getURI(server, "/static/shapetrees/project/shapetree#ProjectsTree");

        // Try first by providing the Milestone Shape Tree as the unplant target
        DocumentResponse responseOne = shapeTreeClient.unplantShapeTree(context, targetResource, targetShapeTreeOne);
        Assertions.assertEquals(500, responseOne.getStatusCode());

        // Try again by providing the (incorrect) Project Shape Tree as the unplant target (which is the shape tree at the root of the hierarchy) - this will be caught by the client immediately
        Assertions.assertThrows(IllegalStateException.class, () -> {
            DocumentResponse responseTwo = shapeTreeClient.unplantShapeTree(context, targetResource, targetShapeTreeTwo);
        });

    }

    @SneakyThrows
    @Test
    @Label("Unplant Projects")
    void unplantProjects() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Unplant the project collection, recursing down the tree (success)
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container"), "GET", "/data/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-locator"), "GET", "/data/.shapetree", null));
        // Add fixtures for /projects/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container"), "GET", "/data/projects/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container-locator-two-locations"), "GET", "/data/projects/.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/204"), "DELETE", "/data/projects/.shapetree", null));
        // Add fixture for /projects/project-1/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container"), "GET", "/data/projects/project-1/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-locator"), "GET", "/data/projects/project-1/.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/204"), "DELETE", "/data/projects/project-1/.shapetree", null));
        // Add fixture for /projects/project-1/milestone-3/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/milestone-3-container"), "GET", "/data/projects/project-1/milestone-3/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/milestone-3-container-locator"), "GET", "/data/projects/project-1/milestone-3/.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/204"), "DELETE", "/data/projects/project-1/milestone-3/.shapetree", null));
        // Add fixtures for tasks in /projects/project-1/milestone-3/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/task-6-container-no-contains"), "GET", "/data/projects/project-1/milestone-3/task-6/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/task-6-container-locator"), "GET", "/data/projects/project-1/milestone-3/task-6/.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/204"), "DELETE", "/data/projects/project-1/milestone-3/task-6/.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/task-48-container"), "GET", "/data/projects/project-1/milestone-3/task-48/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/task-48-container-locator"), "GET", "/data/projects/project-1/milestone-3/task-48/.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/204"), "DELETE", "/data/projects/project-1/milestone-3/task-48/.shapetree", null));
        // Add fixtures for attachments in task-48
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/random-png"), "GET", "/data/projects/project-1/milestone-3/task-48/random.png", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/random-png-locator"), "GET", "/data/projects/project-1/milestone-3/task-48/random.png.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/204"), "DELETE", "/data/projects/project-1/milestone-3/task-48/random.png.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/attachment-48"), "GET", "/data/projects/project-1/milestone-3/task-48/attachment-48", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/attachment-48-locator"), "GET", "/data/projects/project-1/milestone-3/task-48/attachment-48.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/204"), "DELETE", "/data/projects/project-1/milestone-3/task-48/attachment-48.shapetree", null));
        // Add fixtures for issues in /projects/project-1/milestone-3/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/issue-2"), "GET", "/data/projects/project-1/milestone-3/issue-2", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/issue-2-locator"), "GET", "/data/projects/project-1/milestone-3/issue-2.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/204"), "DELETE", "/data/projects/project-1/milestone-3/issue-2.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/issue-3"), "GET", "/data/projects/project-1/milestone-3/issue-3", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/issue-3-locator"), "GET", "/data/projects/project-1/milestone-3/issue-3.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/204"), "DELETE", "/data/projects/project-1/milestone-3/issue-3.shapetree", null));

        URI targetResource = getURI(server, "/data/projects/");
        URI targetShapeTree = getURI(server, "/static/shapetrees/project/shapetree#ProjectCollectionTree");

        DocumentResponse response = shapeTreeClient.unplantShapeTree(context, targetResource, targetShapeTree);
        Assertions.assertEquals(201, response.getStatusCode());

    }

    @SneakyThrows
    @Test
    @Label("Unplant Data Set")
    void unplantData() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Unplant the data collection, recursing down the tree (success). The root level (pre-loaded) and one level below projects included for completeness
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container"), "GET", "/data/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-locator"), "GET", "/data/.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/204"), "DELETE", "/data/.shapetree", null));
        // Add fixtures for /projects/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container"), "GET", "/data/projects/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container-locator-two-locations"), "GET", "/data/projects/.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/204"), "PUT", "/data/projects/.shapetree", null));
        // Add fixture for /projects/project-1/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container"), "GET", "/data/projects/project-1/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-locator"), "GET", "/data/projects/project-1/.shapetree", null));

        URI targetResource = getURI(server, "/data/");
        URI targetShapeTree = getURI(server, "/static/shapetrees/project/shapetree#DataRepositoryTree");

        // Unplant the data collection, recursing down the tree (only two levels)
        // Since the projects collection still manages /data/projects/, it should not delete the locator, only update it
        DocumentResponse response = shapeTreeClient.unplantShapeTree(context, targetResource, targetShapeTree);
        Assertions.assertEquals(201, response.getStatusCode());

    }

    @SneakyThrows
    @Test
    @Label("Plant Data Repository with Patch")
    void plantDataRepositoryWithPatch() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Create the data container
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-no-contains"), "GET", "/data/", null));

        URI targetResource = getURI(server, "/data/.shapetree");

        // Plant the data repository on newly created data container
        DocumentResponse response = shapeTreeClient.patchShapeTreeInstance(context, targetResource, null, getPlantDataRepositorySparqlPatch(server));
        Assertions.assertEquals(201, response.getStatusCode());

    }

    @SneakyThrows
    @Test
    @Label("Update Project Collection Locator with Patch")
    void updateProjectsLocatorWithPatch() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Add fixtures for data repository container and locator
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container"), "GET", "/data/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-locator"), "GET", "/data/.shapetree", null));
        // Add fixtures for /projects/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container-no-contains"), "GET", "/data/projects/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container-locator"), "GET", "/data/projects/.shapetree", null));

        URI targetResource = getURI(server, "/data/projects/.shapetree");

        // Update the locator directly for the /data/projects/ with PATCH
        DocumentResponse response = shapeTreeClient.patchShapeTreeInstance(context, targetResource, null, getUpdateDataRepositorySparqlPatch(server));
        Assertions.assertEquals(201, response.getStatusCode());

    }

    private String getProjectsBodyGraph() {
        return "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \n" +
                "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \n" +
                "PREFIX xml: <http://www.w3.org/XML/1998/namespace> \n" +
                "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> \n" +
                "PREFIX ex: <http://www.example.com/ns/ex#> \n" +
                "\n" +
                "\n" +
                "<#collection> \n" +
                "    ex:uri </data/projects/#collection> ; \n" +
                "    ex:id 32 ; \n" +
                "    ex:name \"Projects Data Collection \" ; \n" +
                "    ex:created_at \"2021-04-04T20:15:47.000Z\"^^xsd:dateTime . \n";
    }

    private String getProjectOneBodyGraph() {
        return "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \n" +
                "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \n" +
                "PREFIX xml: <http://www.w3.org/XML/1998/namespace> \n" +
                "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> \n" +
                "PREFIX ex: <http://www.example.com/ns/ex#> \n" +
                "\n" +
                "\n" +
                "<#project> \n" +
                "    ex:uri </data/projects/project-1/#project> ; \n" +
                "    ex:id 6 ; \n" +
                "    ex:name \"Great Validations \" ; \n" +
                "    ex:created_at \"2021-04-04T20:15:47.000Z\"^^xsd:dateTime ; \n" +
                "    ex:hasMilestone </data/projects/project-1/milestone-3/#milestone> . ";
    }

    private String getProjectOneUpdatedBodyGraph() {
        return "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \n" +
                "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \n" +
                "PREFIX xml: <http://www.w3.org/XML/1998/namespace> \n" +
                "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> \n" +
                "PREFIX ex: <http://www.example.com/ns/ex#> \n" +
                "\n" +
                "\n" +
                "<#project> \n" +
                "    ex:uri </data/projects/project-1/#project> ; \n" +
                "    ex:id 12 ; \n" +
                "    ex:name \"Even Greater Validations For Everyone!\" ; \n" +
                "    ex:created_at \"2021-04-04T20:15:47.000Z\"^^xsd:dateTime ; \n" +
                "    ex:hasMilestone </data/projects/project-1/milestone-3/#milestone> . ";
    }

    private String getProjectOneMalformedBodyGraph() {
        return "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \n" +
                "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \n" +
                "PREFIX xml: <http://www.w3.org/XML/1998/namespace> \n" +
                "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> \n" +
                "PREFIX ex: <http://www.example.com/ns/ex#> \n" +
                "\n" +
                "\n" +
                "<#project> \n" +
                "    ex:uri </data/projects/project-1/#project> ; \n" +
                "    ex:name 5 ; \n" +
                "    ex:created_at \"2021-04-04T20:15:47.000Z\"^^xsd:dateTime ; \n" +
                "    ex:hasMilestone </data/projects/project-1/milestone-3/#milestone> . ";
    }

    private String getMilestoneThreeBodyGraph() {
        return "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \n" +
                "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \n" +
                "PREFIX xml: <http://www.w3.org/XML/1998/namespace> \n" +
                "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> \n" +
                "PREFIX ex: <http://www.example.com/ns/ex#> \n" +
                "\n" +
                "\n" +
                "<#milestone> \n" +
                "    ex:uri </data/projects/project-1/milestone-3/#milestone> ; \n" +
                "    ex:id 12345 ; \n" +
                "    ex:name \"Milestone 3 of Project 1\" ; \n" +
                "    ex:created_at \"2021-04-04T20:15:47.000Z\"^^xsd:dateTime ; \n" +
                "    ex:target \"2021-06-05T20:15:47.000Z\"^^xsd:dateTime ; \n" +
                "    ex:inProject </data/projects/project-1/#project> . \n" ;
    }

    private String getMilestoneThreeSparqlPatch() {
        return "PREFIX ex: <http://www.example.com/ns/ex#> \n" +
                "DELETE { ?milestone ex:id 12345 } \n" +
                "INSERT { ?milestone ex:id 54321 } \n" +
                "WHERE { ?milestone ex:uri </data/projects/project-1/milestone-3/#milestone> } \n";
    }

    private String getTaskSixSparqlPatch() {
        return "PREFIX ex: <http://www.example.com/ns/ex#> \n" +
               "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> \n" +
               "INSERT DATA { \n" +
               "    <#task> ex:uri </data/projects/project-1/milestone-3/task-6#task> . \n" +
               "    <#task> ex:id 6 . \n" +
               "    <#task> ex:name \"Somewhat urgent but not critical task\" . \n" +
               "    <#task> ex:description \"Not particularly worried about this but it should get done\" . \n" +
               "    <#task> ex:created_at \"2021-04-04T20:15:47.000Z\"^^xsd:dateTime . \n" +
               "} \n" ;
    }

    private String getTaskFortyEightBodyGraph() {
        return "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \n" +
                "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \n" +
                "PREFIX xml: <http://www.w3.org/XML/1998/namespace> \n" +
                "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> \n" +
                "PREFIX ex: <http://www.example.com/ns/ex#> \n" +
                "\n" +
                "\n" +
                "<#task> \n" +
                "    ex:uri </data/projects/project-1/milestone-3/task-48/#task> ; \n" +
                "    ex:id 2 ; \n" +
                "    ex:name \"Some Development Task\" ; \n" +
                "    ex:description \"Something extremely important that must be done!\" ; \n" +
                "    ex:created_at \"2021-04-04T20:15:47.000Z\"^^xsd:dateTime . \n" ;
    }

    private String getPlantDataRepositorySparqlPatch(MockWebServer server) throws URISyntaxException {
        return  "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \n" +
                "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \n" +
                "PREFIX st: <http://www.w3.org/ns/shapetrees#> \n" +
                "PREFIX ex: <http://www.example.com/ns/ex#> \n" +
                "INSERT DATA { \n" +
                "   <>  st:hasShapeTreeLocator <#locator> . \n" +
                "   <#locator> a st:ShapeTreeLocator . \n" +
                "   <#locator> st:location <#ln1> . \n" +
                "   <#ln1> st:hasShapeTree <" + getURI(server, "/static/shapetrees/project/shapetree#DataRepositoryTree") + "> . \n" +
                "   <#ln1> st:hasManagedResource </data/> . \n" +
                "   <#ln1> st:hasRootShapeTreeLocation </data/.shapetree#ln1> . \n" +
                "   <#ln1> st:node </data/#repository> . \n" +
                "   <#ln1> st:shape <" + getURI(server, "/static/shex/project/shex#DataRepositoryShape") + "> . \n" +
                "} \n" ;
    }

    private String getUpdateDataRepositorySparqlPatch(MockWebServer server) throws URISyntaxException {
        return  "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \n" +
                "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \n" +
                "PREFIX st: <http://www.w3.org/ns/shapetrees#> \n" +
                "PREFIX ex: <http://www.example.com/ns/ex#> \n" +
                "INSERT DATA { \n" +
                "   <#locator> st:location <#ln2> . \n" +
                "   <#ln2> st:hasShapeTree <" + getURI(server, "/static/shapetrees/project/shapetree#ProjectCollectionTree") + "> . \n" +
                "   <#ln2> st:hasManagedResource </data/projects/> . \n" +
                "   <#ln2> st:hasRootShapeTreeLocation </data/projects/.shapetree#ln2> . \n" +
                "   <#ln2> st:node </data/projects/#collection> . \n" +
                "   <#ln2> st:shape <" + getURI(server, "/static/shex/project/shex#ProjectCollectionShape") + "> . \n" +
                "} \n" ;
    }
}
