package com.janeirodigital.shapetrees.okhttp;

import com.janeirodigital.shapetrees.core.ShapeTreeContext;
import com.janeirodigital.shapetrees.core.ShapeTreeManager;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.tests.fixtures.DispatcherEntry;
import com.janeirodigital.shapetrees.tests.fixtures.RequestMatchingFixtureDispatcher;
import okhttp3.OkHttpClient;
import okhttp3.Response;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.jupiter.api.*;

import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static com.janeirodigital.shapetrees.core.enums.ContentType.OCTET_STREAM;
import static com.janeirodigital.shapetrees.core.enums.ContentType.TEXT_TURTLE;
import static com.janeirodigital.shapetrees.okhttp.OkHttpShapeTreeClient.*;
import static com.janeirodigital.shapetrees.tests.fixtures.MockWebServerHelper.toUrl;
import static org.junit.jupiter.api.Assertions.*;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class OkHttpShapeTreeClientProjectTests {

    private static RequestMatchingFixtureDispatcher dispatcher;
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

        dispatcherList.add(new DispatcherEntry(List.of("project/root-container"), "GET", "/", null));
        dispatcherList.add(new DispatcherEntry(List.of("project/root-container-manager"), "GET", "/.shapetree", null));
        dispatcherList.add(new DispatcherEntry(List.of("shapetrees/project-shapetree-ttl"), "GET", "/static/shapetrees/project/shapetree", null));
        dispatcherList.add(new DispatcherEntry(List.of("shapetrees/information-shapetree-ttl"), "GET", "/static/shapetrees/information/shapetree", null));
        dispatcherList.add(new DispatcherEntry(List.of("schemas/project-shex"), "GET", "/static/shex/project/shex", null));
        dispatcherList.add(new DispatcherEntry(List.of("schemas/information-shex"), "GET", "/static/shex/information/shex", null));

        server = new MockWebServer();
        dispatcher = new RequestMatchingFixtureDispatcher(dispatcherList);
        server.setDispatcher(dispatcher);
    }

    @Test
    @DisplayName("Discover unmanaged root resource")
    void discoverUnmanagedRoot() throws ShapeTreeException {
        // Use the discover operation to see if the root container is managed
        // The root container isn't managed so check to ensure that a NULL value is returned
        URL targetResource = toUrl(server,"/");
        ShapeTreeManager manager = discover(okHttpClient, context, targetResource);
        assertNull(manager);
    }

    @Test
    @DisplayName("Fail to plant on a non-existent data container")
    void failPlantOnMissingDataContainer() throws ShapeTreeException {
        // Perform plant on /data container that doesn't exist yet (fails)
        // Look for 404 because /data doesn't exist
        URL targetResource = toUrl(server,"/data/");
        URL targetShapeTree = toUrl(server, "/static/shapetrees/project/shapetree#DataRepositoryTree");
        Response response = plant(okHttpClient, context, targetResource, targetShapeTree, null);
        assertEquals(404, response.code());
    }

    @Test
    @DisplayName("Plant Data Repository")
    void plantDataRepository() throws ShapeTreeException {
        // Create the data container
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-no-contains"), "GET", "/data/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/data/.shapetree", null));

        URL targetResource = toUrl(server, "/data/");
        URL targetShapeTree = toUrl(server, "/static/shapetrees/project/shapetree#DataRepositoryTree");
        URL focusNode = toUrl(server, "/data/#repository");

        // Plant the data repository on newly created data container
        Response response = plant(okHttpClient, context, targetResource, targetShapeTree, focusNode);
        assertEquals(201, response.code());

    }

    @Test
    @DisplayName("Fail to plant on missing shape tree")
    void failPlantOnMissingShapeTree() {
        // Create the data container
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-no-contains"), "GET", "/data/", null));

        URL targetResource = toUrl(server, "/data/");
        URL targetShapeTree = toUrl(server, "/static/shapetrees/missing/shapetree#NonExistentTree");
        URL focusNode = toUrl(server, "/data/#repository");

        // Plant will fail and throw an exception when the shape tree to plant cannot be looked up
        assertThrows(ShapeTreeException.class, () -> { plant(okHttpClient, context, targetResource, targetShapeTree, focusNode); });
    }

    @Test
    @DisplayName("Create Projects Container and Validate DataCollectionTree and InformationSetTree")
    void createAndValidateProjectsWithMultipleContains() throws ShapeTreeException {
        // Setup initial fixtures for /data/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container"), "GET", "/data/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-multiplecontains-manager"), "GET", "/data/.shapetree", null));
        // Add fixture for /projects/ to handle the POST response
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container-create-response"), "POST", "/data/projects/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/data/projects/.shapetree", null));

        URL parentContainer = toUrl(server, "/data/");
        List<URL> focusNodes = Arrays.asList(toUrl(server, "/data/projects/#collection"));
        List<URL> targetShapeTrees = Arrays.asList(toUrl(server, "/static/shapetrees/project/shapetree#DataCollectionTree"),
                toUrl(server, "/static/shapetrees/information/shapetree#InformationSetTree"));

        // Create the projects container as a managed instance.
        // 1. Will be validated by the parent DataRepositoryTree and the InformationSetTree both planted on /data (multiple contains)
        // 2. Will have a manager/assignment created for it as an instance of DataCollectionTree and InformationSetTree
        Response response = post(okHttpClient, context, parentContainer, focusNodes, targetShapeTrees, "projects", true, getProjectsBodyGraph(), TEXT_TURTLE);
        assertEquals(201, response.code());

        // Another attempt without any target shape trees
        response = post(okHttpClient, context, parentContainer, focusNodes, null, "projects", true, getProjectsBodyGraph(), TEXT_TURTLE);
        assertEquals(201, response.code());

        // Another attempt without any target focus nodes
        response = post(okHttpClient, context, parentContainer, null, targetShapeTrees, "projects", true, getProjectsBodyGraph(), TEXT_TURTLE);
        assertEquals(201, response.code());

        // Another attempt without any only one of two target shape trees
        response = post(okHttpClient, context, parentContainer, null, targetShapeTrees, "projects", true, getProjectsBodyGraph(), TEXT_TURTLE);
        assertEquals(201, response.code());
    }

    @Test
    @DisplayName("Create Projects Container and Validate DataCollectionTree")
    void createAndValidateProjects() throws ShapeTreeException {
        // Setup initial fixtures for /data/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container"), "GET", "/data/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-manager"), "GET", "/data/.shapetree", null));
        // Add fixture for /projects/ to handle the POST response
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container-create-response"), "POST", "/data/projects/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/data/projects/.shapetree", null));

        URL parentContainer = toUrl(server, "/data/");
        List<URL> focusNodes = Arrays.asList(toUrl(server, "/data/projects/#collection"));
        List<URL> targetShapeTrees = Arrays.asList(toUrl(server, "/static/shapetrees/project/shapetree#DataCollectionTree"));

        // Create the projects container as a shape tree instance.
        // 1. Will be validated by the parent DataRepositoryTree planted on /data
        // 2. Will have a manager/assignment created for it as an instance of DataCollectionTree
        Response response = post(okHttpClient, context, parentContainer, focusNodes, targetShapeTrees, "projects", true, getProjectsBodyGraph(), TEXT_TURTLE);
        assertEquals(201, response.code());
    }

    @Test
    @DisplayName("Plant ProjectCollectionTree on Projects Container")
    void plantSecondShapeTreeOnProjects() throws ShapeTreeException {
        // Add fixtures for /data/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container"), "GET", "/data/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-manager"), "GET", "/data/.shapetree", null));
        // Add fixtures for /projects/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container-no-contains"), "GET", "/data/projects/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container-manager"), "GET", "/data/projects/.shapetree", null));

        URL targetResource = toUrl(server, "/data/projects/");
        URL targetShapeTree = toUrl(server, "/static/shapetrees/project/shapetree#ProjectCollectionTree");

        // Plant the second shape tree (ProjectCollectionTree) on /data/projects/
        Response response = plant(okHttpClient, context, targetResource, targetShapeTree, null);
        assertEquals(201, response.code());
    }

    @Test
    @DisplayName("Create Project in the Projects Collection")
    void createProjectInProjects() throws ShapeTreeException {
        // Add fixtures for /data/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container"), "GET", "/data/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-manager"), "GET", "/data/.shapetree", null));
        // Add fixtures for /projects/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container-no-contains"), "GET", "/data/projects/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container-manager-two-assignments"), "GET", "/data/projects/.shapetree", null));
        // Add fixture for /projects/project-1/ to handle the POST response
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-create-response"), "POST", "/data/projects/project-1/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/data/projects/project-1/.shapetree", null));

        URL parentContainer = toUrl(server, "/data/projects/");
        List<URL> focusNodes = Arrays.asList(toUrl(server, "/data/projects/project-1/#project"));
        List<URL> targetShapeTrees = Arrays.asList(toUrl(server, "/static/shapetrees/project/shapetree#ProjectTree"));

        // Create the project-1 container as a shape tree instance.
        // 1. Will be validated by the parent ProjectCollectionTree planted on /data/projects/
        // 2. Will have a manager/assignment created for it as an instance of ProjectTree
        Response response = post(okHttpClient, context, parentContainer, focusNodes, targetShapeTrees, "project-1", true, getProjectOneBodyGraph(), TEXT_TURTLE);
        assertEquals(201, response.code());
    }

    @Test
    @DisplayName("Update Project in the Projects Collection")
    void updateProjectInProjects() throws ShapeTreeException {
        // Add fixtures for /data/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container"), "GET", "/data/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-manager"), "GET", "/data/.shapetree", null));
        // Add fixtures for /projects/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container"), "GET", "/data/projects/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container-manager-two-assignments"), "GET", "/data/projects/.shapetree", null));
        // Add fixture for /projects/project-1/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-no-contains"), "GET", "/data/projects/project-1/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-manager"), "GET", "/data/projects/project-1/.shapetree", null));
        // Add fixture for updated project-1
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-no-contains-updated"), "PUT", "/data/projects/project-1/", null));

        URL targetResource = toUrl(server, "/data/projects/project-1/");
        List<URL> focusNodes = Arrays.asList(toUrl(server, "/data/projects/project-1/#project"));

        // Update the project-1 container as a shape tree instance.
        // 1. Will be validated by the parent ProjectCollectionTree planted on /data/projects/
        // 2. Will have a manager/assignment created for it as an instance of ProjectTree
        Response response = put(okHttpClient, context, targetResource, focusNodes, getProjectOneUpdatedBodyGraph(), TEXT_TURTLE);
        assertEquals(200, response.code());
    }

    @Test
    @DisplayName("Fail to Create a Malformed Project in the Projects Collection")
    void failToCreateMalformedProject() throws ShapeTreeException {
        // Add fixtures for /data/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container"), "GET", "/data/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-manager"), "GET", "/data/.shapetree", null));
        // Add fixtures for /projects/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container"), "GET", "/data/projects/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container-manager-two-assignments"), "GET", "/data/projects/.shapetree", null));

        URL targetResource = toUrl(server, "/data/projects/project-1/");
        List<URL> focusNodes = Arrays.asList(toUrl(server, "/data/projects/project-1/#project"));
        List<URL> targetShapeTrees = Arrays.asList(toUrl(server, "/static/shapetrees/project/shapetree#ProjectTree"));

        // Create the project-1 container as a shape tree instance via PUT
        // 1. Will be validated by the parent ProjectCollectionTree planted on /data/projects/
        Response response = put(okHttpClient, context, targetResource, focusNodes, targetShapeTrees, true, getProjectOneMalformedBodyGraph(), TEXT_TURTLE);
        // 2. Will fail validation because the body content doesn't validate against the assigned shape
        assertEquals(422, response.code());
    }

    @Test
    @DisplayName("Fail to Update a Project to be Malformed in the Projects Collection")
    void failToUpdateMalformedProject() throws ShapeTreeException {
        // try to update an existing project-1 to be malformed and fail validation
        // Add fixtures for /data/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container"), "GET", "/data/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-manager"), "GET", "/data/.shapetree", null));
        // Add fixtures for /projects/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container"), "GET", "/data/projects/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container-manager-two-assignments"), "GET", "/data/projects/.shapetree", null));
        // Add fixture for /projects/project-1/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-no-contains"), "GET", "/data/projects/project-1/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-manager"), "GET", "/data/projects/project-1/.shapetree", null));

        URL targetResource = toUrl(server, "/data/projects/project-1/");
        List<URL> focusNodes = Arrays.asList(toUrl(server, "/data/projects/project-1/#project"));

        // Update the project-1 container as a shape tree instance via PUT
        // 1. Will be validated by the parent ProjectCollectionTree planted on /data/projects/
        Response response = put(okHttpClient, context, targetResource, focusNodes, getProjectOneMalformedBodyGraph(), TEXT_TURTLE);
        // 2. Will fail validation because the body content doesn't validate against the assigned shape
        assertEquals(422, response.code());

    }

    @Test
    @DisplayName("Create Milestone in Project With Put")
    void createMilestoneInProjectWithPut() throws ShapeTreeException {
        // Add fixtures for /data/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container"), "GET", "/data/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-manager"), "GET", "/data/.shapetree", null));
        // Add fixtures for /projects/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container"), "GET", "/data/projects/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container-manager-two-assignments"), "GET", "/data/projects/.shapetree", null));
        // Add fixture for /projects/project-1/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-no-contains"), "GET", "/data/projects/project-1/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-manager"), "GET", "/data/projects/project-1/.shapetree", null));
        // Add fixture for /projects/project-1/milestone-3 to handle response to create via PUT
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/milestone-3-container-no-contains"), "PUT", "/data/projects/project-1/milestone-3/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/data/projects/project-1/milestone-3/.shapetree", null));

        URL targetResource = toUrl(server, "/data/projects/project-1/milestone-3/");
        List<URL> focusNodes = Arrays.asList(toUrl(server, "/data/projects/project-1/milestone-3/#milestone"));
        List<URL> targetShapeTrees = Arrays.asList(toUrl(server, "/static/shapetrees/project/shapetree#MilestoneTree"));

        // Create the milestone-3 container in /projects/project-1/ as a shape tree instance using PUT to create
        // 1. Will be validated by the parent ProjectTree planted on /data/projects/project-1/
        // 2. Will have a manager/assignment created for it as an instance of MilestoneTree
        Response response = put(okHttpClient, context, targetResource, focusNodes, targetShapeTrees, true, getMilestoneThreeBodyGraph(), TEXT_TURTLE);
        assertEquals(201, response.code());
    }

    @Test
    @DisplayName("Update Milestone in Project With Patch")
    void updateMilestoneInProjectWithPatch() throws ShapeTreeException {
        // Add fixtures for /data/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container"), "GET", "/data/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-manager"), "GET", "/data/.shapetree", null));
        // Add fixtures for /projects/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container"), "GET", "/data/projects/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container-manager-two-assignments"), "GET", "/data/projects/.shapetree", null));
        // Add fixture for /projects/project-1/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-no-contains"), "GET", "/data/projects/project-1/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-manager"), "GET", "/data/projects/project-1/.shapetree", null));
        // Add fixture for /projects/project-1/milestone-3/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/milestone-3-container-no-contains"), "GET", "/data/projects/project-1/milestone-3/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/milestone-3-container-manager"), "GET", "/data/projects/project-1/milestone-3/.shapetree", null));
        // Add fixture for /projects/project-1/milestone-3 to handle response to update via PATCH
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/milestone-3-container-no-contains-updated"), "PATCH", "/data/projects/project-1/milestone-3/", null));

        URL targetResource = toUrl(server, "/data/projects/project-1/milestone-3/");
        List<URL> focusNodes = Arrays.asList(toUrl(server, "/data/projects/project-1/milestone-3/#milestone"));

        // Update the milestone-3 container in /projects/project-1/ using PATCH
        // 1. Will be validated by the MilestoneTree planted on /data/projects/project-1/milestone-3/
        Response response = patch(okHttpClient, context, targetResource, focusNodes, getMilestoneThreeSparqlPatch());
        assertEquals(201, response.code());
    }

    @Test
    @DisplayName("Create First Task in Project With Patch")
    void createFirstTaskInProjectWithPatch() throws ShapeTreeException {
        // Add fixtures for /data/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container"), "GET", "/data/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-manager"), "GET", "/data/.shapetree", null));
        // Add fixtures for /projects/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container"), "GET", "/data/projects/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container-manager-two-assignments"), "GET", "/data/projects/.shapetree", null));
        // Add fixture for /projects/project-1/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-no-contains"), "GET", "/data/projects/project-1/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-manager"), "GET", "/data/projects/project-1/.shapetree", null));
        // Add fixture for /projects/project-1/milestone-3/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/milestone-3-container-no-contains"), "GET", "/data/projects/project-1/milestone-3/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/milestone-3-container-manager"), "GET", "/data/projects/project-1/milestone-3/.shapetree", null));
        // Add fixture for /projects/project-1/milestone-3/task-6/ to handle response to update via PATCH
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/task-6-container-no-contains-updated"), "PATCH", "/data/projects/project-1/milestone-3/task-6/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/data/projects/project-1/milestone-3/task-6/.shapetree", null));

        URL targetResource = toUrl(server, "/data/projects/project-1/milestone-3/task-6/");
        List<URL> focusNodes = Arrays.asList(toUrl(server, "/data/projects/project-1/milestone-3/task-6/#task"));

        // Create the task-6 container in /projects/project-1/milestone-3/ using PATCH
        // 1. Will be validated by the parent MilestoneTree planted on /data/projects/project-1/milestone-3/
        Response response = patch(okHttpClient, context, targetResource, focusNodes, getTaskSixSparqlPatch());
        assertEquals(201, response.code());

    }

    @Test
    @DisplayName("Create Second Task in Project Without Focus Node")
    void createSecondTaskInProjectWithoutFocusNode() throws ShapeTreeException {
        // Add fixtures for /data/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container"), "GET", "/data/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-manager"), "GET", "/data/.shapetree", null));
        // Add fixtures for /projects/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container"), "GET", "/data/projects/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container-manager-two-assignments"), "GET", "/data/projects/.shapetree", null));
        // Add fixture for /projects/project-1/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-no-contains"), "GET", "/data/projects/project-1/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-manager"), "GET", "/data/projects/project-1/.shapetree", null));
        // Add fixture for /projects/project-1/milestone-3/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/milestone-3-container-no-contains"), "GET", "/data/projects/project-1/milestone-3/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/milestone-3-container-manager"), "GET", "/data/projects/project-1/milestone-3/.shapetree", null));
        // Add fixture for /projects/project-1/milestone-3/task-6/ to handle response to create via POST
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/task-48-create-response"), "POST", "/data/projects/project-1/milestone-3/task-48/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/data/projects/project-1/milestone-3/task-48/.shapetree", null));

        URL targetContainer = toUrl(server, "/data/projects/project-1/milestone-3/");
        List<URL> targetShapeTrees = Arrays.asList(toUrl(server, "/static/shapetrees/project/shapetree#TaskTree"));

        // create task-48 in milestone-3 - supply a target shape tree, but not a focus node
        Response response = post(okHttpClient, context, targetContainer, null, targetShapeTrees, "task-48", true, getTaskFortyEightBodyGraph(), TEXT_TURTLE);
        assertEquals(201, response.code());

    }

    @Test
    @DisplayName("Create Third Task in Project Without Target Shape Tree or Focus Node")
    void createThirdTaskInProjectWithoutAnyContext() throws ShapeTreeException {
        // Add fixtures for /data/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container"), "GET", "/data/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-manager"), "GET", "/data/.shapetree", null));
        // Add fixtures for /projects/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container"), "GET", "/data/projects/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container-manager-two-assignments"), "GET", "/data/projects/.shapetree", null));
        // Add fixture for /projects/project-1/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-no-contains"), "GET", "/data/projects/project-1/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-manager"), "GET", "/data/projects/project-1/.shapetree", null));
        // Add fixture for /projects/project-1/milestone-3/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/milestone-3-container-no-contains"), "GET", "/data/projects/project-1/milestone-3/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/milestone-3-container-manager"), "GET", "/data/projects/project-1/milestone-3/.shapetree", null));
        // Add fixture for /projects/project-1/milestone-3/task-6/ to handle response to create via POST
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/task-48-create-response"), "POST", "/data/projects/project-1/milestone-3/task-48/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/data/projects/project-1/milestone-3/task-48/.shapetree", null));

        URL targetContainer = toUrl(server, "/data/projects/project-1/milestone-3/");

        // create task-48 in milestone-3 - don't supply a target shape tree or focus node
        Response response = post(okHttpClient, context, targetContainer, null, null, "task-48", true, getTaskFortyEightBodyGraph(), TEXT_TURTLE);
        assertEquals(201, response.code());

    }

    @Test
    @DisplayName("Create Second Task in Project With Focus Node Without Target Shape Tree")
    void createSecondTaskInProjectWithFocusNodeWithoutTargetShapeTree() throws ShapeTreeException {
        // Add fixtures for /data/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container"), "GET", "/data/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-manager"), "GET", "/data/.shapetree", null));
        // Add fixtures for /projects/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container"), "GET", "/data/projects/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container-manager-two-assignments"), "GET", "/data/projects/.shapetree", null));
        // Add fixture for /projects/project-1/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-no-contains"), "GET", "/data/projects/project-1/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-manager"), "GET", "/data/projects/project-1/.shapetree", null));
        // Add fixture for /projects/project-1/milestone-3/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/milestone-3-container-no-contains"), "GET", "/data/projects/project-1/milestone-3/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/milestone-3-container-manager"), "GET", "/data/projects/project-1/milestone-3/.shapetree", null));
        // Add fixture for /projects/project-1/milestone-3/task-6/ to handle response to create via POST
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/task-48-create-response"), "POST", "/data/projects/project-1/milestone-3/task-48/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/data/projects/project-1/milestone-3/task-48/.shapetree", null));

        URL targetContainer = toUrl(server, "/data/projects/project-1/milestone-3/");
        List<URL> focusNodes = Arrays.asList(toUrl(server, "/data/projects/project-1/milestone-3/task-48/#task"));

        // create task-48 in milestone-3 - supply a focus node but no target shape tree
        Response response = post(okHttpClient, context, targetContainer, focusNodes, null, "task-48", true, getTaskFortyEightBodyGraph(), TEXT_TURTLE);
        assertEquals(201, response.code());

    }

    @Test
    @DisplayName("Create Attachment in Task")
    void createAttachmentInTask() throws ShapeTreeException {
        // create an attachment in task-48 (success)
        // Add fixtures for /data/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container"), "GET", "/data/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-manager"), "GET", "/data/.shapetree", null));
        // Add fixtures for /projects/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container"), "GET", "/data/projects/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container-manager-two-assignments"), "GET", "/data/projects/.shapetree", null));
        // Add fixture for /projects/project-1/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-no-contains"), "GET", "/data/projects/project-1/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-manager"), "GET", "/data/projects/project-1/.shapetree", null));
        // Add fixture for /projects/project-1/milestone-3/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/milestone-3-container-no-contains"), "GET", "/data/projects/project-1/milestone-3/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/milestone-3-container-manager"), "GET", "/data/projects/project-1/milestone-3/.shapetree", null));
        // Add fixture for /projects/project-1/milestone-3/task-6/ to handle response to create via POST
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/task-48-container-no-contains"), "GET", "/data/projects/project-1/milestone-3/task-48/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/task-48-container-manager"), "GET", "/data/projects/project-1/milestone-3/task-48/.shapetree", null));
        // Add fixture to handle PUT response and follow-up request
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/attachment-48"), "PUT", "/data/projects/project-1/milestone-3/task-48/attachment-48", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/data/projects/project-1/milestone-3/task-48/attachment-48.shapetree", null));

        URL targetResource = toUrl(server, "/data/projects/project-1/milestone-3/task-48/attachment-48");
        List<URL> targetShapeTrees = Arrays.asList(toUrl(server, "/static/shapetrees/project/shapetree#AttachmentTree"));

        Response response = put(okHttpClient, context, targetResource, null, targetShapeTrees, false, null, OCTET_STREAM);
        assertEquals(201, response.code());

    }

    @Test
    @DisplayName("Create Second Attachment in Task")
    void createSecondAttachmentInTask() throws ShapeTreeException {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // create an attachment in task-48 (success)
        // Add fixtures for /data/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container"), "GET", "/data/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-manager"), "GET", "/data/.shapetree", null));
        // Add fixtures for /projects/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container"), "GET", "/data/projects/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container-manager-two-assignments"), "GET", "/data/projects/.shapetree", null));
        // Add fixture for /projects/project-1/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-no-contains"), "GET", "/data/projects/project-1/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-manager"), "GET", "/data/projects/project-1/.shapetree", null));
        // Add fixture for /projects/project-1/milestone-3/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/milestone-3-container-no-contains"), "GET", "/data/projects/project-1/milestone-3/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/milestone-3-container-manager"), "GET", "/data/projects/project-1/milestone-3/.shapetree", null));
        // Add fixture for /projects/project-1/milestone-3/task-6/ to handle response to create via POST
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/task-48-container-no-contains"), "GET", "/data/projects/project-1/milestone-3/task-48/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/task-48-container-manager"), "GET", "/data/projects/project-1/milestone-3/task-48/.shapetree", null));
        // Add fixture to handle PUT response and follow-up request
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/random-png"), "PUT", "/data/projects/project-1/milestone-3/task-48/random.png", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/data/projects/project-1/milestone-3/task-48/random.png.shapetree", null));

        URL targetResource = toUrl(server, "/data/projects/project-1/milestone-3/task-48/random.png");
        List<URL> targetShapeTrees = Arrays.asList(toUrl(server, "/static/shapetrees/project/shapetree#AttachmentTree"));

        Response response = put(okHttpClient, context, targetResource, null, targetShapeTrees, false, null, OCTET_STREAM);
        assertEquals(201, response.code());

    }

    @Test
    @DisplayName("Fail to Unplant Non-Root Task")
    void failToUnplantNonRootTask() throws ShapeTreeException {
        // Add fixture for /data/projects/project-1/milestone-3/, which is not the root of the project hierarchy according to its manager
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/milestone-3-container"), "GET", "/data/projects/project-1/milestone-3/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/milestone-3-container-manager"), "GET", "/data/projects/project-1/milestone-3/.shapetree", null));

        URL targetResource = toUrl(server, "/data/projects/project-1/milestone-3/");
        URL targetShapeTreeOne = toUrl(server, "/static/shapetrees/project/shapetree#MilestoneTree");
        URL targetShapeTreeTwo = toUrl(server, "/static/shapetrees/project/shapetree#ProjectsTree");

        // Try first by providing the Milestone Shape Tree as the unplant target
        Response responseOne = unplant(okHttpClient, context, targetResource, targetShapeTreeOne);
        assertEquals(500, responseOne.code());

        // Try again by providing the (incorrect) Project Shape Tree as the unplant target (which is the shape tree at the root of the hierarchy) - this will be caught by the client immediately
        assertThrows(IllegalStateException.class, () -> { unplant(okHttpClient, context, targetResource, targetShapeTreeTwo); });
    }

    @Test
    @DisplayName("Unplant Projects")
    void unplantProjects() throws ShapeTreeException {
        // Unplant the project collection, recursing down the tree (success)
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container"), "GET", "/data/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-manager"), "GET", "/data/.shapetree", null));
        // Add fixtures for /projects/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container"), "GET", "/data/projects/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container-manager-two-assignments"), "GET", "/data/projects/.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/204"), "DELETE", "/data/projects/.shapetree", null));
        // Add fixture for /projects/project-1/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container"), "GET", "/data/projects/project-1/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-manager"), "GET", "/data/projects/project-1/.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/204"), "DELETE", "/data/projects/project-1/.shapetree", null));
        // Add fixture for /projects/project-1/milestone-3/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/milestone-3-container"), "GET", "/data/projects/project-1/milestone-3/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/milestone-3-container-manager"), "GET", "/data/projects/project-1/milestone-3/.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/204"), "DELETE", "/data/projects/project-1/milestone-3/.shapetree", null));
        // Add fixtures for tasks in /projects/project-1/milestone-3/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/task-6-container-no-contains"), "GET", "/data/projects/project-1/milestone-3/task-6/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/task-6-container-manager"), "GET", "/data/projects/project-1/milestone-3/task-6/.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/204"), "DELETE", "/data/projects/project-1/milestone-3/task-6/.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/task-48-container"), "GET", "/data/projects/project-1/milestone-3/task-48/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/task-48-container-manager"), "GET", "/data/projects/project-1/milestone-3/task-48/.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/204"), "DELETE", "/data/projects/project-1/milestone-3/task-48/.shapetree", null));
        // Add fixtures for attachments in task-48
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/random-png"), "GET", "/data/projects/project-1/milestone-3/task-48/random.png", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/random-png-manager"), "GET", "/data/projects/project-1/milestone-3/task-48/random.png.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/204"), "DELETE", "/data/projects/project-1/milestone-3/task-48/random.png.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/attachment-48"), "GET", "/data/projects/project-1/milestone-3/task-48/attachment-48", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/attachment-48-manager"), "GET", "/data/projects/project-1/milestone-3/task-48/attachment-48.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/204"), "DELETE", "/data/projects/project-1/milestone-3/task-48/attachment-48.shapetree", null));
        // Add fixtures for issues in /projects/project-1/milestone-3/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/issue-2"), "GET", "/data/projects/project-1/milestone-3/issue-2", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/issue-2-manager"), "GET", "/data/projects/project-1/milestone-3/issue-2.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/204"), "DELETE", "/data/projects/project-1/milestone-3/issue-2.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/issue-3"), "GET", "/data/projects/project-1/milestone-3/issue-3", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/issue-3-manager"), "GET", "/data/projects/project-1/milestone-3/issue-3.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/204"), "DELETE", "/data/projects/project-1/milestone-3/issue-3.shapetree", null));

        URL targetResource = toUrl(server, "/data/projects/");
        URL targetShapeTree = toUrl(server, "/static/shapetrees/project/shapetree#ProjectCollectionTree");

        Response response = unplant(okHttpClient, context, targetResource, targetShapeTree);
        assertEquals(201, response.code());
    }

    @Test
    @DisplayName("Unplant Data Set")
    void unplantData() throws ShapeTreeException {
        // Unplant the data collection, recursing down the tree (success). The root level (pre-loaded) and one level below projects included for completeness
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container"), "GET", "/data/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-manager"), "GET", "/data/.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/204"), "DELETE", "/data/.shapetree", null));
        // Add fixtures for /projects/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container"), "GET", "/data/projects/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container-manager-two-assignments"), "GET", "/data/projects/.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/204"), "PUT", "/data/projects/.shapetree", null));
        // Add fixture for /projects/project-1/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container"), "GET", "/data/projects/project-1/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-manager"), "GET", "/data/projects/project-1/.shapetree", null));

        URL targetResource = toUrl(server, "/data/");
        URL targetShapeTree = toUrl(server, "/static/shapetrees/project/shapetree#DataRepositoryTree");

        // Unplant the data collection, recursing down the tree (only two levels)
        // Since the projects collection still manages /data/projects/, it should not delete the manager, only update it
        Response response = unplant(okHttpClient, context, targetResource, targetShapeTree);
        assertEquals(201, response.code());
    }

    @Test
    @DisplayName("Plant Data Repository with Patch")
    void plantDataRepositoryWithPatch() throws ShapeTreeException {
        // Create the data container
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-no-contains"), "GET", "/data/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/data/.shapetree", null));

        URL targetResource = toUrl(server, "/data/.shapetree");

        // Plant the data repository on newly created data container
        Response response = patch(okHttpClient, context, targetResource, null, getPlantDataRepositorySparqlPatch(server));
        assertEquals(201, response.code());
    }

    @Test
    @DisplayName("Update Project Collection manager with Patch")
    void updateProjectsManagerWithPatch() throws ShapeTreeException {
        // Add fixtures for data repository container and manager
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container"), "GET", "/data/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-manager"), "GET", "/data/.shapetree", null));
        // Add fixtures for /projects/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container-no-contains"), "GET", "/data/projects/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container-manager"), "GET", "/data/projects/.shapetree", null));

        URL targetResource = toUrl(server, "/data/projects/.shapetree");

        // Update the manager directly for the /data/projects/ with PATCH
        Response response = patch(okHttpClient, context, targetResource, null, getUpdateDataRepositorySparqlPatch(server));
        assertEquals(201, response.code());
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

    private String getPlantDataRepositorySparqlPatch(MockWebServer server) {
        return  "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \n" +
                "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \n" +
                "PREFIX st: <http://www.w3.org/ns/shapetrees#> \n" +
                "PREFIX ex: <http://www.example.com/ns/ex#> \n" +
                "INSERT DATA { \n" +
                "   <> a st:Manager . \n" +
                "   <> st:hasAssignment <#ln1> . \n" +
                "   <#ln1> st:assigns <" + toUrl(server, "/static/shapetrees/project/shapetree#DataRepositoryTree") + "> . \n" +
                "   <#ln1> st:manages </data/> . \n" +
                "   <#ln1> st:hasRootAssignment </data/.shapetree#ln1> . \n" +
                "   <#ln1> st:focusNode </data/#repository> . \n" +
                "   <#ln1> st:shape <" + toUrl(server, "/static/shex/project/shex#DataRepositoryShape") + "> . \n" +
                "} \n" ;
    }

    private String getUpdateDataRepositorySparqlPatch(MockWebServer server) {
        return  "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \n" +
                "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \n" +
                "PREFIX st: <http://www.w3.org/ns/shapetrees#> \n" +
                "PREFIX ex: <http://www.example.com/ns/ex#> \n" +
                "INSERT DATA { \n" +
                "   <> a st:Manager . \n" +
                "   <> st:hasAssignment <#ln2> . \n" +
                "   <#ln2> st:assigns <" + toUrl(server, "/static/shapetrees/project/shapetree#ProjectCollectionTree") + "> . \n" +
                "   <#ln2> st:manages </data/projects/> . \n" +
                "   <#ln2> st:hasRootAssignment </data/projects/.shapetree#ln2> . \n" +
                "   <#ln2> st:focusNode </data/projects/#collection> . \n" +
                "   <#ln2> st:shape <" + toUrl(server, "/static/shex/project/shex#ProjectCollectionShape") + "> . \n" +
                "} \n" ;
    }

}