package com.janeirodigital.shapetrees.client.okhttp;

import com.janeirodigital.shapetrees.client.okhttp.fixtures.DispatcherEntry;
import com.janeirodigital.shapetrees.client.okhttp.fixtures.RequestMatchingFixtureDispatcher;
import com.janeirodigital.shapetrees.core.ShapeTreeResponse;
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
    static void beforeEach() {

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
        ShapeTreeResponse response = this.shapeTreeClient.plantShapeTree(this.context, getURI(server, "/data/"), getURI(server, "/static/shapetrees/project/shapetree#DataRepositoryTree"), null, false);
        // Look for 404 because /data doesn't exist
        Assertions.assertEquals(404, response.getStatusCode());

    }

    @Order(3)
    @SneakyThrows
    @Test
    @Label("Create Then Plant Data Container")
    void createThenPlantData() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Create the data container
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-no-contains"), "GET", "/data/", null));

        // Plant the data repository on newly created data container
        ShapeTreeResponse response = this.shapeTreeClient.plantShapeTree(this.context, getURI(server, "/data/"), getURI(server, "/static/shapetrees/project/shapetree#DataRepositoryTree"), getURI(server, "/data/#repository").toString(), false);
        Assertions.assertEquals(201, response.getStatusCode());

    }

    @Order(4)
    @SneakyThrows
    @Test
    @Label("Create Projects Container and Validate DataCollectionTree")
    void createProjectsAndPlantTrees() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Setup initial fixtures for /data/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container"), "GET", "/data/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-locator"), "GET", "/data/.shapetree", null));

        // Create the projects container as a shape tree instance.
        // 1. Will be validated by the parent DataRepositoryTree planted on /data
        // 2. Will have a locator/location created for it as an instance of DataCollectionTree
        ShapeTreeResponse response = shapeTreeClient.postShapeTreeInstance(context, getURI(server, "/data/"), getURI(server, "/data/projects/#collection"), getURI(server, "/static/shapetrees/project/shapetree#DataCollectionTree"), "projects", true, getProjectsBodyGraph(), TEXT_TURTLE);
        Assertions.assertEquals(201, response.getStatusCode());

    }

    @Order(5)
    @SneakyThrows
    @Test
    @Label("Plant ProjectCollectionTree on Projects Container")
    void plantSecondShapeTreeOnProject() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Add fixtures for /data/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container"), "GET", "/data/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/data-container-locator"), "GET", "/data/.shapetree", null));
        // Add fixtures for /projects/
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container-no-contains"), "GET", "/data/projects/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container-locator"), "GET", "/data/projects/.shapetree", null));

        // Plant the second shape tree (ProjectCollectionTree) on /data/projects/
        ShapeTreeResponse response = this.shapeTreeClient.plantShapeTree(this.context, getURI(server, "/data/projects/"), getURI(server, "/static/shapetrees/project/shapetree#ProjectCollectionTree"), getURI(server, "/data/projects/#collection").toString(), false);
        Assertions.assertEquals(201, response.getStatusCode());


    }

    @Order(6)
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

        // Create the project-1 container as a shape tree instance.
        // 1. Will be validated by the parent ProjectCollectionTree planted on /data/projects/
        // 2. Will have a locator/location created for it as an instance of ProjectTree
        ShapeTreeResponse response = shapeTreeClient.postShapeTreeInstance(context, getURI(server, "/data/projects/"), getURI(server, "/data/projects/project-1/#collection"), getURI(server, "/static/shapetrees/project/shapetree#ProjectTree"), "project-1", true, getProjectOneBodyGraph(), TEXT_TURTLE);
        Assertions.assertEquals(201, response.getStatusCode());

        // Add fixtures to represent a successfully added projects container
        dispatcher.removeFixtureByPath("/data/projects/"); // Remove the previous locator from fixtures
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/projects-container"), "GET", "/data/projects/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container"), "GET", "/data/projects/project-1/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("project/project-1-container-locator"), "GET", "/data/projects/project-1/.shapetree", null));

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

}
