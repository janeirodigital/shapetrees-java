package com.janeirodigital.shapetrees.tests;

import com.janeirodigital.shapetrees.core.ShapeTreeAssignment;
import com.janeirodigital.shapetrees.core.ShapeTreeManager;
import com.janeirodigital.shapetrees.core.contentloaders.DocumentLoaderManager;
import com.janeirodigital.shapetrees.core.contentloaders.HttpExternalDocumentLoader;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.helpers.GraphHelper;
import com.janeirodigital.shapetrees.tests.fixtures.DispatcherEntry;
import com.janeirodigital.shapetrees.tests.fixtures.RequestMatchingFixtureDispatcher;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import okhttp3.mockwebserver.MockWebServer;
import org.apache.jena.graph.Graph;
import org.junit.jupiter.api.*;

import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.util.List;

import static com.janeirodigital.shapetrees.tests.fixtures.MockWebServerHelper.toUrl;

@Slf4j
class ShapeTreeManagerTests {

    private static URL managerUrl;
    private static ShapeTreeManager manager;
    private static MockWebServer server;
    private static ShapeTreeAssignment assignment1, assignment2, assignment3, nonContainingAssignment1, nonContainingAssignment2, containingAssignment1;
    private static RequestMatchingFixtureDispatcher dispatcher = null;
    private static HttpExternalDocumentLoader httpExternalDocumentLoader;

    public ShapeTreeManagerTests() {
        httpExternalDocumentLoader = new HttpExternalDocumentLoader();
        DocumentLoaderManager.setLoader(httpExternalDocumentLoader);
    }

    @BeforeAll
    static void beforeAll() throws MalformedURLException {

        dispatcher = new RequestMatchingFixtureDispatcher(List.of(
                new DispatcherEntry(List.of("shapetrees/manager-shapetree-ttl"), "GET", "/static/shapetrees/managers/shapetree", null)
        ));

        server = new MockWebServer();
        server.setDispatcher(dispatcher);

        managerUrl = new URL("https://site.example/resource.shapetree");
        assignment1 = new ShapeTreeAssignment(
                new URL("https://tree.example/tree#TreeOne"),
                new URL("https://site.example/resource"),
                new URL("https://site.example/resource.shapetree#ln1"),
                new URL("https://site.example/resource#node"),
                new URL("https://shapes.example/schema#ShapeOne"),
                new URL("https://site.example/resource.shapetree#ln1"));

        assignment2 = new ShapeTreeAssignment(
                new URL("https://tree.example/tree#TreeTwo"),
                new URL("https://site.example/resource"),
                new URL("https://site.example/resource.shapetree#ln2"),
                new URL("https://site.example/resource#node"),
                new URL("https://shapes.example/schema#ShapeTwo"),
                new URL("https://site.example/resource.shapetree#ln2"));

        assignment3 = new ShapeTreeAssignment(
                new URL("https://tree.example/tree#TreeThree"),
                new URL("https://site.example/resource"),
                new URL("https://site.example/resource.shapetree#ln3"),
                new URL("https://site.example/resource#node"),
                new URL("https://shapes.example/schema#ShapeThree"),
                new URL("https://site.example/resource.shapetree#ln3"));

        nonContainingAssignment1 = new ShapeTreeAssignment(
                toUrl(server, "/static/shapetrees/managers/shapetree#NonContainingTree"),
                toUrl(server, "/data/container/"),
                toUrl(server, "/data/container/.shapetree#ln1"),
                toUrl(server, "/data/container/#container"),
                null,
                toUrl(server, "/data/container/.shapetree#ln1"));

        containingAssignment1 = new ShapeTreeAssignment(
                toUrl(server, "/static/shapetrees/managers/shapetree#ContainingTree"),
                toUrl(server, "/data/container/"),
                toUrl(server, "/data/container/.shapetree#ln2"),
                toUrl(server, "/data/container/#container"),
                null,
                toUrl(server, "/data/container/.shapetree#ln2"));

        nonContainingAssignment2 = new ShapeTreeAssignment(
                toUrl(server, "/static/shapetrees/managers/shapetree#NonContainingTree2"),
                toUrl(server, "/data/container/"),
                toUrl(server, "/data/container/.shapetree#ln3"),
                toUrl(server, "/data/container/#container"),
                null,
                toUrl(server, "/data/container/.shapetree#ln3"));

    }

    @BeforeEach
    void beforeEach() {
        manager = new ShapeTreeManager(managerUrl);
    }

    @SneakyThrows
    @Test
    @DisplayName("Initialize a new manager")
    void initializeShapeTreeManager() {
        ShapeTreeManager newManager = new ShapeTreeManager(managerUrl);
        Assertions.assertNotNull(newManager);
        Assertions.assertEquals(newManager.getId(), managerUrl);
    }

    @SneakyThrows
    @Test
    @DisplayName("Add a new assignment")
    void addNewShapeTreeAssignmentToManager() {
        Assertions.assertTrue(manager.getAssignments().isEmpty());
        manager.addAssignment(assignment1);
        Assertions.assertFalse(manager.getAssignments().isEmpty());
        Assertions.assertEquals(manager.getAssignments().size(), 1);
    }

    @SneakyThrows
    @Test
    @DisplayName("Fail to add a null assignment")
    void failToAddNullAssignmentToManager() {
        Assertions.assertThrows( ShapeTreeException.class, () -> { manager.addAssignment(null); });
    }

    @SneakyThrows
    @Test
    @DisplayName("Fail to add a duplicate assignment")
    void failToAddDuplicateAssignment() {
        manager.addAssignment(assignment1);
        Assertions.assertThrows( ShapeTreeException.class, () -> { manager.addAssignment(assignment1); });
    }

    @SneakyThrows
    @Test
    @DisplayName("Fail to add assignment with Malformed URLs")
    void failToAddAssignmentWithBadUrls() {

        Assertions.assertThrows( MalformedURLException.class, () -> {
            ShapeTreeAssignment assignmentWithNullUrl = new ShapeTreeAssignment(
                    new URL("https://tree.example/tree#TreeThree"),
                    new URL("https://site.example/resource"),
                    new URL(null),
                    new URL("https://site.example/resource#node"),
                    new URL("https://shapes.example/schema#ShapeThree"),
                    new URL("https://site.example/resource.shapetree#ln3"));
        });

        Assertions.assertThrows( MalformedURLException.class, () -> {
            ShapeTreeAssignment assignmentWithNullRoot = new ShapeTreeAssignment(
                    new URL("https://tree.example/tree#TreeThree"),
                    new URL("https://site.example/resource"),
                    new URL("https://site.example/resource.shapetree#ln3"),
                    new URL("https://site.example/resource#node"),
                    new URL("https://shapes.example/schema#ShapeThree"),
                    new URL("urn:cool:names:specification:what:evs:rdf:4.1.2"));
        });

    }

    @SneakyThrows
    @Test
    @DisplayName("Fail to mint the same assignment twice")
    void failToMintDuplicateAssignment() {
        manager.addAssignment(assignment1);
        URL adjustedUrl = manager.mintAssignmentUrl(assignment1.getUrl());
        Assertions.assertNotEquals(assignment1.getUrl(), adjustedUrl);
    }

    @SneakyThrows
    @Test
    @DisplayName("Get containing shape tree assignment from shape tree manager")
    void getContainingShapeTreeAssignmentsFromManager() {

        manager.addAssignment(nonContainingAssignment1);
        manager.addAssignment(containingAssignment1);

        Assertions.assertEquals(1, manager.getContainingAssignments().size());
        Assertions.assertTrue(manager.getContainingAssignments().contains(containingAssignment1));
        Assertions.assertFalse(manager.getContainingAssignments().contains(nonContainingAssignment1));

    }

    @SneakyThrows
    @Test
    @DisplayName("Get no containing shape tree assignment for shape tree manager")
    void getNoContainingShapeTreeAssignmentFromManager() {
        manager.addAssignment(nonContainingAssignment1);
        manager.addAssignment(nonContainingAssignment2);
        Assertions.assertTrue(manager.getContainingAssignments().isEmpty());
    }

    @SneakyThrows
    @Test
    @DisplayName("Get no shape tree assignment for shape tree from manager with no assignments")
    void getNoShapeTreeAssignmentsFromEmptyManager() {
        Assertions.assertNull(manager.getAssignmentForShapeTree(new URL("https://tree.example/shapetree#ExampleTree")));
    }

    @SneakyThrows
    @Test
    @DisplayName("Get shape tree assignment from manager for shape tree")
    void getShapeTreeAssignmentFromManagerForShapeTree() {
        manager.addAssignment(nonContainingAssignment1);
        manager.addAssignment(nonContainingAssignment2);
        manager.addAssignment(containingAssignment1);
        Assertions.assertEquals(containingAssignment1, manager.getAssignmentForShapeTree(containingAssignment1.getShapeTree()));
    }

    @SneakyThrows
    @Test
    @DisplayName("Get no shape tree assignment from manager without matching shape tree")
    void getNoShapeTreeAssignmentForShapeTree() {
        manager.addAssignment(nonContainingAssignment1);
        manager.addAssignment(nonContainingAssignment2);
        manager.addAssignment(containingAssignment1);
        Assertions.assertNull(manager.getAssignmentForShapeTree(new URL("https://tree.example/shapetree#ExampleTree")));
    }

    @SneakyThrows
    @Test
    @DisplayName("Fail to remove assignment from empty manager")
    void failToRemoveAssignmentFromEmptyManager() {
        Assertions.assertThrows( IllegalStateException.class, () -> {
            manager.removeAssignment(containingAssignment1);
        });
    }

    @SneakyThrows
    @Test
    @DisplayName("Fail to remove assignment from empty manager")
    void failToRemoveAssignmentMissingFromManager() {
        manager.addAssignment(nonContainingAssignment1);
        manager.addAssignment(nonContainingAssignment2);
        Assertions.assertThrows( IllegalStateException.class, () -> {
            manager.removeAssignment(containingAssignment1);
        });
    }

    @SneakyThrows
    @Test
    @DisplayName("Remove assignment from manager")
    void removeAssignmentFromManager() {
        manager.addAssignment(nonContainingAssignment1);
        manager.addAssignment(nonContainingAssignment2);
        manager.addAssignment(containingAssignment1);
        Assertions.assertEquals(manager.getAssignmentForShapeTree(containingAssignment1.getShapeTree()), containingAssignment1);
        manager.removeAssignment(containingAssignment1);
        Assertions.assertNull(manager.getAssignmentForShapeTree(containingAssignment1.getShapeTree()));
    }

    @SneakyThrows
    @Test
    @DisplayName("Get valid assignment from graph")
    void getAssignmentFromGraph() {

        URI managerUri = URI.create("https://data.example/container.shapetree");
        Graph managerGraph = GraphHelper.readStringIntoGraph(managerUri, getValidManagerString(), "text/turtle");
        ShapeTreeManager manager = ShapeTreeManager.getFromGraph(managerUri.toURL(), managerGraph);

        Assertions.assertNotNull(manager);
        Assertions.assertNotNull(manager.getAssignmentForShapeTree(new URL("https://tree.example/#Tree1")));

    }

    @SneakyThrows
    @Test
    @DisplayName("Fail to get assignment from graph due to missing triples")
    void failToGetAssignmentFromGraphMissingTriples() {

        URI managerUri = URI.create("https://data.example/container.shapetree");
        Graph managerGraph = GraphHelper.readStringIntoGraph(managerUri, getInvalidManagerMissingTriplesString(), "text/turtle");
        Assertions.assertThrows( IllegalStateException.class, () -> {
            ShapeTreeManager.getFromGraph(managerUrl, managerGraph);
        });

    }

    @SneakyThrows
    @Test
    @DisplayName("Fail to get assignment from graph due to unexpected values")
    void failToGetAssignmentFromGraphUnexpectedValues() {

        URI managerUri = URI.create("https://data.example/container.shapetree");
        Graph managerGraph = GraphHelper.readStringIntoGraph(managerUri, getInvalidManagerUnexpectedTriplesString(), "text/turtle");
        Assertions.assertThrows( IllegalStateException.class, () -> {
            ShapeTreeManager.getFromGraph(managerUrl, managerGraph);
        });

    }

    private String getValidManagerString() {
        return "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \n" +
                "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \n" +
                "PREFIX xml: <http://www.w3.org/XML/1998/namespace> \n" +
                "PREFIX st: <http://www.w3.org/ns/shapetrees#> \n \n" +
                "PREFIX ex: <http://www.example.com/ns/ex#> \n" +
                "\n" +
                "\n" +
                "<https://data.example/container.shapetree> \n" +
                "    a st:Manager ; \n" +
                "    st:hasAssignment <https://data.example/container.shapetree#ln1> . \n" +
                "\n" +
                "<https://data.example/container.shapetree#ln1> \n" +
                "    st:assigns <https://tree.example/#Tree1> ; \n" +
                "    st:hasRootAssignment <https://data.example/container.shapetree#ln1> ; \n" +
                "    st:manages <https://data.example/container> ; \n" +
                "    st:shape <https://shapes.example/#Shape1> ; \n" +
                "    st:focusNode <https://data.example/container#node> . \n" +
                "\n" ;
    }

    private String getInvalidManagerMissingTriplesString() {
        return "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \n" +
                "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \n" +
                "PREFIX xml: <http://www.w3.org/XML/1998/namespace> \n" +
                "PREFIX st: <http://www.w3.org/ns/shapetrees#> \n \n" +
                "PREFIX ex: <http://www.example.com/ns/ex#> \n" +
                "\n" +
                "\n" +
                "<https://data.example/container.shapetree> \n" +
                "    a st:Manager ; \n" +
                "    st:hasAssignment <https://data.example/container.shapetree#ln1> . \n" +
                "\n" +
                "<https://data.example/container.shapetree#ln1> \n" +
                "    st:assigns <https://tree.example/#Tree1> ; \n" +
                "\n" ;
    }

    private String getInvalidManagerUnexpectedTriplesString() {
        return "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \n" +
                "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \n" +
                "PREFIX xml: <http://www.w3.org/XML/1998/namespace> \n" +
                "PREFIX st: <http://www.w3.org/ns/shapetrees#> \n \n" +
                "PREFIX ex: <http://www.example.com/ns/ex#> \n" +
                "\n" +
                "\n" +
                "<https://data.example/container.shapetree> \n" +
                "    a st:Manager ; \n" +
                "    st:hasAssignment <https://data.example/container.shapetree#ln1> . \n" +
                "\n" +
                "<https://data.example/container.shapetree#ln1> \n" +
                "    st:assigns <https://tree.example/#Tree1> ; \n" +
                "    st:hasRootAssignment <https://data.example/container.shapetree#ln1> ; \n" +
                "    st:manages <https://data.example/container> ; \n" +
                "    st:shape <https://shapes.example/#Shape1> ; \n" +
                "    st:focusNode <https://data.example/container#node> ; \n" +
                "    st:unexpected \"why am i here\" . \n" +
                "\n" ;
    }

}
