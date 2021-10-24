package com.janeirodigital.shapetrees.tests;

import com.janeirodigital.shapetrees.core.ShapeTreeFactory;
import com.janeirodigital.shapetrees.core.contentloaders.DocumentLoaderManager;
import com.janeirodigital.shapetrees.core.contentloaders.HttpExternalDocumentLoader;
import com.janeirodigital.shapetrees.core.enums.RecursionMethods;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
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

import static org.junit.jupiter.api.Assertions.*;

@Slf4j
class ShapeTreeParsingTests {

    private static RequestMatchingFixtureDispatcher dispatcher = null;
    private static HttpExternalDocumentLoader httpExternalDocumentLoader;

    public ShapeTreeParsingTests() {
        httpExternalDocumentLoader = new HttpExternalDocumentLoader();
        DocumentLoaderManager.setLoader(httpExternalDocumentLoader);
    }

    protected URL getURL(MockWebServer server, String path) throws URLSyntaxException {
        return new URL(server.url(path).toString());
    }

    @BeforeAll
    static void beforeAll() {
        dispatcher = new RequestMatchingFixtureDispatcher(List.of(
                new DispatcherEntry(List.of("shapetrees/project-shapetree-ttl"), "GET", "/static/shapetrees/project/shapetree", null),
                new DispatcherEntry(List.of("shapetrees/project-shapetree-virtual-ttl"), "GET", "/static/shapetrees/project/shapetree-virtual", null),
                new DispatcherEntry(List.of("shapetrees/project-shapetree-invalid-ttl"), "GET", "/static/shapetrees/project/shapetree-invalid", null),
                new DispatcherEntry(List.of("shapetrees/project-shapetree-invalid-2-ttl"), "GET", "/static/shapetrees/project/shapetree-invalid2", null),
                new DispatcherEntry(List.of("http/404"), "GET", "/static/shapetrees/project/shapetree-missing", null)
        ));
    }

    @SneakyThrows
    @Test
    @DisplayName("Reuse previously cached shapetree")
    void parseShapeTreeReuse() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ShapeTree projectShapeTree1 = ShapeTreeFactory.getShapeTree(getURL(server,"/static/shapetrees/project/shapetree#ProjectTree"));
        Assertions.assertNotNull(projectShapeTree1);
        ShapeTree projectShapeTree2 = ShapeTreeFactory.getShapeTree(getURL(server,"/static/shapetrees/project/shapetree#ProjectTree"));
        Assertions.assertNotNull(projectShapeTree1);
        assertEquals(projectShapeTree1.hashCode(), projectShapeTree2.hashCode());
    }

    @SneakyThrows
    @Test
    @DisplayName("Invalid shapetree contents within non-container")
    void validateContainsWithinNonContainer() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        Assertions.assertThrows(ShapeTreeException.class, () ->
                ShapeTreeFactory.getShapeTree(getURL(server,"/static/shapetrees/project/shapetree-invalid#DataRepositoryTree"))
        );
    }

    @SneakyThrows
    @Test
    @DisplayName("Fail to parse missing shape tree")
    void failToParseMissingShapeTree() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        Assertions.assertThrows(ShapeTreeException.class, () ->
                ShapeTreeFactory.getShapeTree(getURL(server,"/static/shapetrees/project/shapetree-missing#missing"))
        );
    }

    @SneakyThrows
    @Test
    @DisplayName("Ensure reuse within recursion")
    void ensureCacheWithRecursion() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        // Retrieve the MilestoneTree shapetree (which is referred to by the ProjectTree shapetree)
        ShapeTree milestoneShapeTree1 = ShapeTreeFactory.getShapeTree(getURL(server,"/static/shapetrees/project/shapetree-virtual#MilestoneTree"));
        Assertions.assertNotNull(milestoneShapeTree1);

        // Retrieve the ProjectTree shapetree which will recursively cache the MilestoneTree shapetree
        ShapeTree projectShapeTree1 = ShapeTreeFactory.getShapeTree(getURL(server,"/static/shapetrees/project/shapetree-virtual#ProjectTree"));
        Assertions.assertNotNull(projectShapeTree1);

        // Retrieve the MilestoneTree shapetree again, ensuring the same instance is used
        ShapeTree milestoneShapeTree2 = ShapeTreeFactory.getShapeTree(getURL(server,"/static/shapetrees/project/shapetree-virtual#MilestoneTree"));
        Assertions.assertNotNull(milestoneShapeTree2);

        assertEquals(milestoneShapeTree1.hashCode(), milestoneShapeTree2.hashCode());
    }


    @SneakyThrows
    @Test
    @DisplayName("Parse Tree with references")
    void parseShapeTreeReferences() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ShapeTree projectShapeTree = ShapeTreeFactory.getShapeTree(getURL(server,"/static/shapetrees/project/shapetree-virtual#ProjectTree"));
        Assertions.assertNotNull(projectShapeTree);
        assertFalse(projectShapeTree.getReferences().isEmpty());
    }

    @SneakyThrows
    @Test
    @DisplayName("Parse Tree with contains")
    void parseShapeTreeContains() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ShapeTree projectShapeTree = ShapeTreeFactory.getShapeTree(getURL(server,"/static/shapetrees/project/shapetree#ProjectTree"));
        Assertions.assertNotNull(projectShapeTree);
        assertTrue(projectShapeTree.getContains().contains(getURL(server,"/static/shapetrees/project/shapetree#MilestoneTree")));
    }

    @SneakyThrows
    @Test
    @DisplayName("Parse Tree Failure")
    void parseShapeTreeFailure() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        Assertions.assertThrows(ShapeTreeException.class, () ->
            ShapeTreeFactory.getShapeTree(getURL(server,"/static/shapetrees/project-record/shapetree-invalid#DataRepositoryTree"))
        );
    }

    @SneakyThrows
    @Test
    @DisplayName("Traverse References")
    void testTraverseReferences() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ShapeTree projectShapeTree = ShapeTreeFactory.getShapeTree(getURL(server,"/static/shapetrees/project/shapetree-virtual#ProjectTree"));
        projectShapeTree.getReferencedShapeTrees();
        Assertions.assertTrue(projectShapeTree.getReferencedShapeTrees(RecursionMethods.BREADTH_FIRST).hasNext());
        Assertions.assertTrue(projectShapeTree.getReferencedShapeTrees(RecursionMethods.DEPTH_FIRST).hasNext());
    }
}
