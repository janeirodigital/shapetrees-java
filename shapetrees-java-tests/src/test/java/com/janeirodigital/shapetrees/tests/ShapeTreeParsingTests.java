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

import java.util.List;

import static com.janeirodigital.shapetrees.tests.fixtures.MockWebServerHelper.toUrl;
import static org.junit.jupiter.api.Assertions.*;

@Slf4j
class ShapeTreeParsingTests {

    private static RequestMatchingFixtureDispatcher dispatcher = null;
    private static HttpExternalDocumentLoader httpExternalDocumentLoader;

    public ShapeTreeParsingTests() {
        httpExternalDocumentLoader = new HttpExternalDocumentLoader();
        DocumentLoaderManager.setLoader(httpExternalDocumentLoader);
    }

    @BeforeAll
    static void beforeAll() {
        dispatcher = new RequestMatchingFixtureDispatcher(List.of(
                new DispatcherEntry(List.of("shapetrees/project-shapetree-ttl"), "GET", "/static/shapetrees/project/shapetree", null),
                new DispatcherEntry(List.of("shapetrees/business-shapetree-ttl"), "GET", "/static/shapetrees/business/shapetree", null),
                new DispatcherEntry(List.of("shapetrees/reserved-type-shapetree-ttl"), "GET", "/static/shapetrees/reserved/shapetree", null),
                new DispatcherEntry(List.of("shapetrees/project-shapetree-virtual-ttl"), "GET", "/static/shapetrees/project/shapetree-virtual", null),
                new DispatcherEntry(List.of("shapetrees/project-shapetree-invalid-ttl"), "GET", "/static/shapetrees/project/shapetree-invalid", null),
                new DispatcherEntry(List.of("shapetrees/project-shapetree-invalid-2-ttl"), "GET", "/static/shapetrees/project/shapetree-invalid2", null),
                new DispatcherEntry(List.of("shapetrees/content-type-invalid-shapetree-ttl"), "GET", "/static/shapetrees/project/shapetree-bad-content-type", null),
                new DispatcherEntry(List.of("shapetrees/missing-expects-type-shapetree-ttl"), "GET", "/static/shapetrees/invalid/missing-expects-type", null),
                new DispatcherEntry(List.of("shapetrees/contains-with-bad-expects-type-shapetree-ttl"), "GET", "/static/shapetrees/invalid/contains-with-bad-expects-type", null),
                new DispatcherEntry(List.of("shapetrees/bad-object-type-shapetree-ttl"), "GET", "/static/shapetrees/invalid/bad-object-type", null),
                new DispatcherEntry(List.of("shapetrees/invalid-contains-objects-shapetree-ttl"), "GET", "/static/shapetrees/invalid/shapetree-invalid-contains-objects", null),
                new DispatcherEntry(List.of("shapetrees/contains-with-nonrdf-expects-type-shapetree-ttl"), "GET", "/static/shapetrees/invalid/contains-with-nonrdf-expects-type", null),
                new DispatcherEntry(List.of("http/404"), "GET", "/static/shapetrees/invalid/shapetree-missing", null)
        ));
    }

    @SneakyThrows
    @Test
    @DisplayName("Reuse previously cached shapetree")
    void parseShapeTreeReuse() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ShapeTree projectShapeTree1 = ShapeTreeFactory.getShapeTree(toUrl(server,"/static/shapetrees/project/shapetree#ProjectTree"));
        Assertions.assertNotNull(projectShapeTree1);
        ShapeTree projectShapeTree2 = ShapeTreeFactory.getShapeTree(toUrl(server,"/static/shapetrees/project/shapetree#ProjectTree"));
        Assertions.assertNotNull(projectShapeTree2);
        assertEquals(projectShapeTree1.hashCode(), projectShapeTree2.hashCode());
        // The "business" shape tree won't be in the cache, but it cross-contains pm:MilestoneTree, which should be.
        ShapeTree businessShapeTree = ShapeTreeFactory.getShapeTree(toUrl(server,"/static/shapetrees/business/shapetree#BusinessTree"));
        Assertions.assertNotNull(businessShapeTree);
    }

    @SneakyThrows
    @Test
    @DisplayName("Ensure reuse within recursion")
    void ensureCacheWithRecursion() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        // Retrieve the MilestoneTree shapetree (which is referred to by the ProjectTree shapetree)
        ShapeTree milestoneShapeTree1 = ShapeTreeFactory.getShapeTree(toUrl(server,"/static/shapetrees/project/shapetree-virtual#MilestoneTree"));
        Assertions.assertNotNull(milestoneShapeTree1);

        // Retrieve the ProjectTree shapetree which will recursively cache the MilestoneTree shapetree
        ShapeTree projectShapeTree1 = ShapeTreeFactory.getShapeTree(toUrl(server,"/static/shapetrees/project/shapetree-virtual#ProjectTree"));
        Assertions.assertNotNull(projectShapeTree1);

        // Retrieve the MilestoneTree shapetree again, ensuring the same instance is used
        ShapeTree milestoneShapeTree2 = ShapeTreeFactory.getShapeTree(toUrl(server,"/static/shapetrees/project/shapetree-virtual#MilestoneTree"));
        Assertions.assertNotNull(milestoneShapeTree2);

        assertEquals(milestoneShapeTree1.hashCode(), milestoneShapeTree2.hashCode());
    }


    @SneakyThrows
    @Test
    @DisplayName("Parse Tree with references")
    void parseShapeTreeReferences() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ShapeTree projectShapeTree = ShapeTreeFactory.getShapeTree(toUrl(server,"/static/shapetrees/project/shapetree-virtual#ProjectTree"));
        Assertions.assertNotNull(projectShapeTree);
        assertFalse(projectShapeTree.getReferences().isEmpty());
    }

    @SneakyThrows
    @Test
    @DisplayName("Parse Tree with contains")
    void parseShapeTreeContains() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ShapeTree projectShapeTree = ShapeTreeFactory.getShapeTree(toUrl(server,"/static/shapetrees/project/shapetree#ProjectTree"));
        Assertions.assertNotNull(projectShapeTree);
        assertTrue(projectShapeTree.getContains().contains(toUrl(server,"/static/shapetrees/project/shapetree#MilestoneTree")));
    }

    /* TODO - This test currently fails due to cross-resource parsing.
    @SneakyThrows
    @Test
    @DisplayName("Parse Tree that allows reserved resource types")
    void parseShapeTreeContainsReservedTypes() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ShapeTree reservedShapeTree = ShapeTreeFactory.getShapeTree(getURL(server,"/static/shapetrees/reserved/shapetree#EverythingTree"));
        Assertions.assertNotNull(reservedShapeTree);
        assertTrue(reservedShapeTree.getContains().contains(getURL(server,"http://www.w3.org/ns/shapetrees#ResourceTree")));
        assertTrue(reservedShapeTree.getContains().contains(getURL(server,"http://www.w3.org/ns/shapetrees#NonRDFResourceTree")));
        assertTrue(reservedShapeTree.getContains().contains(getURL(server,"https://www.w3.org/ns/shapetrees#ContainerTree")));
    }
     */

    @SneakyThrows
    @Test
    @DisplayName("Traverse References")
    void testTraverseReferences() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ShapeTree projectShapeTree = ShapeTreeFactory.getShapeTree(toUrl(server,"/static/shapetrees/project/shapetree-virtual#ProjectTree"));
        projectShapeTree.getReferencedShapeTrees();
        Assertions.assertTrue(projectShapeTree.getReferencedShapeTrees(RecursionMethods.BREADTH_FIRST).hasNext());
        Assertions.assertTrue(projectShapeTree.getReferencedShapeTrees(RecursionMethods.DEPTH_FIRST).hasNext());
    }

    @SneakyThrows
    @Test
    @DisplayName("Fail to parse shape tree with missing expectsType")
    void failToParseMissingExpectsType() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        Assertions.assertThrows(ShapeTreeException.class, () ->
            ShapeTreeFactory.getShapeTree(toUrl(server,"/static/shapetrees/invalid/missing-expects-type#DataRepositoryTree"))
        );
    }

    @SneakyThrows
    @Test
    @DisplayName("Fail to parse shape tree with st:contains but expects a non-container resource")
    void failToParseBadExpectsTypeOnContains() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        Assertions.assertThrows(ShapeTreeException.class, () ->
                ShapeTreeFactory.getShapeTree(toUrl(server,"/static/shapetrees/invalid/contains-with-bad-expects-type#DataRepositoryTree"))
        );
        Assertions.assertThrows(ShapeTreeException.class, () ->
                ShapeTreeFactory.getShapeTree(toUrl(server,"/static/shapetrees/invalid/contains-with-nonrdf-expects-type#DataRepositoryTree"))
        );
    }

    @SneakyThrows
    @Test
    @DisplayName("Fail to parse shape tree with invalid object type")
    void failToParseBadObjectTypeOnContains() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        Assertions.assertThrows(ShapeTreeException.class, () ->
                ShapeTreeFactory.getShapeTree(toUrl(server,"/static/shapetrees/invalid/bad-object-type#DataRepositoryTree"))
        );
    }

    @SneakyThrows
    @Test
    @DisplayName("Fail to parse missing shape tree")
    void failToParseMissingShapeTree() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        Assertions.assertThrows(ShapeTreeException.class, () ->
                ShapeTreeFactory.getShapeTree(toUrl(server,"/static/shapetrees/invalid/shapetree-missing#missing"))
        );
    }

    @Test
    @DisplayName("Fail to parse shape tree with invalid content type")
    void failToParseShapeTreeWithInvalidContentType() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        Assertions.assertThrows(ShapeTreeException.class, () ->
                ShapeTreeFactory.getShapeTree(toUrl(server,"/static/shapetrees/project/shapetree-bad-content-type#bad"))
        );
    }

    @SneakyThrows
    @Test
    @DisplayName("Fail to parse shape tree with invalid contains objects")
    void failToParseInvalidContainsObjects() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        Assertions.assertThrows(ShapeTreeException.class, () ->
            ShapeTreeFactory.getShapeTree(toUrl(server,"/static/shapetrees/invalid/shapetree-invalid-contains-objects#DataRepositoryTree"))
        );
    }

}
