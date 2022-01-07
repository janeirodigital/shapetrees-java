package com.janeirodigital.shapetrees.tests;

import com.janeirodigital.shapetrees.core.ShapeTree;
import com.janeirodigital.shapetrees.core.ShapeTreeFactory;
import com.janeirodigital.shapetrees.core.ShapeTreeResource;
import com.janeirodigital.shapetrees.core.contentloaders.DocumentLoaderManager;
import com.janeirodigital.shapetrees.core.contentloaders.HttpExternalDocumentLoader;
import com.janeirodigital.shapetrees.core.enums.RecursionMethod;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.tests.fixtures.RequestMatchingFixtureDispatcher;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.jupiter.api.*;

import static com.janeirodigital.shapetrees.core.helpers.GraphHelper.urlToUri;
import static com.janeirodigital.shapetrees.tests.fixtures.DispatcherHelper.mockOnGet;
import static com.janeirodigital.shapetrees.tests.fixtures.MockWebServerHelper.toUrl;
import static org.junit.jupiter.api.Assertions.*;

@Slf4j
class ShapeTreeParsingTests {

    private static MockWebServer server;

    public ShapeTreeParsingTests() {
        HttpExternalDocumentLoader httpExternalDocumentLoader = new HttpExternalDocumentLoader();
        DocumentLoaderManager.setLoader(httpExternalDocumentLoader);
    }

    @BeforeEach
    @SneakyThrows
    void beforeEach() {
        ShapeTreeFactory.clearCache();
        ShapeTreeResource.clearCache();
    }

    @BeforeAll
    static void beforeAll() {
        RequestMatchingFixtureDispatcher dispatcher = new RequestMatchingFixtureDispatcher();

        mockOnGet(dispatcher, "/static/shapetrees/project/shapetree", "shapetrees/project-shapetree-ttl");
        mockOnGet(dispatcher, "/static/shapetrees/business/shapetree", "shapetrees/business-shapetree-ttl");
        mockOnGet(dispatcher, "/static/shapetrees/reserved/shapetree", "shapetrees/reserved-type-shapetree-ttl");
        mockOnGet(dispatcher, "/static/shapetrees/project/shapetree-virtual", "shapetrees/project-shapetree-virtual-ttl");
        mockOnGet(dispatcher, "/static/shapetrees/project/shapetree-invalid", "shapetrees/project-shapetree-invalid-ttl");
        mockOnGet(dispatcher, "/static/shapetrees/project/shapetree-invalid2", "shapetrees/project-shapetree-invalid-2-ttl");
        mockOnGet(dispatcher, "/static/shapetrees/project/shapetree-bad-content-type", "shapetrees/content-type-invalid-shapetree-ttl");
        mockOnGet(dispatcher, "/static/shapetrees/invalid/missing-expects-type", "shapetrees/missing-expects-type-shapetree-ttl");
        mockOnGet(dispatcher, "/static/shapetrees/invalid/contains-with-bad-expects-type", "shapetrees/contains-with-bad-expects-type-shapetree-ttl");
        mockOnGet(dispatcher, "/static/shapetrees/invalid/bad-object-type", "shapetrees/bad-object-type-shapetree-ttl");
        mockOnGet(dispatcher, "/static/shapetrees/invalid/shapetree-invalid-contains-objects", "shapetrees/invalid-contains-objects-shapetree-ttl");
        mockOnGet(dispatcher, "/static/shapetrees/invalid/contains-with-nonrdf-expects-type", "shapetrees/contains-with-nonrdf-expects-type-shapetree-ttl");
        mockOnGet(dispatcher, "/static/shapetrees/parsing/contains-1", "parsing/contains/contains-1-ttl");
        mockOnGet(dispatcher, "/static/shapetrees/parsing/contains-2", "parsing/contains/contains-2-ttl");
        mockOnGet(dispatcher, "/static/shapetrees/parsing/contains-2A", "parsing/contains/contains-2A-ttl");
        mockOnGet(dispatcher, "/static/shapetrees/parsing/contains-2B", "parsing/contains/contains-2B-ttl");
        mockOnGet(dispatcher, "/static/shapetrees/parsing/contains-2C", "parsing/contains/contains-2C-ttl");
        mockOnGet(dispatcher, "/static/shapetrees/parsing/contains-2C2", "parsing/contains/contains-2C2-ttl");
        mockOnGet(dispatcher, "/static/shapetrees/parsing/references-1", "parsing/references/references-1-ttl");
        mockOnGet(dispatcher, "/static/shapetrees/parsing/references-2", "parsing/references/references-2-ttl");
        mockOnGet(dispatcher, "/static/shapetrees/parsing/references-2A", "parsing/references/references-2A-ttl");
        mockOnGet(dispatcher, "/static/shapetrees/parsing/references-2B", "parsing/references/references-2B-ttl");
        mockOnGet(dispatcher, "/static/shapetrees/parsing/references-2C", "parsing/references/references-2C-ttl");
        mockOnGet(dispatcher, "/static/shapetrees/parsing/references-2C2", "parsing/references/references-2C2-ttl");
        mockOnGet(dispatcher, "/static/shapetrees/parsing/mixed-1", "parsing/mixed/mixed-1-ttl");
        mockOnGet(dispatcher, "/static/shapetrees/parsing/mixed-2", "parsing/mixed/mixed-2-ttl");
        mockOnGet(dispatcher, "/static/shapetrees/parsing/mixed-2A", "parsing/mixed/mixed-2A-ttl");
        mockOnGet(dispatcher, "/static/shapetrees/parsing/mixed-2B", "parsing/mixed/mixed-2B-ttl");
        mockOnGet(dispatcher, "/static/shapetrees/parsing/mixed-2C", "parsing/mixed/mixed-2C-ttl");
        mockOnGet(dispatcher, "/static/shapetrees/parsing/mixed-2C2", "parsing/mixed/mixed-2C2-ttl");
        mockOnGet(dispatcher, "/static/shapetrees/parsing/mixed-2D", "parsing/mixed/mixed-2D-ttl");
        mockOnGet(dispatcher, "/static/shapetrees/parsing/cycle", "parsing/cycle-ttl");
        mockOnGet(dispatcher, "/static/shapetrees/invalid/shapetree-missing", "http/404");

        server = new MockWebServer();
        server.setDispatcher(dispatcher);
    }

    @SneakyThrows
    @Test
    @DisplayName("Reuse previously cached shapetree")
    void parseShapeTreeReuse() {
        ShapeTree projectShapeTree1 = ShapeTreeFactory.getShapeTree(toUrl(server,"/static/shapetrees/project/shapetree#ProjectTree"));
        assertNotNull(projectShapeTree1);
        ShapeTree projectShapeTree2 = ShapeTreeFactory.getShapeTree(toUrl(server,"/static/shapetrees/project/shapetree#ProjectTree"));
        assertNotNull(projectShapeTree2);
        assertEquals(projectShapeTree1.hashCode(), projectShapeTree2.hashCode());
        // The "business" shape tree won't be in the cache, but it cross-contains pm:MilestoneTree, which should be.
        ShapeTree businessShapeTree = ShapeTreeFactory.getShapeTree(toUrl(server,"/static/shapetrees/business/shapetree#BusinessTree"));
        assertNotNull(businessShapeTree);
    }

    @SneakyThrows
    @Test
    @DisplayName("Ensure reuse within recursion")
    void ensureCacheWithRecursion() {
        // Retrieve the MilestoneTree shapetree (which is referred to by the ProjectTree shapetree)
        ShapeTree milestoneShapeTree1 = ShapeTreeFactory.getShapeTree(toUrl(server,"/static/shapetrees/project/shapetree-virtual#MilestoneTree"));
        assertNotNull(milestoneShapeTree1);

        // Retrieve the ProjectTree shapetree which will recursively cache the MilestoneTree shapetree
        ShapeTree projectShapeTree1 = ShapeTreeFactory.getShapeTree(toUrl(server,"/static/shapetrees/project/shapetree-virtual#ProjectTree"));
        assertNotNull(projectShapeTree1);

        // Retrieve the MilestoneTree shapetree again, ensuring the same instance is used
        ShapeTree milestoneShapeTree2 = ShapeTreeFactory.getShapeTree(toUrl(server,"/static/shapetrees/project/shapetree-virtual#MilestoneTree"));
        assertNotNull(milestoneShapeTree2);

        assertEquals(milestoneShapeTree1.hashCode(), milestoneShapeTree2.hashCode());
    }


    @SneakyThrows
    @Test
    @DisplayName("Parse Tree with references")
    void parseShapeTreeReferences() {
        ShapeTree projectShapeTree = ShapeTreeFactory.getShapeTree(toUrl(server,"/static/shapetrees/project/shapetree-virtual#ProjectTree"));
        assertNotNull(projectShapeTree);
        assertFalse(projectShapeTree.getReferences().isEmpty());
    }

    @SneakyThrows
    @Test
    @DisplayName("Parse Tree with contains")
    void parseShapeTreeContains() {
        ShapeTree projectShapeTree = ShapeTreeFactory.getShapeTree(toUrl(server,"/static/shapetrees/project/shapetree#ProjectTree"));
        assertNotNull(projectShapeTree);
        assertTrue(projectShapeTree.getContains().contains(toUrl(server,"/static/shapetrees/project/shapetree#MilestoneTree")));
    }

    @SneakyThrows
    @Test
    @DisplayName("Parse Tree that allows reserved resource types")
    void parseShapeTreeContainsReservedTypes() {
        ShapeTree reservedShapeTree = ShapeTreeFactory.getShapeTree(toUrl(server,"/static/shapetrees/reserved/shapetree#EverythingTree"));
        assertNotNull(reservedShapeTree);
        assertTrue(reservedShapeTree.getContains().contains(toUrl(server,"http://www.w3.org/ns/shapetrees#ResourceTree")));
        assertTrue(reservedShapeTree.getContains().contains(toUrl(server,"http://www.w3.org/ns/shapetrees#NonRDFResourceTree")));
        assertTrue(reservedShapeTree.getContains().contains(toUrl(server,"http://www.w3.org/ns/shapetrees#ContainerTree")));
    }

    @SneakyThrows
    @Test
    @DisplayName("Traverse References")
    void testTraverseReferences() {
        ShapeTree projectShapeTree = ShapeTreeFactory.getShapeTree(toUrl(server,"/static/shapetrees/project/shapetree-virtual#ProjectTree"));
        projectShapeTree.getReferencedShapeTrees();
        assertTrue(projectShapeTree.getReferencedShapeTrees(RecursionMethod.BREADTH_FIRST).hasNext());
        assertTrue(projectShapeTree.getReferencedShapeTrees(RecursionMethod.DEPTH_FIRST).hasNext());
    }

    @SneakyThrows
    @Test
    @DisplayName("Fail to parse shape tree with missing expectsType")
    void failToParseMissingExpectsType() {
        assertThrows(ShapeTreeException.class, () ->
            ShapeTreeFactory.getShapeTree(toUrl(server,"/static/shapetrees/invalid/missing-expects-type#DataRepositoryTree"))
        );
    }

    @SneakyThrows
    @Test
    @DisplayName("Fail to parse shape tree with st:contains but expects a non-container resource")
    void failToParseBadExpectsTypeOnContains() {
        assertThrows(ShapeTreeException.class, () ->
                ShapeTreeFactory.getShapeTree(toUrl(server,"/static/shapetrees/invalid/contains-with-bad-expects-type#DataRepositoryTree"))
        );
        assertThrows(ShapeTreeException.class, () ->
                ShapeTreeFactory.getShapeTree(toUrl(server,"/static/shapetrees/invalid/contains-with-nonrdf-expects-type#DataRepositoryTree"))
        );
    }

    @SneakyThrows
    @Test
    @DisplayName("Fail to parse shape tree with invalid object type")
    void failToParseBadObjectTypeOnContains() {
        assertThrows(ShapeTreeException.class, () ->
                ShapeTreeFactory.getShapeTree(toUrl(server,"/static/shapetrees/invalid/bad-object-type#DataRepositoryTree"))
        );
    }

    @SneakyThrows
    @Test
    @DisplayName("Fail to parse missing shape tree")
    void failToParseMissingShapeTree() {
        assertThrows(ShapeTreeException.class, () ->
                ShapeTreeFactory.getShapeTree(toUrl(server,"/static/shapetrees/invalid/shapetree-missing#missing"))
        );
    }

    @Test
    @DisplayName("Fail to parse shape tree with invalid content type")
    void failToParseShapeTreeWithInvalidContentType() {
        assertThrows(ShapeTreeException.class, () ->
                ShapeTreeFactory.getShapeTree(toUrl(server,"/static/shapetrees/project/shapetree-bad-content-type#bad"))
        );
    }

    @SneakyThrows
    @Test
    @DisplayName("Fail to parse shape tree with invalid contains objects")
    void failToParseInvalidContainsObjects() {
        assertThrows(ShapeTreeException.class, () ->
            ShapeTreeFactory.getShapeTree(toUrl(server,"/static/shapetrees/invalid/shapetree-invalid-contains-objects#DataRepositoryTree"))
        );
    }

    @SneakyThrows
    @Test
    @DisplayName("Parse st:contains across multiple documents")
    void parseContainsAcrossMultipleDocuments() {
        // Parse for recursive st:contains (use contains across multiple documents)
        ShapeTreeFactory.getShapeTree(toUrl(server,"/static/shapetrees/parsing/contains-1#1ATree"));

        // Check the shape tree cache to ensure every contains shape tree was visited, parsed, and cached
        assertEquals(11, ShapeTreeFactory.getLocalShapeTreeCache().size());
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/contains-1#1ATree"))));
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/contains-2#2ATree"))));
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/contains-2#2BTree"))));
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/contains-2#2CTree"))));
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/contains-2A#2A1Tree"))));
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/contains-2A#2A2Tree"))));
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/contains-2B#2B1Tree"))));
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/contains-2C#2C1Tree"))));
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/contains-2C#2C2Tree"))));
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/contains-2C#2C3Tree"))));
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/contains-2C2#2C2ATree"))));

        // Check the resource cache to ensure every visited resource was cached
        assertEquals(6, ShapeTreeResource.getLocalResourceCache().size());
        assertTrue(ShapeTreeResource.getLocalResourceCache().containsKey(urlToUri(toUrl(server, "/static/shapetrees/parsing/contains-1"))));
        assertTrue(ShapeTreeResource.getLocalResourceCache().containsKey(urlToUri(toUrl(server, "/static/shapetrees/parsing/contains-2"))));
        assertTrue(ShapeTreeResource.getLocalResourceCache().containsKey(urlToUri(toUrl(server, "/static/shapetrees/parsing/contains-2A"))));
        assertTrue(ShapeTreeResource.getLocalResourceCache().containsKey(urlToUri(toUrl(server, "/static/shapetrees/parsing/contains-2B"))));
        assertTrue(ShapeTreeResource.getLocalResourceCache().containsKey(urlToUri(toUrl(server, "/static/shapetrees/parsing/contains-2C2"))));
        assertTrue(ShapeTreeResource.getLocalResourceCache().containsKey(urlToUri(toUrl(server, "/static/shapetrees/parsing/contains-2C"))));
    }

    @SneakyThrows
    @Test
    @DisplayName("Parse st:references across multiple documents")
    void parseReferencesAcrossMultipleDocuments() {
        // Parse for recursive st:references (use references across multiple documents)
        ShapeTreeFactory.getShapeTree(toUrl(server,"/static/shapetrees/parsing/references-1#1ATree"));

        // Check the shape tree cache to ensure every referenced shape tree was visited, parsed, and cached
        assertEquals(11, ShapeTreeFactory.getLocalShapeTreeCache().size());
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/references-1#1ATree"))));
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/references-2#2ATree"))));
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/references-2#2BTree"))));
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/references-2#2CTree"))));
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/references-2A#2A1Tree"))));
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/references-2A#2A2Tree"))));
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/references-2B#2B1Tree"))));
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/references-2C#2C1Tree"))));
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/references-2C#2C2Tree"))));
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/references-2C#2C3Tree"))));
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/references-2C2#2C2ATree"))));

        // Check the resource cache to ensure every visited resource was cached
        assertEquals(6, ShapeTreeResource.getLocalResourceCache().size());
        assertTrue(ShapeTreeResource.getLocalResourceCache().containsKey(urlToUri(toUrl(server, "/static/shapetrees/parsing/references-1"))));
        assertTrue(ShapeTreeResource.getLocalResourceCache().containsKey(urlToUri(toUrl(server, "/static/shapetrees/parsing/references-2"))));
        assertTrue(ShapeTreeResource.getLocalResourceCache().containsKey(urlToUri(toUrl(server, "/static/shapetrees/parsing/references-2A"))));
        assertTrue(ShapeTreeResource.getLocalResourceCache().containsKey(urlToUri(toUrl(server, "/static/shapetrees/parsing/references-2B"))));
        assertTrue(ShapeTreeResource.getLocalResourceCache().containsKey(urlToUri(toUrl(server, "/static/shapetrees/parsing/references-2C2"))));
        assertTrue(ShapeTreeResource.getLocalResourceCache().containsKey(urlToUri(toUrl(server, "/static/shapetrees/parsing/references-2C"))));
    }

    @SneakyThrows
    @Test
    @DisplayName("Parse st:contains and st:references across multiple documents")
    void parseContainsAndReferencesAcrossMultipleDocuments() {
        // Parse for mix of st:contains and references
        ShapeTreeFactory.getShapeTree(toUrl(server,"/static/shapetrees/parsing/mixed-1#1ATree"));

        // Check the shape tree cache to ensure every referenced shape tree was visited, parsed, and cached
        assertEquals(13, ShapeTreeFactory.getLocalShapeTreeCache().size());
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/mixed-1#1ATree"))));
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/mixed-2#2ATree"))));
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/mixed-2#2BTree"))));
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/mixed-2#2CTree"))));
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/mixed-2#2DTree"))));
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/mixed-2A#2A1Tree"))));
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/mixed-2A#2A2Tree"))));
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/mixed-2B#2B1Tree"))));
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/mixed-2C#2C1Tree"))));
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/mixed-2C#2C2Tree"))));
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/mixed-2C#2C3Tree"))));
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/mixed-2C2#2C2ATree"))));
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/mixed-2D#2D1Tree"))));

        // Check the resource cache to ensure every visited resource was cached
        assertEquals(7, ShapeTreeResource.getLocalResourceCache().size());
        assertTrue(ShapeTreeResource.getLocalResourceCache().containsKey(urlToUri(toUrl(server, "/static/shapetrees/parsing/mixed-1"))));
        assertTrue(ShapeTreeResource.getLocalResourceCache().containsKey(urlToUri(toUrl(server, "/static/shapetrees/parsing/mixed-2"))));
        assertTrue(ShapeTreeResource.getLocalResourceCache().containsKey(urlToUri(toUrl(server, "/static/shapetrees/parsing/mixed-2A"))));
        assertTrue(ShapeTreeResource.getLocalResourceCache().containsKey(urlToUri(toUrl(server, "/static/shapetrees/parsing/mixed-2B"))));
        assertTrue(ShapeTreeResource.getLocalResourceCache().containsKey(urlToUri(toUrl(server, "/static/shapetrees/parsing/mixed-2C2"))));
        assertTrue(ShapeTreeResource.getLocalResourceCache().containsKey(urlToUri(toUrl(server, "/static/shapetrees/parsing/mixed-2C"))));
        assertTrue(ShapeTreeResource.getLocalResourceCache().containsKey(urlToUri(toUrl(server, "/static/shapetrees/parsing/mixed-2D"))));
    }

    @SneakyThrows
    @Test
    @DisplayName("Parse shape tree hierarchy with circular reference")
    void parseWithCircularReference() {
        // Ensure the parser correctly handles circular references
        ShapeTreeFactory.getShapeTree(toUrl(server,"/static/shapetrees/parsing/cycle#1ATree"));
        assertEquals(12, ShapeTreeFactory.getLocalShapeTreeCache().size());
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/cycle#1ATree"))));
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/cycle#2ATree"))));
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/cycle#2BTree"))));
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/cycle#2CTree"))));
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/cycle#2DTree"))));
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/cycle#2A1Tree"))));
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/cycle#2A2Tree"))));
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/cycle#2C1Tree"))));
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/cycle#2C2Tree"))));
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/cycle#2C3Tree"))));
        assertTrue(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/cycle#2D1Tree"))));
        assertFalse(ShapeTreeFactory.getLocalShapeTreeCache().containsKey(urlToUri(toUrl(server,"/static/shapetrees/parsing/cycle#2B1Tree"))));
    }

}