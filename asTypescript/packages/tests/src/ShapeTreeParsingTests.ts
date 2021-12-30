// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.tests
import { ShapeTreeFactory } from '@shapetrees/core/src/ShapeTreeFactory';
import { ShapeTreeResource } from '@shapetrees/core/src/ShapeTreeResource';
import { DocumentLoaderManager } from '@shapetrees/core/src/contentloaders/DocumentLoaderManager';
import { HttpExternalDocumentLoader } from '@shapetrees/core/src/contentloaders/HttpExternalDocumentLoader';
import { RecursionMethods } from '@shapetrees/core/src/enums/RecursionMethods';
import { ShapeTreeException } from '@shapetrees/core/src/exceptions/ShapeTreeException';
import { ShapeTree } from '@shapetrees/core/src/ShapeTree';
import { DispatcherEntry } from './fixtures/DispatcherEntry';
import { RequestMatchingFixtureDispatcher } from './fixtures/RequestMatchingFixtureDispatcher';
import * as MockWebServer from 'okhttp3/mockwebserver';
import { toUrl } from './fixtures/MockWebServerHelper/toUrl';

class ShapeTreeParsingTests {

   private static httpExternalDocumentLoader: HttpExternalDocumentLoader;

   private static dispatcher: RequestMatchingFixtureDispatcher = null;

   protected static server: MockWebServer = null;

  public constructor() {
    httpExternalDocumentLoader = new HttpExternalDocumentLoader();
    DocumentLoaderManager.setLoader(httpExternalDocumentLoader);
  }

  // @BeforeEach, @SneakyThrows
  beforeEach(): void {
    ShapeTreeFactory.clearCache();
    ShapeTreeResource.clearCache();
  }

  // @BeforeAll
  static beforeAll(): void {
    dispatcher = new RequestMatchingFixtureDispatcher(List.of(new DispatcherEntry(List.of("shapetrees/project-shapetree-ttl"), "GET", "/static/shapetrees/project/shapetree", null), new DispatcherEntry(List.of("shapetrees/business-shapetree-ttl"), "GET", "/static/shapetrees/business/shapetree", null), new DispatcherEntry(List.of("shapetrees/reserved-type-shapetree-ttl"), "GET", "/static/shapetrees/reserved/shapetree", null), new DispatcherEntry(List.of("shapetrees/project-shapetree-virtual-ttl"), "GET", "/static/shapetrees/project/shapetree-virtual", null), new DispatcherEntry(List.of("shapetrees/project-shapetree-invalid-ttl"), "GET", "/static/shapetrees/project/shapetree-invalid", null), new DispatcherEntry(List.of("shapetrees/project-shapetree-invalid-2-ttl"), "GET", "/static/shapetrees/project/shapetree-invalid2", null), new DispatcherEntry(List.of("shapetrees/content-type-invalid-shapetree-ttl"), "GET", "/static/shapetrees/project/shapetree-bad-content-type", null), new DispatcherEntry(List.of("shapetrees/missing-expects-type-shapetree-ttl"), "GET", "/static/shapetrees/invalid/missing-expects-type", null), new DispatcherEntry(List.of("shapetrees/contains-with-bad-expects-type-shapetree-ttl"), "GET", "/static/shapetrees/invalid/contains-with-bad-expects-type", null), new DispatcherEntry(List.of("shapetrees/bad-object-type-shapetree-ttl"), "GET", "/static/shapetrees/invalid/bad-object-type", null), new DispatcherEntry(List.of("shapetrees/invalid-contains-objects-shapetree-ttl"), "GET", "/static/shapetrees/invalid/shapetree-invalid-contains-objects", null), new DispatcherEntry(List.of("shapetrees/contains-with-nonrdf-expects-type-shapetree-ttl"), "GET", "/static/shapetrees/invalid/contains-with-nonrdf-expects-type", null), new DispatcherEntry(List.of("parsing/contains/contains-1-ttl"), "GET", "/static/shapetrees/parsing/contains-1", null), new DispatcherEntry(List.of("parsing/contains/contains-2-ttl"), "GET", "/static/shapetrees/parsing/contains-2", null), new DispatcherEntry(List.of("parsing/contains/contains-2A-ttl"), "GET", "/static/shapetrees/parsing/contains-2A", null), new DispatcherEntry(List.of("parsing/contains/contains-2B-ttl"), "GET", "/static/shapetrees/parsing/contains-2B", null), new DispatcherEntry(List.of("parsing/contains/contains-2C-ttl"), "GET", "/static/shapetrees/parsing/contains-2C", null), new DispatcherEntry(List.of("parsing/contains/contains-2C2-ttl"), "GET", "/static/shapetrees/parsing/contains-2C2", null), new DispatcherEntry(List.of("parsing/references/references-1-ttl"), "GET", "/static/shapetrees/parsing/references-1", null), new DispatcherEntry(List.of("parsing/references/references-2-ttl"), "GET", "/static/shapetrees/parsing/references-2", null), new DispatcherEntry(List.of("parsing/references/references-2A-ttl"), "GET", "/static/shapetrees/parsing/references-2A", null), new DispatcherEntry(List.of("parsing/references/references-2B-ttl"), "GET", "/static/shapetrees/parsing/references-2B", null), new DispatcherEntry(List.of("parsing/references/references-2C-ttl"), "GET", "/static/shapetrees/parsing/references-2C", null), new DispatcherEntry(List.of("parsing/references/references-2C2-ttl"), "GET", "/static/shapetrees/parsing/references-2C2", null), new DispatcherEntry(List.of("parsing/mixed/mixed-1-ttl"), "GET", "/static/shapetrees/parsing/mixed-1", null), new DispatcherEntry(List.of("parsing/mixed/mixed-2-ttl"), "GET", "/static/shapetrees/parsing/mixed-2", null), new DispatcherEntry(List.of("parsing/mixed/mixed-2A-ttl"), "GET", "/static/shapetrees/parsing/mixed-2A", null), new DispatcherEntry(List.of("parsing/mixed/mixed-2B-ttl"), "GET", "/static/shapetrees/parsing/mixed-2B", null), new DispatcherEntry(List.of("parsing/mixed/mixed-2C-ttl"), "GET", "/static/shapetrees/parsing/mixed-2C", null), new DispatcherEntry(List.of("parsing/mixed/mixed-2C2-ttl"), "GET", "/static/shapetrees/parsing/mixed-2C2", null), new DispatcherEntry(List.of("parsing/mixed/mixed-2D-ttl"), "GET", "/static/shapetrees/parsing/mixed-2D", null), new DispatcherEntry(List.of("parsing/cycle-ttl"), "GET", "/static/shapetrees/parsing/cycle", null), new DispatcherEntry(List.of("http/404"), "GET", "/static/shapetrees/invalid/shapetree-missing", null)));
    server = new MockWebServer();
    server.setDispatcher(dispatcher);
  }

  // @SneakyThrows, @Test, @DisplayName("Reuse previously cached shapetree")
  parseShapeTreeReuse(): void {
    let projectShapeTree1: ShapeTree = ShapeTreeFactory.getShapeTree(toUrl(server, "/static/shapetrees/project/shapetree#ProjectTree"));
    Assertions.assertNotNull(projectShapeTree1);
    let projectShapeTree2: ShapeTree = ShapeTreeFactory.getShapeTree(toUrl(server, "/static/shapetrees/project/shapetree#ProjectTree"));
    Assertions.assertNotNull(projectShapeTree2);
    assertEquals(projectShapeTree1.hashCode(), projectShapeTree2.hashCode());
    // The "business" shape tree won't be in the cache, but it cross-contains pm:MilestoneTree, which should be.
    let businessShapeTree: ShapeTree = ShapeTreeFactory.getShapeTree(toUrl(server, "/static/shapetrees/business/shapetree#BusinessTree"));
    Assertions.assertNotNull(businessShapeTree);
  }

  // @SneakyThrows, @Test, @DisplayName("Ensure reuse within recursion")
  ensureCacheWithRecursion(): void {
    // Retrieve the MilestoneTree shapetree (which is referred to by the ProjectTree shapetree)
    let milestoneShapeTree1: ShapeTree = ShapeTreeFactory.getShapeTree(toUrl(server, "/static/shapetrees/project/shapetree-virtual#MilestoneTree"));
    Assertions.assertNotNull(milestoneShapeTree1);
    // Retrieve the ProjectTree shapetree which will recursively cache the MilestoneTree shapetree
    let projectShapeTree1: ShapeTree = ShapeTreeFactory.getShapeTree(toUrl(server, "/static/shapetrees/project/shapetree-virtual#ProjectTree"));
    Assertions.assertNotNull(projectShapeTree1);
    // Retrieve the MilestoneTree shapetree again, ensuring the same instance is used
    let milestoneShapeTree2: ShapeTree = ShapeTreeFactory.getShapeTree(toUrl(server, "/static/shapetrees/project/shapetree-virtual#MilestoneTree"));
    Assertions.assertNotNull(milestoneShapeTree2);
    assertEquals(milestoneShapeTree1.hashCode(), milestoneShapeTree2.hashCode());
  }

  // @SneakyThrows, @Test, @DisplayName("Parse Tree with references")
  parseShapeTreeReferences(): void {
    let projectShapeTree: ShapeTree = ShapeTreeFactory.getShapeTree(toUrl(server, "/static/shapetrees/project/shapetree-virtual#ProjectTree"));
    Assertions.assertNotNull(projectShapeTree);
    assertFalse(projectShapeTree.getReferences().isEmpty());
  }

  // @SneakyThrows, @Test, @DisplayName("Parse Tree with contains")
  parseShapeTreeContains(): void {
    let projectShapeTree: ShapeTree = ShapeTreeFactory.getShapeTree(toUrl(server, "/static/shapetrees/project/shapetree#ProjectTree"));
    Assertions.assertNotNull(projectShapeTree);
    assertTrue(projectShapeTree.getContains().contains(toUrl(server, "/static/shapetrees/project/shapetree#MilestoneTree")));
  }

  // @SneakyThrows, @Test, @DisplayName("Parse Tree that allows reserved resource types")
  parseShapeTreeContainsReservedTypes(): void {
    let reservedShapeTree: ShapeTree = ShapeTreeFactory.getShapeTree(toUrl(server, "/static/shapetrees/reserved/shapetree#EverythingTree"));
    Assertions.assertNotNull(reservedShapeTree);
    assertTrue(reservedShapeTree.getContains().contains(toUrl(server, "http://www.w3.org/ns/shapetrees#ResourceTree")));
    assertTrue(reservedShapeTree.getContains().contains(toUrl(server, "http://www.w3.org/ns/shapetrees#NonRDFResourceTree")));
    assertTrue(reservedShapeTree.getContains().contains(toUrl(server, "http://www.w3.org/ns/shapetrees#ContainerTree")));
  }

  // @SneakyThrows, @Test, @DisplayName("Traverse References")
  testTraverseReferences(): void {
    let projectShapeTree: ShapeTree = ShapeTreeFactory.getShapeTree(toUrl(server, "/static/shapetrees/project/shapetree-virtual#ProjectTree"));
    projectShapeTree.getReferencedShapeTrees();
    Assertions.assertTrue(projectShapeTree.getReferencedShapeTrees(RecursionMethods.BREADTH_FIRST).hasNext());
    Assertions.assertTrue(projectShapeTree.getReferencedShapeTrees(RecursionMethods.DEPTH_FIRST).hasNext());
  }

  // @SneakyThrows, @Test, @DisplayName("Fail to parse shape tree with missing expectsType")
  failToParseMissingExpectsType(): void {
    Assertions.assertThrows(ShapeTreeException.class, () -> ShapeTreeFactory.getShapeTree(toUrl(server, "/static/shapetrees/invalid/missing-expects-type#DataRepositoryTree")));
  }

  // @SneakyThrows, @Test, @DisplayName("Fail to parse shape tree with st:contains but expects a non-container resource")
  failToParseBadExpectsTypeOnContains(): void {
    Assertions.assertThrows(ShapeTreeException.class, () -> ShapeTreeFactory.getShapeTree(toUrl(server, "/static/shapetrees/invalid/contains-with-bad-expects-type#DataRepositoryTree")));
    Assertions.assertThrows(ShapeTreeException.class, () -> ShapeTreeFactory.getShapeTree(toUrl(server, "/static/shapetrees/invalid/contains-with-nonrdf-expects-type#DataRepositoryTree")));
  }

  // @SneakyThrows, @Test, @DisplayName("Fail to parse shape tree with invalid object type")
  failToParseBadObjectTypeOnContains(): void {
    Assertions.assertThrows(ShapeTreeException.class, () -> ShapeTreeFactory.getShapeTree(toUrl(server, "/static/shapetrees/invalid/bad-object-type#DataRepositoryTree")));
  }

  // @SneakyThrows, @Test, @DisplayName("Fail to parse missing shape tree")
  failToParseMissingShapeTree(): void {
    Assertions.assertThrows(ShapeTreeException.class, () -> ShapeTreeFactory.getShapeTree(toUrl(server, "/static/shapetrees/invalid/shapetree-missing#missing")));
  }

  // @Test, @DisplayName("Fail to parse shape tree with invalid content type")
  failToParseShapeTreeWithInvalidContentType(): void {
    Assertions.assertThrows(ShapeTreeException.class, () -> ShapeTreeFactory.getShapeTree(toUrl(server, "/static/shapetrees/project/shapetree-bad-content-type#bad")));
  }

  // @SneakyThrows, @Test, @DisplayName("Fail to parse shape tree with invalid contains objects")
  failToParseInvalidContainsObjects(): void {
    Assertions.assertThrows(ShapeTreeException.class, () -> ShapeTreeFactory.getShapeTree(toUrl(server, "/static/shapetrees/invalid/shapetree-invalid-contains-objects#DataRepositoryTree")));
  }

  // @SneakyThrows, @Test, @DisplayName("Parse st:contains across multiple documents")
  parseContainsAcrossMultipleDocuments(): void {
    // Parse for recursive st:contains (use contains across multiple documents)
    let containsShapeTree: ShapeTree = ShapeTreeFactory.getShapeTree(toUrl(server, "/static/shapetrees/parsing/contains-1#1ATree"));
    // Check the shape tree cache to ensure every contains shape tree was visited, parsed, and cached
    Assertions.assertEquals(11, ShapeTreeFactory.getLocalShapeTreeCache().size());
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/contains-1#1ATree"));
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/contains-2#2ATree"));
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/contains-2#2BTree"));
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/contains-2#2CTree"));
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/contains-2A#2A1Tree"));
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/contains-2A#2A2Tree"));
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/contains-2B#2B1Tree"));
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/contains-2C#2C1Tree"));
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/contains-2C#2C2Tree"));
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/contains-2C#2C3Tree"));
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/contains-2C2#2C21Tree"));
    // Check the resource cache to ensure every visited resource was cached
    Assertions.assertEquals(6, ShapeTreeResource.getLocalResourceCache().size());
    ShapeTreeResource.getLocalResourceCache().containsKey(toUrl(server, "/static/shapetrees/parsing/contains-1"));
    ShapeTreeResource.getLocalResourceCache().containsKey(toUrl(server, "/static/shapetrees/parsing/contains-2"));
    ShapeTreeResource.getLocalResourceCache().containsKey(toUrl(server, "/static/shapetrees/parsing/contains-2A"));
    ShapeTreeResource.getLocalResourceCache().containsKey(toUrl(server, "/static/shapetrees/parsing/contains-2B"));
    ShapeTreeResource.getLocalResourceCache().containsKey(toUrl(server, "/static/shapetrees/parsing/contains-2C2"));
    ShapeTreeResource.getLocalResourceCache().containsKey(toUrl(server, "/static/shapetrees/parsing/contains-2C"));
  }

  // @SneakyThrows, @Test, @DisplayName("Parse st:references across multiple documents")
  parseReferencesAcrossMultipleDocuments(): void {
    // Parse for recursive st:references (use references across multiple documents)
    let referencesShapeTree: ShapeTree = ShapeTreeFactory.getShapeTree(toUrl(server, "/static/shapetrees/parsing/references-1#1ATree"));
    // Check the shape tree cache to ensure every referenced shape tree was visited, parsed, and cached
    Assertions.assertEquals(11, ShapeTreeFactory.getLocalShapeTreeCache().size());
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/references-1#1ATree"));
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/references-2#2ATree"));
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/references-2#2BTree"));
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/references-2#2CTree"));
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/references-2A#2A1Tree"));
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/references-2A#2A2Tree"));
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/references-2B#2B1Tree"));
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/references-2C#2C1Tree"));
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/references-2C#2C2Tree"));
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/references-2C#2C3Tree"));
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/references-2C2#2C21Tree"));
    // Check the resource cache to ensure every visited resource was cached
    Assertions.assertEquals(6, ShapeTreeResource.getLocalResourceCache().size());
    ShapeTreeResource.getLocalResourceCache().containsKey(toUrl(server, "/static/shapetrees/parsing/references-1"));
    ShapeTreeResource.getLocalResourceCache().containsKey(toUrl(server, "/static/shapetrees/parsing/references-2"));
    ShapeTreeResource.getLocalResourceCache().containsKey(toUrl(server, "/static/shapetrees/parsing/references-2A"));
    ShapeTreeResource.getLocalResourceCache().containsKey(toUrl(server, "/static/shapetrees/parsing/references-2B"));
    ShapeTreeResource.getLocalResourceCache().containsKey(toUrl(server, "/static/shapetrees/parsing/references-2C2"));
    ShapeTreeResource.getLocalResourceCache().containsKey(toUrl(server, "/static/shapetrees/parsing/references-2C"));
  }

  // @SneakyThrows, @Test, @DisplayName("Parse st:contains and st:references across multiple documents")
  parseContainsAndReferencesAcrossMultipleDocuments(): void {
    // Parse for mix of st:contains and references
    let referencesShapeTree: ShapeTree = ShapeTreeFactory.getShapeTree(toUrl(server, "/static/shapetrees/parsing/mixed-1#1ATree"));
    // Check the shape tree cache to ensure every referenced shape tree was visited, parsed, and cached
    Assertions.assertEquals(13, ShapeTreeFactory.getLocalShapeTreeCache().size());
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/mixed-1#1ATree"));
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/mixed-2#2ATree"));
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/mixed-2#2BTree"));
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/mixed-2#2CTree"));
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/mixed-2#2DTree"));
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/mixed-2A#2A1Tree"));
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/mixed-2A#2A2Tree"));
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/mixed-2B#2B1Tree"));
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/mixed-2C#2C1Tree"));
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/mixed-2C#2C2Tree"));
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/mixed-2C#2C3Tree"));
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/mixed-2C2#2C21Tree"));
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/mixed-2D#2D1Tree"));
    // Check the resource cache to ensure every visited resource was cached
    Assertions.assertEquals(7, ShapeTreeResource.getLocalResourceCache().size());
    ShapeTreeResource.getLocalResourceCache().containsKey(toUrl(server, "/static/shapetrees/parsing/mixed-1"));
    ShapeTreeResource.getLocalResourceCache().containsKey(toUrl(server, "/static/shapetrees/parsing/mixed-2"));
    ShapeTreeResource.getLocalResourceCache().containsKey(toUrl(server, "/static/shapetrees/parsing/mixed-2A"));
    ShapeTreeResource.getLocalResourceCache().containsKey(toUrl(server, "/static/shapetrees/parsing/mixed-2B"));
    ShapeTreeResource.getLocalResourceCache().containsKey(toUrl(server, "/static/shapetrees/parsing/mixed-2C2"));
    ShapeTreeResource.getLocalResourceCache().containsKey(toUrl(server, "/static/shapetrees/parsing/mixed-2C"));
    ShapeTreeResource.getLocalResourceCache().containsKey(toUrl(server, "/static/shapetrees/parsing/mixed-2D"));
  }

  // @SneakyThrows, @Test, @DisplayName("Parse shape tree hierarchy with circular reference")
  parseWithCircularReference(): void {
    // Ensure the parser correctly handles circular references
    let circularShapeTree: ShapeTree = ShapeTreeFactory.getShapeTree(toUrl(server, "/static/shapetrees/parsing/cycle#1ATree"));
    Assertions.assertEquals(12, ShapeTreeFactory.getLocalShapeTreeCache().size());
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/mixed-1#1ATree"));
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/mixed-2#2ATree"));
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/mixed-2#2BTree"));
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/mixed-2#2CTree"));
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/mixed-2#2DTree"));
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/mixed-2A#2A1Tree"));
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/mixed-2A#2A2Tree"));
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/mixed-2B#2B1Tree"));
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/mixed-2C#2C1Tree"));
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/mixed-2C#2C2Tree"));
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/mixed-2C#2C3Tree"));
    ShapeTreeFactory.getLocalShapeTreeCache().containsKey(toUrl(server, "/static/shapetrees/parsing/mixed-2D#2D1Tree"));
  }
}
