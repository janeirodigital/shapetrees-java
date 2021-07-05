package com.janeirodigital.shapetrees.client.okhttp;

import com.janeirodigital.shapetrees.client.okhttp.fixtures.DispatcherEntry;
import com.janeirodigital.shapetrees.client.okhttp.fixtures.RequestMatchingFixtureDispatcher;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.TestMethodOrder;

import java.util.List;

// TODO - Clean these tests up

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class ShapeTreeValidationTests extends BaseShapeTreeTest {

    public ShapeTreeValidationTests() {
        super();
    }

    private static RequestMatchingFixtureDispatcher dispatcher = null;

    @BeforeAll
    static void beforeAll() {
        dispatcher = new RequestMatchingFixtureDispatcher(List.of(
                new DispatcherEntry(List.of("shapetrees/medical-record-shapetree-ttl"), "GET", "/static/shapetrees/medical-record/shapetree", null),
                new DispatcherEntry(List.of("schemas/fhir-shex"), "GET", "/static/shex/fhir/r4/shex", null),
                new DispatcherEntry(List.of("errors/404"), "GET", "/static/shex/nonexisting", null),
                new DispatcherEntry(List.of("shapetrees/tree-invalid-schema"), "GET", "/static/shapetrees/medical-record-invalid/shapetree", null),
                new DispatcherEntry(List.of("schemas/invalid-shex"), "GET", "/static/shex/invalid", null),
                new DispatcherEntry(List.of("shapetrees/matching-shapetree-ttl"), "GET", "/static/shapetrees/matching/shapetree", null)
        ));
    }

    /*

    @Order(1)
    @SneakyThrows
    @Test
    @Label("Attempt validation with tree that does not validate")
    void testTreeWithNoValidation() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Get the medicalRecords shape tree.  Note this shape tree doesn't have a
        // validatedBy property, as such it should return an error when using to validate
        ShapeTree medicalRecordsShapeTree = ShapeTreeFactory.getShapeTree(getURI(server, "/static/shapetrees/medical-record/shapetree#medicalRecords"));
        String graphTtl = "<#a> <#b> <#c> .";
        StringReader sr = new StringReader(graphTtl);
        Model model = ModelFactory.createDefaultModel();
        RDFDataMgr.read(model, sr, "http://example.com/", Lang.TTL);

        Assertions.assertThrows(ShapeTreeException.class, () -> medicalRecordsShapeTree.validateResource(model.getGraph(), new URI("#a"), ShapeTreeResourceType.CONTAINER));
    }

    @Order(2)
    @SneakyThrows
    @Test
    @Label("Attempt validation with mismatching isContainer parameter")
    void testTreeWithIncorrectContainerFlag() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Get the medicalRecords shape tree.  Note this shape tree is marked
        // as a container - as such it is expected that the resource provided
        // pertains to a container resource
        ShapeTree medicalRecordsShapeTree = ShapeTreeFactory.getShapeTree(getURI(server, "/static/shapetrees/medical-record/shapetree#medicalRecords"));
        String graphTtl = "<#a> <#b> <#c> .";
        StringReader sr = new StringReader(graphTtl);
        Model model = ModelFactory.createDefaultModel();
        RDFDataMgr.read(model, sr, "http://example.com/", Lang.TTL);

        Assertions.assertThrows(ShapeTreeException.class, () -> medicalRecordsShapeTree.validateContent(model.getGraph(), new URI("#a"), ShapeTreeResourceType.RESOURCE));
    }

    @Order(3)
    @SneakyThrows
    @Test
    @Label("Validate a ShapeTree that has a non existing schema")
    void testTreeWithNonExistingSchema() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // This shapetree references a schema that will return a 404
        ShapeTree tree = ShapeTreeFactory.getShapeTree(getURI(server, "/static/shapetrees/medical-record-invalid/shapetree#medicalRecord"));
        String graphTtl = "<#a> <#b> <#c> .";
        StringReader sr = new StringReader(graphTtl);
        Model model = ModelFactory.createDefaultModel();
        RDFDataMgr.read(model, sr, "http://example.com/", Lang.TTL);

        Assertions.assertThrows(ShapeTreeException.class, () -> tree.validateContent(model.getGraph(), new URI("#a"), ShapeTreeResourceType.RESOURCE));
    }

    @Order(4)
    @SneakyThrows
    @Test
    @Label("Validate a ShapeTree that references an invalid schema")
    void testTreeWithInvalidSchema() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // This shapetree references a schema that will return a 404
        ShapeTree tree = ShapeTreeFactory.getShapeTree(getURI(server, "/static/shapetrees/medical-record-invalid/shapetree#medicalRecordInvalidSchema"));
        String graphTtl = "<#a> <#b> <#c> .";
        StringReader sr = new StringReader(graphTtl);
        Model model = ModelFactory.createDefaultModel();
        RDFDataMgr.read(model, sr, "http://example.com/", Lang.TTL);

        Assertions.assertThrows(ShapeTreeException.class, () -> tree.validateContent(model.getGraph(), new URI("#a"), ShapeTreeResourceType.RESOURCE));
    }


    @Order(5)
    @SneakyThrows
    @Test
    @Label("Validate ShapeTree in Schema not yet cached")
    void testInitializedCache() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        SchemaCache.initializeCache();

        ShapeTree tree = ShapeTreeFactory.getShapeTree(getURI(server, "/static/shapetrees/medical-record/shapetree#condition"));
        String graphTtl = MedicalRecordTests.getConditionTtl();
        StringReader sr = new StringReader(graphTtl);
        Model model = ModelFactory.createDefaultModel();
        RDFDataMgr.read(model, sr, "http://example.com/", Lang.TTL);

        ValidationResult result = tree.validateContent(model.getGraph(), new URI("http://hl7.org/fhir/Condition/example"), ShapeTreeResourceType.RESOURCE);
        assertTrue(result.getValid());
    }

    @Order(6)
    @SneakyThrows
    @Test
    @Label("Validate ShapeTree with schema cached")
    void testCachedSchema() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        Map<URI, ShexSchema> schemas = SchemaCacheTests.buildSchemaCache(List.of(getURI(server, "/static/shex/fhir/r4/shex").toString()));
        SchemaCache.initializeCache(schemas);

        ShapeTree tree = ShapeTreeFactory.getShapeTree(getURI(server, "/static/shapetrees/medical-record/shapetree#condition"));
        String graphTtl = MedicalRecordTests.getConditionTtl();
        StringReader sr = new StringReader(graphTtl);
        Model model = ModelFactory.createDefaultModel();
        RDFDataMgr.read(model, sr, "http://example.com/", Lang.TTL);

        ValidationResult result = tree.validateContent(model.getGraph(), new URI("http://hl7.org/fhir/Condition/example"), ShapeTreeResourceType.RESOURCE);
        assertTrue(result.getValid());
    }

    @Order(8)
    @SneakyThrows
    @Test
    @Label("Validate contains matching by ShapeTree hint")
    void testMatchingByHint() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        ShapeTree tree = ShapeTreeFactory.getShapeTree(getURI(server, "/static/shapetrees/matching/shapetree#containsByHint"));
        ShapeTree matchedTree = tree.findMatchingContainsShapeTree("anything", getURI(server, "/static/shapetrees/matching/shapetree#containedHint"), ShapeTreeResourceType.RESOURCE);
        assertEquals(matchedTree.getURI(), getURI(server, "/static/shapetrees/matching/shapetree#containedHint"));

        Assertions.assertThrows(ShapeTreeException.class, () -> tree.findMatchingContainsShapeTree("anythingelse", getURI(server, "/static/shapetrees/matching/shapetree#containedHintThatIsNotPresent"), ShapeTreeResourceType.RESOURCE));
    }

    @Order(9)
    @SneakyThrows
    @Test
    @Label("Validate No contains container")
    void testMatchingNoContainsContainer() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        ShapeTree tree = ShapeTreeFactory.getShapeTree(getURI(server, "/static/shapetrees/matching/shapetree#noContains"));
        ShapeTree matchedTree = tree.findMatchingContainsShapeTree("anything", null, ShapeTreeResourceType.RESOURCE);
        assertEquals(matchedTree, null);
    }

    @Order(10)
    @SneakyThrows
    @Test
    @Label("Validate No contains resource")
    void testMatchingNoContainsResource() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        ShapeTree tree = ShapeTreeFactory.getShapeTree(getURI(server, "/static/shapetrees/matching/shapetree#noContainsResource"));
        ShapeTree matchedTree = tree.findMatchingContainsShapeTree("anything", null, ShapeTreeResourceType.RESOURCE);
        assertEquals(matchedTree, tree);
    }

    @Order(11)
    @SneakyThrows
    @Test
    @Label("Validate AllowResources")
    void testMatchingNoMatchAllowResources() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        ShapeTree tree = ShapeTreeFactory.getShapeTree(getURI(server, "/static/shapetrees/matching/shapetree#containsAllowResource"));
        ShapeTree matchedTree = tree.findMatchingContainsShapeTree("anything", null, ShapeTreeResourceType.RESOURCE);
        assertEquals(null, matchedTree);

        Assertions.assertThrows(ShapeTreeException.class, () -> tree.findMatchingContainsShapeTree("anything", null, ShapeTreeResourceType.CONTAINER));
        Assertions.assertThrows(ShapeTreeException.class, () -> tree.findMatchingContainsShapeTree("anything", null, ShapeTreeResourceType.NON_RDF));
    }

    @Order(11)
    @SneakyThrows
    @Test
    @Label("Validate AllowContainers")
    void testMatchingNoMatchAllowContainers() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        ShapeTree tree = ShapeTreeFactory.getShapeTree(getURI(server, "/static/shapetrees/matching/shapetree#containsAllowContainer"));
        ShapeTree matchedTree = tree.findMatchingContainsShapeTree("anything", null, ShapeTreeResourceType.CONTAINER);
        assertEquals(null, matchedTree);

        Assertions.assertThrows(ShapeTreeException.class, () -> tree.findMatchingContainsShapeTree("anything", null, ShapeTreeResourceType.RESOURCE));
        Assertions.assertThrows(ShapeTreeException.class, () -> tree.findMatchingContainsShapeTree("anything", null, ShapeTreeResourceType.NON_RDF));
    }

    @Order(12)
    @SneakyThrows
    @Test
    @Label("Validate AllowNonRDFSources")
    void testMatchingNoMatchAllowNonRDFSources() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        ShapeTree tree = ShapeTreeFactory.getShapeTree(getURI(server, "/static/shapetrees/matching/shapetree#containsAllowNonRDF"));
        ShapeTree matchedTree = tree.findMatchingContainsShapeTree("anything", null, ShapeTreeResourceType.NON_RDF);
        assertEquals(null, matchedTree);

        Assertions.assertThrows(ShapeTreeException.class, () -> tree.findMatchingContainsShapeTree("anything", null, ShapeTreeResourceType.CONTAINER));
        Assertions.assertThrows(ShapeTreeException.class, () -> tree.findMatchingContainsShapeTree("anything", null, ShapeTreeResourceType.RESOURCE));
    }

    @Order(13)
    @SneakyThrows
    @Test
    @Label("Validate AllowOnly")
    void testMatchingNoMatchAllowOnly() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        ShapeTree tree = ShapeTreeFactory.getShapeTree(getURI(server, "/static/shapetrees/matching/shapetree#containsAllowOnly"));
        Assertions.assertThrows(ShapeTreeException.class, () -> tree.findMatchingContainsShapeTree("anything", null, ShapeTreeResourceType.RESOURCE));
        Assertions.assertThrows(ShapeTreeException.class, () -> tree.findMatchingContainsShapeTree("anything", null, ShapeTreeResourceType.CONTAINER));
        Assertions.assertThrows(ShapeTreeException.class, () -> tree.findMatchingContainsShapeTree("anything", null, ShapeTreeResourceType.NON_RDF));
    }

    @Order(14)
    @SneakyThrows
    @Test
    @Label("Validate No Allow* Predicates")
    void testMatchingNoMatchAllowUndefined() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        ShapeTree tree = ShapeTreeFactory.getShapeTree(getURI(server, "/static/shapetrees/matching/shapetree#containsAllowUndefined"));
        Assertions.assertThrows(ShapeTreeException.class, () -> tree.findMatchingContainsShapeTree("anything", null, ShapeTreeResourceType.RESOURCE));
        Assertions.assertThrows(ShapeTreeException.class, () -> tree.findMatchingContainsShapeTree("anything", null, ShapeTreeResourceType.CONTAINER));
        Assertions.assertThrows(ShapeTreeException.class, () -> tree.findMatchingContainsShapeTree("anything", null, ShapeTreeResourceType.NON_RDF));
    }

     */
}
