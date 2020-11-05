package com.janeirodigital.shapetrees.test;

import com.janeirodigital.shapetrees.SchemaCache;
import com.janeirodigital.shapetrees.ShapeTreeException;
import com.janeirodigital.shapetrees.ShapeTreeFactory;
import com.janeirodigital.shapetrees.model.ShapeTree;
import com.janeirodigital.shapetrees.model.ValidationResult;
import com.janeirodigital.shapetrees.test.fixtures.DispatcherEntry;
import com.janeirodigital.shapetrees.test.fixtures.RequestMatchingFixtureDispatcher;
import fr.inria.lille.shexjava.schema.ShexSchema;
import jdk.jfr.Label;
import lombok.SneakyThrows;
import okhttp3.mockwebserver.MockWebServer;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;
import org.junit.jupiter.api.*;

import java.io.StringReader;
import java.net.URI;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class ShapeTreeValidationTests extends BaseShapeTreeTest {

    public ShapeTreeValidationTests() {
        super(new MockEcosystem());
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

        assertThrows(ShapeTreeException.class, () -> medicalRecordsShapeTree.validateContent(model.getGraph(), new URI("#a"), true));
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

        assertThrows(ShapeTreeException.class, () -> medicalRecordsShapeTree.validateContent(model.getGraph(), new URI("#a"), false));
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

        assertThrows(ShapeTreeException.class, () -> tree.validateContent(model.getGraph(), new URI("#a"), false));
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

        assertThrows(ShapeTreeException.class, () -> tree.validateContent(model.getGraph(), new URI("#a"), false));
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

        ValidationResult result = tree.validateContent(model.getGraph(), new URI("http://hl7.org/fhir/Condition/example"), false);
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

        ValidationResult result = tree.validateContent(model.getGraph(), new URI("http://hl7.org/fhir/Condition/example"), false);
        assertTrue(result.getValid());
    }

    @Order(7)
    @SneakyThrows
    @Test
    @Label("Validate contains matching by URI Template")
    void testMatchingByURITemplate() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        ShapeTree tree = ShapeTreeFactory.getShapeTree(getURI(server, "/static/shapetrees/matching/shapetree#containsMultipleByTemplate"));
        ShapeTree matchedTree = tree.findMatchingContainsShapeTree("a-test", null, false, false);
        assertEquals(matchedTree.getURI(), getURI(server, "/static/shapetrees/matching/shapetree#containedTemplateA"));

        // Test again with a trailing slash on the requested name
        matchedTree = tree.findMatchingContainsShapeTree("a-test/", null, false, false);
        assertEquals(matchedTree.getURI(), getURI(server, "/static/shapetrees/matching/shapetree#containedTemplateA"));


        matchedTree = tree.findMatchingContainsShapeTree("b-test", null, false, false);
        assertEquals(matchedTree.getURI(), getURI(server, "/static/shapetrees/matching/shapetree#containedTemplateB"));

        matchedTree = tree.findMatchingContainsShapeTree("c-test", null, false, false);
        assertEquals(matchedTree.getURI(), getURI(server, "/static/shapetrees/matching/shapetree#containedTemplateC"));

        assertThrows(ShapeTreeException.class, () -> tree.findMatchingContainsShapeTree("d-test", null, false, false));

        ShapeTree dupeATree = ShapeTreeFactory.getShapeTree(getURI(server, "/static/shapetrees/matching/shapetree#containsTemplateDuplication"));
        assertThrows(ShapeTreeException.class, () -> dupeATree.findMatchingContainsShapeTree("a-test", null, false, false));
    }

    @Order(8)
    @SneakyThrows
    @Test
    @Label("Validate contains matching by ShapeTree hint")
    void testMatchingByHint() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        ShapeTree tree = ShapeTreeFactory.getShapeTree(getURI(server, "/static/shapetrees/matching/shapetree#containsByHint"));
        ShapeTree matchedTree = tree.findMatchingContainsShapeTree("anything", getURI(server, "/static/shapetrees/matching/shapetree#containedHint"), false, false);
        assertEquals(matchedTree.getURI(), getURI(server, "/static/shapetrees/matching/shapetree#containedHint"));

        assertThrows(ShapeTreeException.class, () -> tree.findMatchingContainsShapeTree("anythingelse", getURI(server, "/static/shapetrees/matching/shapetree#containedHintThatIsNotPresent"), false, false));
    }

    @Order(9)
    @SneakyThrows
    @Test
    @Label("Validate No contains container")
    void testMatchingNoContainsContainer() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        ShapeTree tree = ShapeTreeFactory.getShapeTree(getURI(server, "/static/shapetrees/matching/shapetree#noContains"));
        ShapeTree matchedTree = tree.findMatchingContainsShapeTree("anything", null, false, false);
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
        ShapeTree matchedTree = tree.findMatchingContainsShapeTree("anything", null, false, false);
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
        ShapeTree matchedTree = tree.findMatchingContainsShapeTree("anything", null, false, false);
        assertEquals(null, matchedTree);

        assertThrows(ShapeTreeException.class, () -> tree.findMatchingContainsShapeTree("anything", null, true, false));
        assertThrows(ShapeTreeException.class, () -> tree.findMatchingContainsShapeTree("anything", null, false, true));
    }

    @Order(11)
    @SneakyThrows
    @Test
    @Label("Validate AllowContainers")
    void testMatchingNoMatchAllowContainers() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        ShapeTree tree = ShapeTreeFactory.getShapeTree(getURI(server, "/static/shapetrees/matching/shapetree#containsAllowContainer"));
        ShapeTree matchedTree = tree.findMatchingContainsShapeTree("anything", null, true, false);
        assertEquals(null, matchedTree);

        assertThrows(ShapeTreeException.class, () -> tree.findMatchingContainsShapeTree("anything", null, false, false));
        assertThrows(ShapeTreeException.class, () -> tree.findMatchingContainsShapeTree("anything", null, false, true));
    }

    @Order(12)
    @SneakyThrows
    @Test
    @Label("Validate AllowNonRDFSources")
    void testMatchingNoMatchAllowNonRDFSources() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        ShapeTree tree = ShapeTreeFactory.getShapeTree(getURI(server, "/static/shapetrees/matching/shapetree#containsAllowNonRDF"));
        ShapeTree matchedTree = tree.findMatchingContainsShapeTree("anything", null, false, true);
        assertEquals(null, matchedTree);

        assertThrows(ShapeTreeException.class, () -> tree.findMatchingContainsShapeTree("anything", null, true, false));
        assertThrows(ShapeTreeException.class, () -> tree.findMatchingContainsShapeTree("anything", null, false, false));
    }

    @Order(13)
    @SneakyThrows
    @Test
    @Label("Validate AllowNone")
    void testMatchingNoMatchAllowNone() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        ShapeTree tree = ShapeTreeFactory.getShapeTree(getURI(server, "/static/shapetrees/matching/shapetree#containsAllowNone"));
        assertThrows(ShapeTreeException.class, () -> tree.findMatchingContainsShapeTree("anything", null, false, false));
        assertThrows(ShapeTreeException.class, () -> tree.findMatchingContainsShapeTree("anything", null, true, false));
        assertThrows(ShapeTreeException.class, () -> tree.findMatchingContainsShapeTree("anything", null, false, true));
    }

    @Order(14)
    @SneakyThrows
    @Test
    @Label("Validate No Allow* Predicates")
    void testMatchingNoMatchAllowUndefined() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        ShapeTree tree = ShapeTreeFactory.getShapeTree(getURI(server, "/static/shapetrees/matching/shapetree#containsAllowUndefined"));
        assertThrows(ShapeTreeException.class, () -> tree.findMatchingContainsShapeTree("anything", null, false, false));
        assertThrows(ShapeTreeException.class, () -> tree.findMatchingContainsShapeTree("anything", null, true, false));
        assertThrows(ShapeTreeException.class, () -> tree.findMatchingContainsShapeTree("anything", null, false, true));
    }
}
