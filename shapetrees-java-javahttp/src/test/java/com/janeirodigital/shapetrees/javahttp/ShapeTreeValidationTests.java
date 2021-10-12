package com.janeirodigital.shapetrees.javahttp;

import com.janeirodigital.shapetrees.client.http.HttpRemoteResourceAccessor;
import com.janeirodigital.shapetrees.core.ResourceAccessor;
import com.janeirodigital.shapetrees.core.SchemaCache;
import com.janeirodigital.shapetrees.core.ShapeTreeFactory;
import com.janeirodigital.shapetrees.core.ShapeTreeResource;
import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.helpers.GraphHelper;
import com.janeirodigital.shapetrees.core.models.ShapeTree;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;
import com.janeirodigital.shapetrees.core.models.ValidationResult;
import com.janeirodigital.shapetrees.javahttp.fixtures.DispatcherEntry;
import com.janeirodigital.shapetrees.javahttp.fixtures.RequestMatchingFixtureDispatcher;
import fr.inria.lille.shexjava.schema.ShexSchema;
import jdk.jfr.Label;
import lombok.SneakyThrows;
import okhttp3.mockwebserver.MockWebServer;
import org.apache.jena.graph.Graph;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.io.StringReader;
import java.net.URI;
import java.util.List;
import java.util.Map;

class ShapeTreeValidationTests extends BaseShapeTreeTest {

    public ShapeTreeValidationTests() {
        super();
    }

    private static RequestMatchingFixtureDispatcher dispatcher = null;
    private static ResourceAccessor resourceAccessor;

    @BeforeAll
    static void beforeAll() {

        resourceAccessor = new HttpRemoteResourceAccessor();

        dispatcher = new RequestMatchingFixtureDispatcher(List.of(
                new DispatcherEntry(List.of("shapetrees/validation-shapetree-ttl"), "GET", "/static/shapetrees/validation/shapetree", null),
                new DispatcherEntry(List.of("validation/validation-container"), "GET", "/validation/", null),
                new DispatcherEntry(List.of("validation/valid-resource"), "GET", "/validation/valid-resource", null),
                new DispatcherEntry(List.of("http/404"), "GET", "/static/shex/missing", null),
                new DispatcherEntry(List.of("schemas/validation-shex"), "GET", "/static/shex/validation", null),
                new DispatcherEntry(List.of("schemas/invalid-shex"), "GET", "/static/shex/invalid", null)));
    }

    @SneakyThrows
    @Test
    @Label("Validate expectsType of Container")
    void validateExpectsContainerType() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ValidationResult result;

        ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(getURI(server, "/static/shapetrees/validation/shapetree#ExpectsContainerTree"));
        result = shapeTree.validateResource(null, ShapeTreeResourceType.CONTAINER, null, null);
        Assertions.assertTrue(result.isValid());

        result = shapeTree.validateResource(null, ShapeTreeResourceType.RESOURCE, null, null);
        Assertions.assertFalse(result.isValid());

        result = shapeTree.validateResource(null, ShapeTreeResourceType.NON_RDF, null, null);
        Assertions.assertFalse(result.isValid());
    }

    @SneakyThrows
    @Test
    @Label("Validate expectsType of Resource")
    void validateExpectsResourceType() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ValidationResult result;

        ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(getURI(server, "/static/shapetrees/validation/shapetree#ExpectsResourceTree"));
        result = shapeTree.validateResource(null, ShapeTreeResourceType.RESOURCE, null, null);
        Assertions.assertTrue(result.isValid());

        result = shapeTree.validateResource(null, ShapeTreeResourceType.CONTAINER, null, null);
        Assertions.assertFalse(result.isValid());

        result = shapeTree.validateResource(null, ShapeTreeResourceType.NON_RDF, null, null);
        Assertions.assertFalse(result.isValid());
    }

    @SneakyThrows
    @Test
    @Label("Validate expectsType of NonRDFResource")
    void validateExpectsNonRDFResourceType() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ValidationResult result;

        ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(getURI(server, "/static/shapetrees/validation/shapetree#ExpectsNonRDFResourceTree"));
        result = shapeTree.validateResource(null, ShapeTreeResourceType.NON_RDF, null, null);
        Assertions.assertTrue(result.isValid());

        result = shapeTree.validateResource(null, ShapeTreeResourceType.RESOURCE, null, null);
        Assertions.assertFalse(result.isValid());

        result = shapeTree.validateResource(null, ShapeTreeResourceType.CONTAINER, null, null);
        Assertions.assertFalse(result.isValid());
    }

    @SneakyThrows
    @Test
    @Label("Validate label")
    void validateLabel() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ValidationResult result;

        ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(getURI(server, "/static/shapetrees/validation/shapetree#LabelTree"));
        result = shapeTree.validateResource("resource-name", ShapeTreeResourceType.RESOURCE, null, null);
        Assertions.assertTrue(result.isValid());

        result = shapeTree.validateResource("invalid-name", ShapeTreeResourceType.RESOURCE, null, null);
        Assertions.assertFalse(result.isValid());
    }

    @SneakyThrows
    @Test
    @Label("Validate shape")
    void validateShape() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ValidationResult result;

        ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(getURI(server, "/static/shapetrees/validation/shapetree#FooTree"));

        // Validate shape with focus node
        result = shapeTree.validateResource(null, ShapeTreeResourceType.RESOURCE, getFooBodyGraph(getURI(server, "/validation/valid-resource")), getURI(server, "/validation/valid-resource#foo"));
        Assertions.assertTrue(result.isValid());

        // Validate shape without focus node
        result = shapeTree.validateResource(null, ShapeTreeResourceType.RESOURCE, getFooBodyGraph(getURI(server, "/validation/valid-resource")), null);
        Assertions.assertTrue(result.isValid());

    }

    @SneakyThrows
    @Test
    @Label("Fail to validate shape")
    void failToValidateShape() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ValidationResult result;

        ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(getURI(server, "/static/shapetrees/validation/shapetree#FooTree"));

        // Pass in body content that will fail validation of the shape associated with FooTree
        result = shapeTree.validateResource(null, ShapeTreeResourceType.RESOURCE, getInvalidFooBodyGraph(getURI(server, "/validation/valid-resource")), getURI(server, "/validation/valid-resource#foo"));
        Assertions.assertFalse(result.isValid());

    }

    @SneakyThrows
    @Test
    @Label("Fail to validate shape when the shape resource cannot be found")
    void failToValidateMissingShape() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ValidationResult result;

        ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(getURI(server, "/static/shapetrees/validation/shapetree#MissingShapeSchemaTree"));

        // Catch exception thrown when a shape in a shape tree cannot be found
        Assertions.assertThrows(ShapeTreeException.class, () -> shapeTree.validateResource(null, ShapeTreeResourceType.RESOURCE, getFooBodyGraph(getURI(server, "/validation/valid-resource")), getURI(server, "/validation/valid-resource#foo")));

    }

    @SneakyThrows
    @Test
    @Label("Fail to validate shape when the shape resource is malformed")
    void failToValidateMalformedShape() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ValidationResult result;

        ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(getURI(server, "/static/shapetrees/validation/shapetree#InvalidShapeSchemaTree"));

        // Catch exception thrown when a shape in a shape tree is invalid
        Assertions.assertThrows(ShapeTreeException.class, () -> shapeTree.validateResource(null, ShapeTreeResourceType.RESOURCE, getFooBodyGraph(getURI(server, "/validation/valid-resource")), getURI(server, "/validation/valid-resource#foo")));

    }

    @SneakyThrows
    @Test
    @Label("Fail shape validation when shape tree doesn't validate a shape")
    void failToValidateWhenNoShapeInShapeTree() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Get the NoShapeValidationTree shape tree. This shape tree doesn't enforce shape validation,
        // so it should return an error when using to validate
        ShapeTree noShapeValidationTree = ShapeTreeFactory.getShapeTree(getURI(server, "/static/shapetrees/validation/shapetree#NoShapeValidationTree"));
        String graphTtl = "<#a> <#b> <#c> .";
        StringReader sr = new StringReader(graphTtl);
        Model model = ModelFactory.createDefaultModel();
        RDFDataMgr.read(model, sr, "http://example.com/", Lang.TTL);

        Assertions.assertThrows(ShapeTreeException.class, () -> noShapeValidationTree.validateGraph(model.getGraph(), new URI("#a")));
    }

    @SneakyThrows
    @Test
    @Label("Validate shape before it is cached in schema cache")
    void validateShapeBeforeCaching() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ValidationResult result;

        SchemaCache.initializeCache();

        ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(getURI(server, "/static/shapetrees/validation/shapetree#FooTree"));

        // Validate shape with focus node
        result = shapeTree.validateResource(null, ShapeTreeResourceType.RESOURCE, getFooBodyGraph(getURI(server, "/validation/valid-resource")), getURI(server, "/validation/valid-resource#foo"));
        Assertions.assertTrue(result.isValid());

    }

    @SneakyThrows
    @Test
    @Label("Validate shape after it is cached in schema cache")
    void validateShapeAfterCaching() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ValidationResult result;

        Map<URI, ShexSchema> schemas = SchemaCacheTests.buildSchemaCache(List.of(getURI(server, "/static/shex/validation").toString()));
        SchemaCache.initializeCache(schemas);

        ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(getURI(server, "/static/shapetrees/validation/shapetree#FooTree"));

        // Validate shape with focus node
        result = shapeTree.validateResource(null, ShapeTreeResourceType.RESOURCE, getFooBodyGraph(getURI(server, "/validation/valid-resource")), getURI(server, "/validation/valid-resource#foo"));
        Assertions.assertTrue(result.isValid());

    }
/*
    @SneakyThrows
    @Test
    @Label("Validate contained resource at various input stages")
    void validateCascadingContainedResource() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ValidationResult result;

        ShapeTreeContext context = new ShapeTreeContext("null");
        ShapeTreeResource primaryResource = resourceAccessor.getResource(context, getURI(server, "/validation/valid-resource"));
        ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(getURI(server, "/static/shapetrees/validation/shapetree#FooContainingTree"));

        result = shapeTree.validateContainedResource(primaryResource);
        Assertions.assertTrue(result.isValid());

        result = shapeTree.validateContainedResource(primaryResource, getURI(server, "/static/shapetrees/validation/shapetree#FooTree"), getURI(server, "/validation/valid-resource#foo"));
        Assertions.assertTrue(result.isValid());

        result = shapeTree.validateContainedResource(primaryResource, getURI(server, "/static/shapetrees/validation/shapetree#FooTree"), getURI(server, "/validation/valid-resource#foo"));
        Assertions.assertTrue(result.isValid());

        // With target shape tree and focus node
        result = shapeTree.validateContainedResource("valid-resource", ShapeTreeResourceType.RESOURCE, getURI(server, "/static/shapetrees/validation/shapetree#FooTree"), getFooBodyGraph(getURI(server, "/validation/valid-resource#foo")), getURI(server, "/validation/valid-resource#foo"));
        Assertions.assertTrue(result.isValid());

        // With target shape tree / without focus node
        result = shapeTree.validateContainedResource("valid-resource", ShapeTreeResourceType.RESOURCE, getURI(server, "/static/shapetrees/validation/shapetree#FooTree"), getFooBodyGraph(getURI(server, "/validation/valid-resource#foo")), null);
        Assertions.assertTrue(result.isValid());

        // Without target shape tree / with focus node
        result = shapeTree.validateContainedResource("valid-resource", ShapeTreeResourceType.RESOURCE, null, getFooBodyGraph(getURI(server, "/validation/valid-resource#foo")), getURI(server, "/validation/valid-resource#foo"));
        Assertions.assertTrue(result.isValid());

    }
*/
    private Graph getFooBodyGraph(URI baseURI) throws ShapeTreeException {
        String body = "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \n" +
                      "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \n" +
                      "PREFIX xml: <http://www.w3.org/XML/1998/namespace> \n" +
                      "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> \n" +
                      "PREFIX ex: <http://www.example.com/ns/ex#> \n" +
                      "<#foo> \n" +
                      "    ex:id 56789 ; \n" +
                      "    ex:name \"Footastic\" ; \n" +
                      "    ex:created_at \"2021-04-04T20:15:47.000Z\"^^xsd:dateTime . \n";

        return GraphHelper.readStringIntoGraph(baseURI, body, TEXT_TURTLE);
    }

    private Graph getInvalidFooBodyGraph(URI baseURI) throws ShapeTreeException {
        String body = "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \n" +
                "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \n" +
                "PREFIX xml: <http://www.w3.org/XML/1998/namespace> \n" +
                "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> \n" +
                "PREFIX ex: <http://www.example.com/ns/ex#> \n" +
                "<#foo> \n" +
                "    ex:id 56789 ; \n" +
                "    ex:created_at \"2021-04-04T20:15:47.000Z\"^^xsd:dateTime . \n";

        return GraphHelper.readStringIntoGraph(baseURI, body, TEXT_TURTLE);
    }


}
