package com.janeirodigital.shapetrees.tests;

import com.janeirodigital.shapetrees.core.SchemaCache;
import com.janeirodigital.shapetrees.core.ShapeTree;
import com.janeirodigital.shapetrees.core.ShapeTreeFactory;
import com.janeirodigital.shapetrees.core.ValidationResult;
import com.janeirodigital.shapetrees.core.contentloaders.DocumentLoaderManager;
import com.janeirodigital.shapetrees.core.contentloaders.HttpExternalDocumentLoader;
import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.helpers.GraphHelper;
import com.janeirodigital.shapetrees.tests.fixtures.DispatcherEntry;
import com.janeirodigital.shapetrees.tests.fixtures.MockWebServerHelper;
import com.janeirodigital.shapetrees.tests.fixtures.RequestMatchingFixtureDispatcher;
import fr.inria.lille.shexjava.schema.ShexSchema;
import lombok.SneakyThrows;
import okhttp3.mockwebserver.MockWebServer;
import org.apache.jena.graph.Graph;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.io.StringReader;
import java.net.URL;
import java.util.List;
import java.util.Map;

class ShapeTreeValidationTests {

    private static RequestMatchingFixtureDispatcher dispatcher = null;
    private static HttpExternalDocumentLoader httpExternalDocumentLoader;

    public ShapeTreeValidationTests() {
        httpExternalDocumentLoader = new HttpExternalDocumentLoader();
        DocumentLoaderManager.setLoader(httpExternalDocumentLoader);
    }

    @BeforeAll
    static void beforeAll() {

        dispatcher = new RequestMatchingFixtureDispatcher(List.of(
                new DispatcherEntry(List.of("shapetrees/validation-shapetree-ttl"), "GET", "/static/shapetrees/validation/shapetree", null),
                new DispatcherEntry(List.of("shapetrees/containment-shapetree-ttl"), "GET", "/static/shapetrees/containment/shapetree", null),
                new DispatcherEntry(List.of("validation/validation-container"), "GET", "/validation/", null),
                new DispatcherEntry(List.of("validation/valid-resource"), "GET", "/validation/valid-resource", null),
                new DispatcherEntry(List.of("validation/containment/container-1"), "GET", "/validation/container-1/", null),
                new DispatcherEntry(List.of("validation/containment/container-1-multiplecontains-manager"), "GET", "/validation/container-1/.shapetree", null),
                new DispatcherEntry(List.of("http/404"), "GET", "/static/shex/missing", null),
                new DispatcherEntry(List.of("http/404"), "GET", "/static/shapetrees/missing", null),
                new DispatcherEntry(List.of("schemas/validation-shex"), "GET", "/static/shex/validation", null),
                new DispatcherEntry(List.of("schemas/containment-shex"), "GET", "/static/shex/containment", null),
                new DispatcherEntry(List.of("schemas/invalid-shex"), "GET", "/static/shex/invalid", null)));
    }

    @SneakyThrows
    @Test
    @DisplayName("Validate expectsType of Container")
    void validateExpectsContainerType() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ValidationResult result;

        ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(MockWebServerHelper.toUrl(server, "/static/shapetrees/validation/shapetree#ExpectsContainerTree"));
        result = shapeTree.validateResource(null, ShapeTreeResourceType.CONTAINER, null, null);
        Assertions.assertTrue(result.isValid());

        result = shapeTree.validateResource(null, ShapeTreeResourceType.RESOURCE, null, null);
        Assertions.assertFalse(result.isValid());

        result = shapeTree.validateResource(null, ShapeTreeResourceType.NON_RDF, null, null);
        Assertions.assertFalse(result.isValid());
    }

    @SneakyThrows
    @Test
    @DisplayName("Validate expectsType of Resource")
    void validateExpectsResourceType() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ValidationResult result;

        ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(MockWebServerHelper.toUrl(server, "/static/shapetrees/validation/shapetree#ExpectsResourceTree"));
        result = shapeTree.validateResource(null, ShapeTreeResourceType.RESOURCE, null, null);
        Assertions.assertTrue(result.isValid());

        result = shapeTree.validateResource(null, ShapeTreeResourceType.CONTAINER, null, null);
        Assertions.assertFalse(result.isValid());

        result = shapeTree.validateResource(null, ShapeTreeResourceType.NON_RDF, null, null);
        Assertions.assertFalse(result.isValid());
    }

    @SneakyThrows
    @Test
    @DisplayName("Validate expectsType of NonRDFResource")
    void validateExpectsNonRDFResourceType() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ValidationResult result;

        ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(MockWebServerHelper.toUrl(server, "/static/shapetrees/validation/shapetree#ExpectsNonRDFResourceTree"));
        result = shapeTree.validateResource(null, ShapeTreeResourceType.NON_RDF, null, null);
        Assertions.assertTrue(result.isValid());

        result = shapeTree.validateResource(null, ShapeTreeResourceType.RESOURCE, null, null);
        Assertions.assertFalse(result.isValid());

        result = shapeTree.validateResource(null, ShapeTreeResourceType.CONTAINER, null, null);
        Assertions.assertFalse(result.isValid());
    }

    @SneakyThrows
    @Test
    @DisplayName("Validate label")
    void validateLabel() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ValidationResult result;

        ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(MockWebServerHelper.toUrl(server, "/static/shapetrees/validation/shapetree#LabelTree"));
        result = shapeTree.validateResource("resource-name", ShapeTreeResourceType.RESOURCE, null, null);
        Assertions.assertTrue(result.isValid());

        result = shapeTree.validateResource("invalid-name", ShapeTreeResourceType.RESOURCE, null, null);
        Assertions.assertFalse(result.isValid());
    }

    @SneakyThrows
    @Test
    @DisplayName("Validate shape")
    void validateShape() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ValidationResult result;

        ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(MockWebServerHelper.toUrl(server, "/static/shapetrees/validation/shapetree#FooTree"));

        // Validate shape with focus node
        List<URL> focusNodeUrls = List.of(MockWebServerHelper.toUrl(server, "/validation/valid-resource#foo"));
        result = shapeTree.validateResource(null, ShapeTreeResourceType.RESOURCE, getFooBodyGraph(MockWebServerHelper.toUrl(server, "/validation/valid-resource")), focusNodeUrls);
        Assertions.assertTrue(result.isValid());

        // Validate shape without focus node
        result = shapeTree.validateResource(null, ShapeTreeResourceType.RESOURCE, getFooBodyGraph(MockWebServerHelper.toUrl(server, "/validation/valid-resource")), null);
        Assertions.assertTrue(result.isValid());

    }

    @SneakyThrows
    @Test
    @DisplayName("Fail to validate shape")
    void failToValidateShape() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ValidationResult result;

        ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(MockWebServerHelper.toUrl(server, "/static/shapetrees/validation/shapetree#FooTree"));

        // Pass in body content that will fail validation of the shape associated with FooTree
        List<URL> focusNodeUrls = List.of(MockWebServerHelper.toUrl(server,"/validation/valid-resource#foo"));
        result = shapeTree.validateResource(null, ShapeTreeResourceType.RESOURCE, getInvalidFooBodyGraph(MockWebServerHelper.toUrl(server, "/validation/valid-resource")), focusNodeUrls);
        Assertions.assertFalse(result.isValid());

    }

    @SneakyThrows
    @Test
    @DisplayName("Fail to validate shape when the shape resource cannot be found")
    void failToValidateMissingShape() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ValidationResult result;

        ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(MockWebServerHelper.toUrl(server, "/static/shapetrees/validation/shapetree#MissingShapeSchemaTree"));

        Graph fooBodyGraph = getFooBodyGraph(MockWebServerHelper.toUrl(server, "/validation/valid-resource"));

        // Catch exception thrown when a shape in a shape tree cannot be found
        List<URL> focusNodeUrls = List.of(MockWebServerHelper.toUrl(server,"/validation/valid-resource#foo"));
        Assertions.assertThrows(ShapeTreeException.class, () -> shapeTree.validateResource(null, ShapeTreeResourceType.RESOURCE, fooBodyGraph, focusNodeUrls));

    }

    @SneakyThrows
    @Test
    @DisplayName("Fail to validate shape when the shape resource is malformed")
    void failToValidateMalformedShape() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ValidationResult result;

        ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(MockWebServerHelper.toUrl(server, "/static/shapetrees/validation/shapetree#InvalidShapeSchemaTree"));

        Graph fooBodyGraph = getFooBodyGraph(MockWebServerHelper.toUrl(server, "/validation/valid-resource"));

        // Catch exception thrown when a shape in a shape tree is invalid
        List<URL> focusNodeUrls = List.of(MockWebServerHelper.toUrl(server,"/validation/valid-resource#foo"));
        Assertions.assertThrows(ShapeTreeException.class, () -> shapeTree.validateResource(null, ShapeTreeResourceType.RESOURCE, fooBodyGraph, focusNodeUrls));

    }

    @SneakyThrows
    @Test
    @DisplayName("Fail shape validation when shape tree doesn't validate a shape")
    void failToValidateWhenNoShapeInShapeTree() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Get the NoShapeValidationTree shape tree. This shape tree doesn't enforce shape validation,
        // so it should return an error when using to validate
        ShapeTree noShapeValidationTree = ShapeTreeFactory.getShapeTree(MockWebServerHelper.toUrl(server, "/static/shapetrees/validation/shapetree#NoShapeValidationTree"));
        String graphTtl = "<#a> <#b> <#c> .";
        List<URL> focusNodeUrls = List.of(MockWebServerHelper.toUrl(server,"http://a.example/b/c.d#a"));
        StringReader sr = new StringReader(graphTtl);
        Model model = ModelFactory.createDefaultModel();
        RDFDataMgr.read(model, sr, "http://example.com/", Lang.TTL);

        Assertions.assertThrows(ShapeTreeException.class, () -> noShapeValidationTree.validateGraph(model.getGraph(), focusNodeUrls));
    }

    @SneakyThrows
    @Test
    @DisplayName("Validate shape before it is cached in schema cache")
    void validateShapeBeforeCaching() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ValidationResult result;

        SchemaCache.initializeCache();

        ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(MockWebServerHelper.toUrl(server, "/static/shapetrees/validation/shapetree#FooTree"));

        // Validate shape with focus node
        List<URL> focusNodeUrls = List.of(MockWebServerHelper.toUrl(server,"/validation/valid-resource#foo"));
        result = shapeTree.validateResource(null, ShapeTreeResourceType.RESOURCE, getFooBodyGraph(MockWebServerHelper.toUrl(server, "/validation/valid-resource")), focusNodeUrls);
        Assertions.assertTrue(result.isValid());

    }

    @SneakyThrows
    @Test
    @DisplayName("Validate shape after it is cached in schema cache")
    void validateShapeAfterCaching() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ValidationResult result;

        Map<URL, ShexSchema> schemas = SchemaCacheTests.buildSchemaCache(List.of(MockWebServerHelper.toUrl(server, "/static/shex/validation").toString()));
        SchemaCache.initializeCache(schemas);

        ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(MockWebServerHelper.toUrl(server, "/static/shapetrees/validation/shapetree#FooTree"));

        // Validate shape with focus node
        List<URL> focusNodeUrls = List.of(MockWebServerHelper.toUrl(server,"/validation/valid-resource#foo"));
        result = shapeTree.validateResource(null, ShapeTreeResourceType.RESOURCE, getFooBodyGraph(MockWebServerHelper.toUrl(server, "/validation/valid-resource")), focusNodeUrls);
        Assertions.assertTrue(result.isValid());

    }

    private Graph getFooBodyGraph(URL baseUrl) throws ShapeTreeException {
        String body = "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \n" +
                      "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \n" +
                      "PREFIX xml: <http://www.w3.org/XML/1998/namespace> \n" +
                      "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> \n" +
                      "PREFIX ex: <http://www.example.com/ns/ex#> \n" +
                      "<#foo> \n" +
                      "    ex:id 56789 ; \n" +
                      "    ex:name \"Footastic\" ; \n" +
                      "    ex:created_at \"2021-04-04T20:15:47.000Z\"^^xsd:dateTime . \n";

        return GraphHelper.readStringIntoGraph(GraphHelper.urlToUri(baseUrl), body, "text/turtle");
    }

    private Graph getInvalidFooBodyGraph(URL baseUrl) throws ShapeTreeException {
        String body = "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \n" +
                "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \n" +
                "PREFIX xml: <http://www.w3.org/XML/1998/namespace> \n" +
                "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> \n" +
                "PREFIX ex: <http://www.example.com/ns/ex#> \n" +
                "<#foo> \n" +
                "    ex:id 56789 ; \n" +
                "    ex:created_at \"2021-04-04T20:15:47.000Z\"^^xsd:dateTime . \n";

        return GraphHelper.readStringIntoGraph(GraphHelper.urlToUri(baseUrl), body, "text/turtle");
    }

    private String getAttributeOneBodyGraph() {
        return "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \n" +
                "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \n" +
                "PREFIX xml: <http://www.w3.org/XML/1998/namespace> \n" +
                "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> \n" +
                "PREFIX ex: <http://www.example.com/ns/ex#> \n" +
                "<#resource> \n" +
                "    ex:name \"Attribute 1\" ; \n" +
                "    ex:created_at \"2021-04-04T20:15:47.000Z\"^^xsd:dateTime . \n";
    }

}