package com.janeirodigital.shapetrees.tests;

import com.janeirodigital.shapetrees.core.validation.SchemaCache;
import com.janeirodigital.shapetrees.core.validation.ShapeTree;
import com.janeirodigital.shapetrees.core.validation.ShapeTreeFactory;
import com.janeirodigital.shapetrees.core.validation.ValidationResult;
import com.janeirodigital.shapetrees.core.contentloaders.DocumentLoaderManager;
import com.janeirodigital.shapetrees.core.contentloaders.HttpExternalDocumentLoader;
import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.helpers.GraphHelper;
import com.janeirodigital.shapetrees.tests.fixtures.MockWebServerHelper;
import com.janeirodigital.shapetrees.tests.fixtures.RequestMatchingFixtureDispatcher;
import fr.inria.lille.shexjava.schema.ShexSchema;
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
import java.net.MalformedURLException;
import java.net.URL;
import java.util.List;
import java.util.Map;

import static com.janeirodigital.shapetrees.tests.fixtures.DispatcherHelper.mockOnGet;

class ShapeTreeValidationTests {

    private static MockWebServer server;

    public ShapeTreeValidationTests() {
        HttpExternalDocumentLoader httpExternalDocumentLoader = new HttpExternalDocumentLoader();
        DocumentLoaderManager.setLoader(httpExternalDocumentLoader);
    }

    @BeforeAll
    static void beforeAll() {

        RequestMatchingFixtureDispatcher dispatcher = new RequestMatchingFixtureDispatcher();
        
        mockOnGet(dispatcher, "/static/shapetrees/validation/shapetree", "shapetrees/validation-shapetree-ttl");
        mockOnGet(dispatcher, "/static/shapetrees/containment/shapetree", "shapetrees/containment-shapetree-ttl");
        mockOnGet(dispatcher, "/validation/", "validation/validation-container");
        mockOnGet(dispatcher, "/validation/valid-resource", "validation/valid-resource");
        mockOnGet(dispatcher, "/validation/container-1/", "validation/containment/container-1");
        mockOnGet(dispatcher, "/validation/container-1/.shapetree", "validation/containment/container-1-multiplecontains-manager");
        mockOnGet(dispatcher, "/static/shex/missing", "http/404");
        mockOnGet(dispatcher, "/static/shapetrees/missing", "http/404");
        mockOnGet(dispatcher, "/static/shex/validation", "schemas/validation-shex");
        mockOnGet(dispatcher, "/static/shex/containment", "schemas/containment-shex");
        mockOnGet(dispatcher, "/static/shex/invalid", "schemas/invalid-shex");

        server = new MockWebServer();
        server.setDispatcher(dispatcher);

    }

    @Test
    @DisplayName("Validate expectsType of Container")
    void validateExpectsContainerType() throws ShapeTreeException {
        ValidationResult result;

        ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(MockWebServerHelper.toUrl(server, "/static/shapetrees/validation/shapetree#ExpectsContainerTree"));
        result = shapeTree.validateResource(null, ShapeTreeResourceType.CONTAINER, null, null);
        Assertions.assertTrue(result.isValid());

        result = shapeTree.validateResource(null, ShapeTreeResourceType.RESOURCE, null, null);
        Assertions.assertFalse(result.isValid());

        result = shapeTree.validateResource(null, ShapeTreeResourceType.NON_RDF, null, null);
        Assertions.assertFalse(result.isValid());
    }

    @Test
    @DisplayName("Validate expectsType of Resource")
    void validateExpectsResourceType() throws ShapeTreeException {
        ValidationResult result;

        ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(MockWebServerHelper.toUrl(server, "/static/shapetrees/validation/shapetree#ExpectsResourceTree"));
        result = shapeTree.validateResource(null, ShapeTreeResourceType.RESOURCE, null, null);
        Assertions.assertTrue(result.isValid());

        result = shapeTree.validateResource(null, ShapeTreeResourceType.CONTAINER, null, null);
        Assertions.assertFalse(result.isValid());

        result = shapeTree.validateResource(null, ShapeTreeResourceType.NON_RDF, null, null);
        Assertions.assertFalse(result.isValid());
    }

    @Test
    @DisplayName("Validate expectsType of NonRDFResource")
    void validateExpectsNonRDFResourceType() throws ShapeTreeException {
        ValidationResult result;

        ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(MockWebServerHelper.toUrl(server, "/static/shapetrees/validation/shapetree#ExpectsNonRDFResourceTree"));
        result = shapeTree.validateResource(null, ShapeTreeResourceType.NON_RDF, null, null);
        Assertions.assertTrue(result.isValid());

        result = shapeTree.validateResource(null, ShapeTreeResourceType.RESOURCE, null, null);
        Assertions.assertFalse(result.isValid());

        result = shapeTree.validateResource(null, ShapeTreeResourceType.CONTAINER, null, null);
        Assertions.assertFalse(result.isValid());
    }

    @Test
    @DisplayName("Validate label")
    void validateLabel() throws ShapeTreeException {
        ValidationResult result;

        ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(MockWebServerHelper.toUrl(server, "/static/shapetrees/validation/shapetree#LabelTree"));
        result = shapeTree.validateResource("resource-name", ShapeTreeResourceType.RESOURCE, null, null);
        Assertions.assertTrue(result.isValid());

        result = shapeTree.validateResource("invalid-name", ShapeTreeResourceType.RESOURCE, null, null);
        Assertions.assertFalse(result.isValid());
    }

    @Test
    @DisplayName("Validate shape")
    void validateShape() throws ShapeTreeException {
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

    @Test
    @DisplayName("Fail to validate shape")
    void failToValidateShape() throws ShapeTreeException {
        ValidationResult result;

        ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(MockWebServerHelper.toUrl(server, "/static/shapetrees/validation/shapetree#FooTree"));

        // Pass in body content that will fail validation of the shape associated with FooTree
        List<URL> focusNodeUrls = List.of(MockWebServerHelper.toUrl(server,"/validation/valid-resource#foo"));
        result = shapeTree.validateResource(null, ShapeTreeResourceType.RESOURCE, getInvalidFooBodyGraph(MockWebServerHelper.toUrl(server, "/validation/valid-resource")), focusNodeUrls);
        Assertions.assertFalse(result.isValid());

    }

    @Test
    @DisplayName("Fail to validate shape when the shape resource cannot be found")
    void failToValidateMissingShape() throws ShapeTreeException {
        ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(MockWebServerHelper.toUrl(server, "/static/shapetrees/validation/shapetree#MissingShapeSchemaTree"));
        Graph fooBodyGraph = getFooBodyGraph(MockWebServerHelper.toUrl(server, "/validation/valid-resource"));
        // Catch exception thrown when a shape in a shape tree cannot be found
        List<URL> focusNodeUrls = List.of(MockWebServerHelper.toUrl(server,"/validation/valid-resource#foo"));
        Assertions.assertThrows(ShapeTreeException.class, () -> shapeTree.validateResource(null, ShapeTreeResourceType.RESOURCE, fooBodyGraph, focusNodeUrls));
    }

    @Test
    @DisplayName("Fail to validate shape when the shape resource is malformed")
    void failToValidateMalformedShape() throws ShapeTreeException {
        ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(MockWebServerHelper.toUrl(server, "/static/shapetrees/validation/shapetree#InvalidShapeSchemaTree"));
        Graph fooBodyGraph = getFooBodyGraph(MockWebServerHelper.toUrl(server, "/validation/valid-resource"));
        // Catch exception thrown when a shape in a shape tree is invalid
        List<URL> focusNodeUrls = List.of(MockWebServerHelper.toUrl(server,"/validation/valid-resource#foo"));
        Assertions.assertThrows(ShapeTreeException.class, () -> shapeTree.validateResource(null, ShapeTreeResourceType.RESOURCE, fooBodyGraph, focusNodeUrls));

    }
    
    @Test
    @DisplayName("Fail shape validation when shape tree doesn't validate a shape")
    void failToValidateWhenNoShapeInShapeTree() throws ShapeTreeException {
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

    @Test
    @DisplayName("Validate shape before it is cached in schema cache")
    void validateShapeBeforeCaching() throws ShapeTreeException {
        ValidationResult result;

        SchemaCache.initializeCache();

        ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(MockWebServerHelper.toUrl(server, "/static/shapetrees/validation/shapetree#FooTree"));

        // Validate shape with focus node
        List<URL> focusNodeUrls = List.of(MockWebServerHelper.toUrl(server,"/validation/valid-resource#foo"));
        result = shapeTree.validateResource(null, ShapeTreeResourceType.RESOURCE, getFooBodyGraph(MockWebServerHelper.toUrl(server, "/validation/valid-resource")), focusNodeUrls);
        Assertions.assertTrue(result.isValid());

    }

    @Test
    @DisplayName("Validate shape after it is cached in schema cache")
    void validateShapeAfterCaching() throws MalformedURLException, ShapeTreeException {
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

}
