// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.tests
import { SchemaCache } from '@shapetrees/core/src/SchemaCache';
import { ShapeTree } from '@shapetrees/core/src/ShapeTree';
import { ShapeTreeFactory } from '@shapetrees/core/src/ShapeTreeFactory';
import { ValidationResult } from '@shapetrees/core/src/ValidationResult';
import { DocumentLoaderManager } from '@shapetrees/core/src/contentloaders/DocumentLoaderManager';
import { HttpExternalDocumentLoader } from '@shapetrees/core/src/contentloaders/HttpExternalDocumentLoader';
import { ShapeTreeResourceType } from '@shapetrees/core/src/enums/ShapeTreeResourceType';
import { ShapeTreeException } from '@shapetrees/core/src/exceptions/ShapeTreeException';
import { GraphHelper } from '@shapetrees/core/src/helpers/GraphHelper';
import { DispatcherEntry } from './fixtures/DispatcherEntry';
import { RequestMatchingFixtureDispatcher } from './fixtures/RequestMatchingFixtureDispatcher';
import * as ShexSchema from 'fr/inria/lille/shexjava/schema';
import * as Label from 'jdk/jfr';
import * as MockWebServer from 'okhttp3/mockwebserver';
import * as Graph from 'org/apache/jena/graph';
import * as Model from 'org/apache/jena/rdf/model';
import * as ModelFactory from 'org/apache/jena/rdf/model';
import * as Lang from 'org/apache/jena/riot';
import * as RDFDataMgr from 'org/apache/jena/riot';
import * as Assertions from 'org/junit/jupiter/api';
import * as BeforeAll from 'org/junit/jupiter/api';
import * as Test from 'org/junit/jupiter/api';
import { toUrl } from './fixtures/MockWebServerHelper/toUrl';

class ShapeTreeValidationTests {

   private static dispatcher: RequestMatchingFixtureDispatcher = null;

   private static httpExternalDocumentLoader: HttpExternalDocumentLoader;

  public constructor() {
    httpExternalDocumentLoader = new HttpExternalDocumentLoader();
    DocumentLoaderManager.setLoader(httpExternalDocumentLoader);
  }

  // @BeforeAll
  static beforeAll(): void {
    dispatcher = new RequestMatchingFixtureDispatcher(List.of(new DispatcherEntry(List.of("shapetrees/validation-shapetree-ttl"), "GET", "/static/shapetrees/validation/shapetree", null), new DispatcherEntry(List.of("shapetrees/containment-shapetree-ttl"), "GET", "/static/shapetrees/containment/shapetree", null), new DispatcherEntry(List.of("validation/validation-container"), "GET", "/validation/", null), new DispatcherEntry(List.of("validation/valid-resource"), "GET", "/validation/valid-resource", null), new DispatcherEntry(List.of("validation/containment/container-1"), "GET", "/validation/container-1/", null), new DispatcherEntry(List.of("validation/containment/container-1-multiplecontains-manager"), "GET", "/validation/container-1/.shapetree", null), new DispatcherEntry(List.of("http/404"), "GET", "/static/shex/missing", null), new DispatcherEntry(List.of("http/404"), "GET", "/static/shapetrees/missing", null), new DispatcherEntry(List.of("schemas/validation-shex"), "GET", "/static/shex/validation", null), new DispatcherEntry(List.of("schemas/containment-shex"), "GET", "/static/shex/containment", null), new DispatcherEntry(List.of("schemas/invalid-shex"), "GET", "/static/shex/invalid", null)));
  }

  // @SneakyThrows, @Test, @Label("Validate expectsType of Container")
  validateExpectsContainerType(): void {
    let server: MockWebServer = new MockWebServer();
    server.setDispatcher(dispatcher);
    let result: ValidationResult;
    let shapeTree: ShapeTree = ShapeTreeFactory.getShapeTree(toUrl(server, "/static/shapetrees/validation/shapetree#ExpectsContainerTree"));
    result = shapeTree.validateResource(null, null, ShapeTreeResourceType.CONTAINER, null);
    Assertions.assertTrue(result.isValid());
    result = shapeTree.validateResource(null, null, ShapeTreeResourceType.RESOURCE, null);
    Assertions.assertFalse(result.isValid());
    result = shapeTree.validateResource(null, null, ShapeTreeResourceType.NON_RDF, null);
    Assertions.assertFalse(result.isValid());
  }

  // @SneakyThrows, @Test, @Label("Validate expectsType of Resource")
  validateExpectsResourceType(): void {
    let server: MockWebServer = new MockWebServer();
    server.setDispatcher(dispatcher);
    let result: ValidationResult;
    let shapeTree: ShapeTree = ShapeTreeFactory.getShapeTree(toUrl(server, "/static/shapetrees/validation/shapetree#ExpectsResourceTree"));
    result = shapeTree.validateResource(null, null, ShapeTreeResourceType.RESOURCE, null);
    Assertions.assertTrue(result.isValid());
    result = shapeTree.validateResource(null, null, ShapeTreeResourceType.CONTAINER, null);
    Assertions.assertFalse(result.isValid());
    result = shapeTree.validateResource(null, null, ShapeTreeResourceType.NON_RDF, null);
    Assertions.assertFalse(result.isValid());
  }

  // @SneakyThrows, @Test, @Label("Validate expectsType of NonRDFResource")
  validateExpectsNonRDFResourceType(): void {
    let server: MockWebServer = new MockWebServer();
    server.setDispatcher(dispatcher);
    let result: ValidationResult;
    let shapeTree: ShapeTree = ShapeTreeFactory.getShapeTree(toUrl(server, "/static/shapetrees/validation/shapetree#ExpectsNonRDFResourceTree"));
    result = shapeTree.validateResource(null, null, ShapeTreeResourceType.NON_RDF, null);
    Assertions.assertTrue(result.isValid());
    result = shapeTree.validateResource(null, null, ShapeTreeResourceType.RESOURCE, null);
    Assertions.assertFalse(result.isValid());
    result = shapeTree.validateResource(null, null, ShapeTreeResourceType.CONTAINER, null);
    Assertions.assertFalse(result.isValid());
  }

  // @SneakyThrows, @Test, @Label("Validate label")
  validateLabel(): void {
    let server: MockWebServer = new MockWebServer();
    server.setDispatcher(dispatcher);
    let result: ValidationResult;
    let shapeTree: ShapeTree = ShapeTreeFactory.getShapeTree(toUrl(server, "/static/shapetrees/validation/shapetree#LabelTree"));
    result = shapeTree.validateResource("resource-name", null, ShapeTreeResourceType.RESOURCE, null);
    Assertions.assertTrue(result.isValid());
    result = shapeTree.validateResource("invalid-name", null, ShapeTreeResourceType.RESOURCE, null);
    Assertions.assertFalse(result.isValid());
  }

  // @SneakyThrows, @Test, @Label("Validate shape")
  validateShape(): void {
    let server: MockWebServer = new MockWebServer();
    server.setDispatcher(dispatcher);
    let result: ValidationResult;
    let shapeTree: ShapeTree = ShapeTreeFactory.getShapeTree(toUrl(server, "/static/shapetrees/validation/shapetree#FooTree"));
    // Validate shape with focus node
    let focusNodeUrls: Array<URL> = List.of(toUrl(server, "/validation/valid-resource#foo"));
    result = shapeTree.validateResource(null, focusNodeUrls, ShapeTreeResourceType.RESOURCE, getFooBodyGraph(toUrl(server, "/validation/valid-resource")));
    Assertions.assertTrue(result.isValid());
    // Validate shape without focus node
    result = shapeTree.validateResource(null, null, ShapeTreeResourceType.RESOURCE, getFooBodyGraph(toUrl(server, "/validation/valid-resource")));
    Assertions.assertTrue(result.isValid());
  }

  // @SneakyThrows, @Test, @Label("Fail to validate shape")
  failToValidateShape(): void {
    let server: MockWebServer = new MockWebServer();
    server.setDispatcher(dispatcher);
    let result: ValidationResult;
    let shapeTree: ShapeTree = ShapeTreeFactory.getShapeTree(toUrl(server, "/static/shapetrees/validation/shapetree#FooTree"));
    // Pass in body content that will fail validation of the shape associated with FooTree
    let focusNodeUrls: Array<URL> = List.of(toUrl(server, "/validation/valid-resource#foo"));
    result = shapeTree.validateResource(null, focusNodeUrls, ShapeTreeResourceType.RESOURCE, getInvalidFooBodyGraph(toUrl(server, "/validation/valid-resource")));
    Assertions.assertFalse(result.isValid());
  }

  // @SneakyThrows, @Test, @Label("Fail to validate shape when the shape resource cannot be found")
  failToValidateMissingShape(): void {
    let server: MockWebServer = new MockWebServer();
    server.setDispatcher(dispatcher);
    let result: ValidationResult;
    let shapeTree: ShapeTree = ShapeTreeFactory.getShapeTree(toUrl(server, "/static/shapetrees/validation/shapetree#MissingShapeSchemaTree"));
    let fooBodyGraph: Graph = getFooBodyGraph(toUrl(server, "/validation/valid-resource"));
    // Catch exception thrown when a shape in a shape tree cannot be found
    let focusNodeUrls: Array<URL> = List.of(toUrl(server, "/validation/valid-resource#foo"));
    Assertions.assertThrows(ShapeTreeException.class, () -> shapeTree.validateResource(null, focusNodeUrls, ShapeTreeResourceType.RESOURCE, fooBodyGraph));
  }

  // @SneakyThrows, @Test, @Label("Fail to validate shape when the shape resource is malformed")
  failToValidateMalformedShape(): void {
    let server: MockWebServer = new MockWebServer();
    server.setDispatcher(dispatcher);
    let result: ValidationResult;
    let shapeTree: ShapeTree = ShapeTreeFactory.getShapeTree(toUrl(server, "/static/shapetrees/validation/shapetree#InvalidShapeSchemaTree"));
    let fooBodyGraph: Graph = getFooBodyGraph(toUrl(server, "/validation/valid-resource"));
    // Catch exception thrown when a shape in a shape tree is invalid
    let focusNodeUrls: Array<URL> = List.of(toUrl(server, "/validation/valid-resource#foo"));
    Assertions.assertThrows(ShapeTreeException.class, () -> shapeTree.validateResource(null, focusNodeUrls, ShapeTreeResourceType.RESOURCE, fooBodyGraph));
  }

  // @SneakyThrows, @Test, @Label("Fail shape validation when shape tree doesn't validate a shape")
  failToValidateWhenNoShapeInShapeTree(): void {
    let server: MockWebServer = new MockWebServer();
    server.setDispatcher(dispatcher);
    // Get the NoShapeValidationTree shape tree. This shape tree doesn't enforce shape validation,
    // so it should return an error when using to validate
    let noShapeValidationTree: ShapeTree = ShapeTreeFactory.getShapeTree(toUrl(server, "/static/shapetrees/validation/shapetree#NoShapeValidationTree"));
    let graphTtl: string = "<#a> <#b> <#c> .";
    let focusNodeUrls: Array<URL> = List.of(toUrl(server, "http://a.example/b/c.d#a"));
    let sr: StringReader = new StringReader(graphTtl);
    let model: Model = ModelFactory.createDefaultModel();
    RDFDataMgr.read(model, sr, "http://example.com/", Lang.TTL);
    Assertions.assertThrows(ShapeTreeException.class, () -> noShapeValidationTree.validateGraph(model.getGraph(), focusNodeUrls));
  }

  // @SneakyThrows, @Test, @Label("Validate shape before it is cached in schema cache")
  validateShapeBeforeCaching(): void {
    let server: MockWebServer = new MockWebServer();
    server.setDispatcher(dispatcher);
    let result: ValidationResult;
    SchemaCache.initializeCache();
    let shapeTree: ShapeTree = ShapeTreeFactory.getShapeTree(toUrl(server, "/static/shapetrees/validation/shapetree#FooTree"));
    // Validate shape with focus node
    let focusNodeUrls: Array<URL> = List.of(toUrl(server, "/validation/valid-resource#foo"));
    result = shapeTree.validateResource(null, focusNodeUrls, ShapeTreeResourceType.RESOURCE, getFooBodyGraph(toUrl(server, "/validation/valid-resource")));
    Assertions.assertTrue(result.isValid());
  }

  // @SneakyThrows, @Test, @Label("Validate shape after it is cached in schema cache")
  validateShapeAfterCaching(): void {
    let server: MockWebServer = new MockWebServer();
    server.setDispatcher(dispatcher);
    let result: ValidationResult;
    let schemas: Map<URL, ShexSchema> = SchemaCacheTests.buildSchemaCache(List.of(toUrl(server, "/static/shex/validation").toString()));
    SchemaCache.initializeCache(schemas);
    let shapeTree: ShapeTree = ShapeTreeFactory.getShapeTree(toUrl(server, "/static/shapetrees/validation/shapetree#FooTree"));
    // Validate shape with focus node
    let focusNodeUrls: Array<URL> = List.of(toUrl(server, "/validation/valid-resource#foo"));
    result = shapeTree.validateResource(null, focusNodeUrls, ShapeTreeResourceType.RESOURCE, getFooBodyGraph(toUrl(server, "/validation/valid-resource")));
    Assertions.assertTrue(result.isValid());
  }

  private getFooBodyGraph(baseUrl: URL): Graph /* throws ShapeTreeException */ {
    let body: string = "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \n" + "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \n" + "PREFIX xml: <http://www.w3.org/XML/1998/namespace> \n" + "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> \n" + "PREFIX ex: <http://www.example.com/ns/ex#> \n" + "<#foo> \n" + "    ex:id 56789 ; \n" + "    ex:name \"Footastic\" ; \n" + "    ex:created_at \"2021-04-04T20:15:47.000Z\"^^xsd:dateTime . \n";
    return GraphHelper.readStringIntoGraph(GraphHelper.urlToUri(baseUrl), body, "text/turtle");
  }

  private getInvalidFooBodyGraph(baseUrl: URL): Graph /* throws ShapeTreeException */ {
    let body: string = "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \n" + "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \n" + "PREFIX xml: <http://www.w3.org/XML/1998/namespace> \n" + "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> \n" + "PREFIX ex: <http://www.example.com/ns/ex#> \n" + "<#foo> \n" + "    ex:id 56789 ; \n" + "    ex:created_at \"2021-04-04T20:15:47.000Z\"^^xsd:dateTime . \n";
    return GraphHelper.readStringIntoGraph(GraphHelper.urlToUri(baseUrl), body, "text/turtle");
  }

  private getAttributeOneBodyGraph(): string {
    return "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \n" + "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \n" + "PREFIX xml: <http://www.w3.org/XML/1998/namespace> \n" + "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> \n" + "PREFIX ex: <http://www.example.com/ns/ex#> \n" + "<#resource> \n" + "    ex:name \"Attribute 1\" ; \n" + "    ex:created_at \"2021-04-04T20:15:47.000Z\"^^xsd:dateTime . \n";
  }
}
