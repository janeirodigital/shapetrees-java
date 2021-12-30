// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.tests
import { ShapeTreeException } from '@shapetrees/core/src/exceptions/ShapeTreeException';
import { GraphHelper } from '@shapetrees/core/src/helpers/GraphHelper';
import * as Graph from 'org/apache/jena/graph';
import * as NodeFactory from 'org/apache/jena/graph';
import * as Triple from 'org/apache/jena/graph';
import * as ModelFactory from 'org/apache/jena/rdf/model';
import * as Lang from 'org/apache/jena/riot';
import * as Assertions from 'org/junit/jupiter/api';
import * as DisplayName from 'org/junit/jupiter/api';
import * as Test from 'org/junit/jupiter/api';
import * as ParameterizedTest from 'org/junit/jupiter/params';
import * as NullAndEmptySource from 'org/junit/jupiter/params/provider';
import * as ValueSource from 'org/junit/jupiter/params/provider';
import * as URI from 'java/net';
import * as OffsetDateTime from 'java/time';
import { newTriple } from '@shapetrees/core/src/helpers/GraphHelper/newTriple';

class GraphHelperTests {

  // @ParameterizedTest, @NullAndEmptySource, @DisplayName("Handle null or empty content types with defaults"), @SneakyThrows
  handleNullOrEmptyContentTypes(type: string): void {
    let lang: Lang = GraphHelper.getLangForContentType(type);
    Assertions.assertEquals(lang, Lang.TURTLE);
  }

  // @ParameterizedTest, @ValueSource(strings = { "text/turtle", "something/bogus" }), @DisplayName("Handle turtle content type when specified or as default"), @SneakyThrows
  handleTurtleContentType(type: string): void {
    let lang: Lang = GraphHelper.getLangForContentType(type);
    Assertions.assertEquals(lang, Lang.TURTLE);
  }

  // @Test, @DisplayName("JSON LD content type"), @SneakyThrows
  handleJsonLD(): void {
    let lang: Lang = GraphHelper.getLangForContentType("application/ld+json");
    Assertions.assertEquals(lang, Lang.JSONLD);
  }

  // @Test, @DisplayName("N-Triples content type"), @SneakyThrows
  hanldeNTriples(): void {
    let lang: Lang = GraphHelper.getLangForContentType("application/n-triples");
    Assertions.assertEquals(lang, Lang.NTRIPLES);
  }

  // @Test, @DisplayName("rdf+xml content type"), @SneakyThrows
  hanldeRDFXMLTriples(): void {
    let lang: Lang = GraphHelper.getLangForContentType("application/rdf+xml");
    Assertions.assertEquals(lang, Lang.RDFXML);
  }

  // @Test, @DisplayName("Parse invalid TTL"), @SneakyThrows
  parseInvalidTTL(): void {
    let invalidTtl: string = "<#a> b c";
    Assertions.assertThrows(ShapeTreeException.class, () -> GraphHelper.readStringIntoGraph(URI.create("https://example.com/a"), invalidTtl, "text/turtle"));
  }

  // @Test, @DisplayName("Parse valid TTL"), @SneakyThrows
  parseValidTTL(): void {
    let invalidTtl: string = "<#a> <#b> <#c> .";
    Assertions.assertNotNull(GraphHelper.readStringIntoGraph(URI.create("https://example.com/a"), invalidTtl, "text/turtle"));
  }

  // @Test, @DisplayName("Write graph to TTL String"), @SneakyThrows
  writeGraphToTTLString(): void {
    let graph: Graph = ModelFactory.createDefaultModel().getGraph();
    graph.add(new Triple(NodeFactory.createURI("<#b>"), NodeFactory.createURI("<#c>"), NodeFactory.createURI("<#d>")));
    Assertions.assertNotNull(GraphHelper.writeGraphToTurtleString(graph));
  }

  // @Test, @DisplayName("Write null graph to TTL String"), @SneakyThrows
  writeNullGraphToTTLString(): void {
    Assertions.assertNull(GraphHelper.writeGraphToTurtleString(null));
  }

  // @Test, @DisplayName("Write closed graph to TTL String"), @SneakyThrows
  writeClosedGraphtoTTLString(): void {
    let graph: Graph = ModelFactory.createDefaultModel().getGraph();
    graph.add(new Triple(NodeFactory.createURI("<#b>"), NodeFactory.createURI("<#c>"), NodeFactory.createURI("<#d>")));
    graph.close();
    Assertions.assertNull(GraphHelper.writeGraphToTurtleString(graph));
  }

  // @Test, @DisplayName("Fail to store null string objects in new Triple helper"), @SneakyThrows
  failToStoreNullTripleStringObjects(): void {
    Assertions.assertThrows(ShapeTreeException.class, () -> {
      newTriple("<#b>", "<#c>", null);
    });
    Assertions.assertThrows(ShapeTreeException.class, () -> {
      newTriple("<#b>", null, "<#d>");
    });
    Assertions.assertThrows(ShapeTreeException.class, () -> {
      newTriple(null, "<#c>", "<#d>");
    });
  }

  // @Test, @DisplayName("Fail to store null URI objects in new Triple helper"), @SneakyThrows
  failToStoreNullTripleURIObjects(): void {
    let subjectURI: URI = URI.create("https://site.example/#a");
    let predicateURI: URI = URI.create("https://site.example/#b");
    let objectURI: URI = URI.create("https://site.example/#c");
    Assertions.assertThrows(ShapeTreeException.class, () -> {
      newTriple(subjectURI, predicateURI, null);
    });
    Assertions.assertThrows(ShapeTreeException.class, () -> {
      newTriple(subjectURI, null, objectURI);
    });
    Assertions.assertThrows(ShapeTreeException.class, () -> {
      newTriple(null, predicateURI, objectURI);
    });
  }

  // @Test, @DisplayName("Store URI as subject and predicate with new Triple helper"), @SneakyThrows
  storeURISubjectAndPredicate(): void {
    let uriTriple: Triple = newTriple(URI.create("https://site.example/#a"), URI.create("https://site.example/#b"), URI.create("https://site.example/#a"));
    Assertions.assertNotNull(uriTriple);
    Assertions.assertTrue(uriTriple.getSubject().isURI());
    Assertions.assertTrue(uriTriple.getPredicate().isURI());
    Assertions.assertTrue(uriTriple.getObject().isURI());
  }

  // @Test, @DisplayName("Store URI object with new Triple helper"), @SneakyThrows
  storeURIasTripleObject(): void {
    let uriTriple: Triple = newTriple("https://site.example/#a", "https://site.example/#b", URI.create("https://site.example/#c"));
    Assertions.assertNotNull(uriTriple);
    Assertions.assertTrue(uriTriple.getObject().isURI());
  }

  // @Test, @DisplayName("Store String object with new Triple helper"), @SneakyThrows
  storeStringAsTripleObject(): void {
    let uriTriple: Triple = newTriple("https://site.example/#a", "https://site.example/#b", "This is a test string");
    Assertions.assertNotNull(uriTriple);
    Assertions.assertTrue(uriTriple.getObject().isLiteral());
  }

  // @Test, @DisplayName("Store DateTime object with new Triple helper"), @SneakyThrows
  storeDateTimeAsTripleObject(): void {
    let uriTriple: Triple = newTriple("https://site.example/#a", "https://site.example/#b", OffsetDateTime.now());
    Assertions.assertNotNull(uriTriple);
    Assertions.assertTrue(uriTriple.getObject().isLiteral());
  }

  // @Test, @DisplayName("Store Boolean object with new Triple helper"), @SneakyThrows
  storeBooleanAsTripleObject(): void {
    let uriTriple: Triple = newTriple("https://site.example/#a", "https://site.example/#b", Boolean.TRUE);
    Assertions.assertNotNull(uriTriple);
    Assertions.assertTrue(uriTriple.getObject().isLiteral());
  }

  // @Test, @DisplayName("Store Blank Node with new Triple helper"), @SneakyThrows
  storeBlankNodeAsTripleObject(): void {
    let uriTriple: Triple = newTriple("https://site.example/#a", "https://site.example/#b", NodeFactory.createBlankNode());
    Assertions.assertNotNull(uriTriple);
    Assertions.assertTrue(uriTriple.getObject().isBlank());
  }

  // @Test, @DisplayName("Fail to store Unsupported Type with new Triple helper"), @SneakyThrows
  failedToStoreUnsupportedTypeAsTripleObject(): void {
    Assertions.assertThrows(ShapeTreeException.class, () -> {
      newTriple("https://site.example/#a", "https://site.example/#b", (float) 35.6);
    });
  }
}
