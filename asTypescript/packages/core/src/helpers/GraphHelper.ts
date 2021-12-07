// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.core.helpers
import { ShapeTreeException } from '../exceptions/ShapeTreeException';
import * as XSDDatatype from 'org/apache/jena/datatypes/xsd';
import * as Graph from 'org/apache/jena/graph';
import * as Triple from 'org/apache/jena/graph';
import * as NodeFactory from 'org/apache/jena/graph';
import * as Node from 'org/apache/jena/graph';
import * as Node_Blank from 'org/apache/jena/graph';
import * as Model from 'org/apache/jena/rdf/model';
import * as ModelFactory from 'org/apache/jena/rdf/model';
import * as Lang from 'org/apache/jena/riot';
import * as RDFDataMgr from 'org/apache/jena/riot';
import * as RiotException from 'org/apache/jena/riot';
import { Writable } from 'stream';
import * as MalformedURLException from 'java/net';
import * as URI from 'java/net';
import * as URISyntaxException from 'java/net';
import * as OffsetDateTime from 'java/time';

/**
 * Assorted helper methods related to RDF Graphs
 */
export class GraphHelper {

  private constructor() {
  }

  /**
   * Determine the Jena language (graph serialization type) based on a content type string
   * @param contentType Content type string
   * @return Serialization language
   */
  public static getLangForContentType(contentType: string): Lang {
    // !! Optional<String>
    if (contentType === null) {
      return Lang.TURTLE;
    }
    switch(contentType) {
      case "application/ld+json":
        return Lang.JSONLD;
      case "application/rdf+xml":
        return Lang.RDFXML;
      case "application/n-triples":
        return Lang.NTRIPLES;
      default:
        return Lang.TURTLE;
    }
  }

  /**
   * Writes a graph into a turtle serialization
   * @param graph Graph to serialize
   * @return String in TTL serialization
   */
  public static writeGraphToTurtleString(graph: Graph): string {
    if (graph === null)
      return null;
    if (graph.isClosed())
      return null;
    let sw: Writable = new Writable();
    RDFDataMgr.write(sw, graph, Lang.TURTLE);
    graph.close();
    return sw.toString();
  }

  /**
   * Deserializes a string into a Model
   * @param baseURI Base URI to use for statements
   * @param rawContent String of RDF
   * @param contentType Content type of content
   * @return Deserialized model
   * @throws ShapeTreeException ShapeTreeException
   */
  public static readStringIntoModel(baseURI: URI, rawContent: string, contentType: string): Model /* throws ShapeTreeException */ {
    try {
      let model: Model = ModelFactory.createDefaultModel();
      let reader: StringReader = new StringReader(rawContent);
      RDFDataMgr.read(model.getGraph(), reader, baseURI.toString(), GraphHelper.getLangForContentType(contentType));
      return model;
    } catch (ex) {
 if (ex instanceof RiotException) {
       throw new ShapeTreeException(422, "Error processing input - " + rex.getMessage());
     }
  }

  /**
   * Deserializes a string into a Graph
   * @param baseURI Base URI to use for statements
   * @param rawContent String of RDF
   * @param contentType Content type of content
   * @return Deserialized graph
   * @throws ShapeTreeException ShapeTreeException
   */
  public static readStringIntoGraph(baseURI: URI, rawContent: string, contentType: string): Graph /* throws ShapeTreeException */ {
    return readStringIntoModel(baseURI, rawContent, contentType).getGraph();
  }

  /**
   * Creates an empty Graph with initialized prefixes
   * @return Graph Empty Graph
   */
  public static getEmptyGraph(): Graph {
    let model: Model = ModelFactory.createDefaultModel();
    model.setNsPrefix("rdfs", "http://www.w3.org/2000/01/rdf-schema#");
    model.setNsPrefix("xsd", "http://www.w3.org/2001/XMLSchema#");
    model.setNsPrefix("st", "http://www.w3.org/ns/shapetrees#");
    return model.getGraph();
  }

  /**
   * Create a new triple statement with URIs
   * @param subject Subject to include
   * @param predicate Predicate to include
   * @param object Object to include
   * @return
   */
  public static newTriple(subject: URI, predicate: URI, object: Object): Triple /* throws ShapeTreeException */ {
    if (subject === null || predicate === null || object === null) {
      throw new ShapeTreeException(500, "Cannot provide null values as input to triple construction");
    }
    return newTriple(subject.toString(), predicate.toString(), object);
  }

  /**
   * Create a new triple statement with strings
   * @param subject Subject to include
   * @param predicate Predicate to include
   * @param object Object to include
   * @return
   */
  public static newTriple(subject: string, predicate: string, object: Object): Triple /* throws ShapeTreeException */ {
    if (subject === null || predicate === null || object === null) {
      throw new ShapeTreeException(500, "Cannot provide null values as input to triple construction");
    }
    let objectNode: Node = null;
    if (object.getClass() === URI.class) {
      // TODO: needed?
      objectNode = NodeFactory.createURI(object.toString());
    } else if (object.getClass() === URL.class) {
      objectNode = NodeFactory.createURI(object.toString());
    } else if (object.getClass() === String.class) {
      objectNode = NodeFactory.createLiteral(object.toString());
    } else if (object.getClass() === OffsetDateTime.class) {
      objectNode = NodeFactory.createLiteralByValue(object, XSDDatatype.XSDdateTime);
    } else if (object.getClass() === Boolean.class) {
      objectNode = NodeFactory.createLiteralByValue(object, XSDDatatype.XSDboolean);
    } else if (object.getClass() === Node_Blank.class) {
      objectNode = (Node) object;
    }
    if (objectNode === null) {
      throw new ShapeTreeException(500, "Unsupported object value in triple construction: " + object.getClass());
    }
    return new Triple(NodeFactory.createURI(subject), NodeFactory.createURI(predicate), objectNode);
  }

  /**
   * Wrap conversion from URL to URI which should never fail on a well-formed URL.
   * @param url covert this URL to a URI
   * @return IRI java native object for a URI (useful for Jena graph operations)
   */
  public static urlToUri(url: URL): URI {
    try {
      return url.toURI();
    } catch (ex) {
 if (ex instanceof URISyntaxException) {
       throw new IllegalStateException("can't convert URL <" + url + "> to IRI: " + ex);
     }
  }

  /**
   * Remove a fragment from a URL. Returns the same URL if there is no fragment
   * @param url to remove fragment from
   * @return URL without fragment
   */
  public static removeUrlFragment(url: URL): URL {
    let uri: URI = urlToUri(url);
    if (uri.getFragment() === null) {
      return url;
    }
    try {
      let noFragment: URI = new URI(uri.getScheme(), uri.getSchemeSpecificPart(), null);
      return noFragment.toURL();
    } catch (ex) {
 if (ex instanceof MalformedURLException || ex instanceof URISyntaxException) {
       throw new IllegalStateException("Unable to remove fragment from URL: " + ex.getMessage());
     }
  }

  public static knownUrl(urlString: string): URL {
    try {
      return new URL(urlString);
    } catch (ex) {
 if (ex instanceof MalformedURLException) {
       throw new IllegalStateException("Expected known URL <" + urlString + "> to parse as valid URL - " + ex.toString());
     }
  }
}
