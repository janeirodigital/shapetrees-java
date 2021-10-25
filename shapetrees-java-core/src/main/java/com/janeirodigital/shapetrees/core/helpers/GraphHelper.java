package com.janeirodigital.shapetrees.core.helpers;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import lombok.extern.slf4j.Slf4j;
import org.apache.jena.datatypes.xsd.XSDDatatype;
import org.apache.jena.graph.*;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.riot.RiotException;

import java.io.StringReader;
import java.io.StringWriter;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.time.OffsetDateTime;

/**
 * Assorted helper methods related to RDF Graphs
 */
@Slf4j
public class GraphHelper {

    private GraphHelper() {
    }

    /**
     * Determine the Jena language (graph serialization type) based on a content type string
     * @param contentType Content type string
     * @return Serialization language
     */
    public static Lang getLangForContentType(String contentType) { // !! Optional<String>
        if (contentType == null) {
            return Lang.TURTLE;
        }
        switch (contentType) {
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
    public static String writeGraphToTurtleString(Graph graph) {
        if (graph == null) return null;
        if (graph.isClosed()) return null;

        StringWriter sw = new StringWriter();
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
    public static Model readStringIntoModel(URI baseURI, String rawContent, String contentType) throws ShapeTreeException {
        try {
            Model model = ModelFactory.createDefaultModel();
            StringReader reader = new StringReader(rawContent);
            RDFDataMgr.read(model.getGraph(), reader, baseURI.toString(), GraphHelper.getLangForContentType(contentType));
            return model;
        } catch (RiotException rex) {
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
    public static Graph readStringIntoGraph(URI baseURI, String rawContent, String contentType) throws ShapeTreeException {
        return readStringIntoModel(baseURI, rawContent, contentType).getGraph();
    }

    /**
     * Creates an empty Graph with initialized prefixes
     * @return Graph Empty Graph
     * @throws ShapeTreeException ShapeTreeException
     */
    public static Graph getEmptyGraph() {
        Model model = ModelFactory.createDefaultModel();
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
    public static Triple newTriple(URI subject, URI predicate, Object object) throws ShapeTreeException {

        if (subject == null || predicate == null || object == null) {
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
    public static Triple newTriple(String subject, String predicate, Object object) throws ShapeTreeException {

        if (subject == null || predicate == null || object == null) {
            throw new ShapeTreeException(500, "Cannot provide null values as input to triple construction");
        }

        Node objectNode = null;
        if (object.getClass().equals(URI.class)) { // TODO: needed?
            objectNode = NodeFactory.createURI(object.toString());
        }
        else if (object.getClass().equals(URL.class)) {
            objectNode = NodeFactory.createURI(object.toString());
        }
        else if (object.getClass().equals(String.class)) {
            objectNode = NodeFactory.createLiteral(object.toString());
        }
        else if (object.getClass().equals(OffsetDateTime.class)) {
            objectNode = NodeFactory.createLiteralByValue(object, XSDDatatype.XSDdateTime);
        }
        else if (object.getClass().equals(Boolean.class)) {
            objectNode = NodeFactory.createLiteralByValue(object, XSDDatatype.XSDboolean);
        }
        else if (object.getClass().equals(Node_Blank.class)) {
            objectNode = (Node) object;
        }

        if (objectNode == null) {
            throw new ShapeTreeException(500, "Unsupported object value in triple construction: " + object.getClass());
        }

        return new Triple(NodeFactory.createURI(subject), NodeFactory.createURI(predicate), objectNode);
    }

    /**
     * Wrap conversion from URL to URI which should never fail on a well-formed URL.
     * @param url covert this URL to a URI
     * @return IRI java native object for a URI (useful for Jena graph operations)
     */
    public static URI urlToUri(URL url) {
        try {
            return url.toURI();
        } catch (URISyntaxException e) {
            throw new IllegalStateException("can't convert URL <" + url + "> to IRI: " + e);
        }
    }
}
