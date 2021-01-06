package com.janeirodigital.shapetrees.core.helpers;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import org.apache.jena.graph.Graph;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.riot.RiotException;

import java.io.StringReader;
import java.io.StringWriter;
import java.net.URI;

/**
 * Assorted helper methods related to RDF Graphs
 */
public class GraphHelper {

    private GraphHelper() {
    }

    /**
     * Determine the Jena language (graph serialization type) based on a content type string
     * @param contentType Content type string
     * @return Serialization language
     */
    public static Lang getLangForContentType(String contentType) {
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
}
