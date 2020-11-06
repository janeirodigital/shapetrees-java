package com.janeirodigital.shapetrees.test;

import com.janeirodigital.shapetrees.ShapeTreeException;
import com.janeirodigital.shapetrees.helper.GraphHelper;
import lombok.SneakyThrows;
import org.apache.jena.graph.Graph;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Triple;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.riot.Lang;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class GraphHelperTests {
    @Test
    @DisplayName("Null content type")
    @SneakyThrows
    void handleNullContentType() {
        Lang lang = GraphHelper.getLangForContentType(null);
        assertEquals(lang, Lang.TURTLE);
    }

    @Test
    @DisplayName("Turtle content type")
    @SneakyThrows
    void handleTurtleContentType() {
        Lang lang = GraphHelper.getLangForContentType("text/turtle");
        assertEquals(lang, Lang.TURTLE);
    }

    @Test
    @DisplayName("Turtle content type default")
    @SneakyThrows
    void handleDefaultContentType() {
        Lang lang = GraphHelper.getLangForContentType("something/bogus");
        assertEquals(lang, Lang.TURTLE);
    }

    @Test
    @DisplayName("JSON LD content type")
    @SneakyThrows
    void handleJsonLD() {
        Lang lang = GraphHelper.getLangForContentType("application/ld+json");
        assertEquals(lang, Lang.JSONLD);
    }

    @Test
    @DisplayName("N-Triples content type")
    @SneakyThrows
    void hanldeNTriples() {
        Lang lang = GraphHelper.getLangForContentType("application/n-triples");
        assertEquals(lang, Lang.NTRIPLES);
    }

    @Test
    @DisplayName("rdf+xml content type")
    @SneakyThrows
    void hanldeRDFXMLTriples() {
        Lang lang = GraphHelper.getLangForContentType("application/rdf+xml");
        assertEquals(lang, Lang.RDFXML);
    }

    @Test
    @DisplayName("Parse invalid TTL")
    @SneakyThrows
    void parseInvalidTTL() {
        String invalidTtl = "<#a> b c";
        assertThrows(ShapeTreeException.class, () -> GraphHelper.readStringIntoGraph(invalidTtl, "text/turtle"));
    }

    @Test
    @DisplayName("Parse valid TTL")
    @SneakyThrows
    void parseValidTTL() {
        String invalidTtl = "<#a> <#b> <#c> .";
        assertNotNull(GraphHelper.readStringIntoGraph(invalidTtl, "text/turtle"));
    }


    @Test
    @DisplayName("Write graph to TTL String")
    @SneakyThrows
    void writeGraphToTTLString() {
        Graph graph = ModelFactory.createDefaultModel().getGraph();
        graph.add(new Triple(NodeFactory.createURI("<#b>"), NodeFactory.createURI("<#c>"), NodeFactory.createURI("<#d>")));
        assertNotNull(GraphHelper.writeGraphToTurtleString(graph));
    }

    @Test
    @DisplayName("Write null graph to TTL String")
    @SneakyThrows
    void writeNullGraphToTTLString() {
        assertNull(GraphHelper.writeGraphToTurtleString(null));
    }

    @Test
    @DisplayName("Write closed graph to TTL String")
    @SneakyThrows
    void writeClosedGraphtoTTLString() {
        Graph graph = ModelFactory.createDefaultModel().getGraph();
        graph.add(new Triple(NodeFactory.createURI("<#b>"), NodeFactory.createURI("<#c>"), NodeFactory.createURI("<#d>")));
        graph.close();
        assertNull(GraphHelper.writeGraphToTurtleString(graph));
    }


}
