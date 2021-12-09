package com.janeirodigital.shapetrees.tests;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.helpers.GraphHelper;
import lombok.SneakyThrows;
import org.apache.jena.graph.Graph;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Triple;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.riot.Lang;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;

import java.net.URI;
import java.time.OffsetDateTime;

import static com.janeirodigital.shapetrees.core.helpers.GraphHelper.newTriple;

class GraphHelperTests {

    @ParameterizedTest
    @NullAndEmptySource
    @DisplayName("Handle null or empty content types with defaults")
    @SneakyThrows
    void handleNullOrEmptyContentTypes(String type) {
        Lang lang = GraphHelper.getLangForContentType(type);
        Assertions.assertEquals(lang, Lang.TURTLE);
    }

    @ParameterizedTest
    @ValueSource(strings = {"text/turtle","something/bogus"})
    @DisplayName("Handle turtle content type when specified or as default")
    @SneakyThrows
    void handleTurtleContentType(String type) {
        Lang lang = GraphHelper.getLangForContentType(type);
        Assertions.assertEquals(lang, Lang.TURTLE);
    }

    @Test
    @DisplayName("JSON LD content type")
    @SneakyThrows
    void handleJsonLD() {
        Lang lang = GraphHelper.getLangForContentType("application/ld+json");
        Assertions.assertEquals(lang, Lang.JSONLD);
    }

    @Test
    @DisplayName("N-Triples content type")
    @SneakyThrows
    void hanldeNTriples() {
        Lang lang = GraphHelper.getLangForContentType("application/n-triples");
        Assertions.assertEquals(lang, Lang.NTRIPLES);
    }

    @Test
    @DisplayName("rdf+xml content type")
    @SneakyThrows
    void hanldeRDFXMLTriples() {
        Lang lang = GraphHelper.getLangForContentType("application/rdf+xml");
        Assertions.assertEquals(lang, Lang.RDFXML);
    }

    @Test
    @DisplayName("Parse invalid TTL")
    @SneakyThrows
    void parseInvalidTTL() {
        String invalidTtl = "<#a> b c";
        Assertions.assertThrows(ShapeTreeException.class, () -> GraphHelper.readStringIntoGraph(URI.create("https://example.com/a"), invalidTtl, "text/turtle"));
    }

    @Test
    @DisplayName("Parse valid TTL")
    @SneakyThrows
    void parseValidTTL() {
        String invalidTtl = "<#a> <#b> <#c> .";
        Assertions.assertNotNull(GraphHelper.readStringIntoGraph(URI.create("https://example.com/a"), invalidTtl, "text/turtle"));
    }


    @Test
    @DisplayName("Write graph to TTL String")
    @SneakyThrows
    void writeGraphToTTLString() {
        Graph graph = ModelFactory.createDefaultModel().getGraph();
        graph.add(new Triple(NodeFactory.createURI("<#b>"), NodeFactory.createURI("<#c>"), NodeFactory.createURI("<#d>")));
        Assertions.assertNotNull(GraphHelper.writeGraphToTurtleString(graph));
    }

    @Test
    @DisplayName("Write null graph to TTL String")
    @SneakyThrows
    void writeNullGraphToTTLString() {
        Assertions.assertNull(GraphHelper.writeGraphToTurtleString(null));
    }

    @Test
    @DisplayName("Write closed graph to TTL String")
    @SneakyThrows
    void writeClosedGraphtoTTLString() {
        Graph graph = ModelFactory.createDefaultModel().getGraph();
        graph.add(new Triple(NodeFactory.createURI("<#b>"), NodeFactory.createURI("<#c>"), NodeFactory.createURI("<#d>")));
        graph.close();
        Assertions.assertNull(GraphHelper.writeGraphToTurtleString(graph));
    }

    @Test
    @DisplayName("Fail to store null string objects in new Triple helper")
    @SneakyThrows
    void failToStoreNullTripleStringObjects() {
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

    @Test
    @DisplayName("Fail to store null URI objects in new Triple helper")
    @SneakyThrows
    void failToStoreNullTripleURIObjects() {

        URI subjectURI = URI.create("https://site.example/#a");
        URI predicateURI = URI.create("https://site.example/#b");
        URI objectURI = URI.create("https://site.example/#c");

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

    @Test
    @DisplayName("Store URI as subject and predicate with new Triple helper")
    @SneakyThrows
    void storeURISubjectAndPredicate() {
        Triple uriTriple = newTriple(URI.create("https://site.example/#a"), URI.create("https://site.example/#b"), URI.create("https://site.example/#a"));
        Assertions.assertNotNull(uriTriple);
        Assertions.assertTrue(uriTriple.getSubject().isURI());
        Assertions.assertTrue(uriTriple.getPredicate().isURI());
        Assertions.assertTrue(uriTriple.getObject().isURI());
    }

    @Test
    @DisplayName("Store URI object with new Triple helper")
    @SneakyThrows
    void storeURIasTripleObject() {
        Triple uriTriple = newTriple("https://site.example/#a", "https://site.example/#b", URI.create("https://site.example/#c"));
        Assertions.assertNotNull(uriTriple);
        Assertions.assertTrue(uriTriple.getObject().isURI());
    }

    @Test
    @DisplayName("Store String object with new Triple helper")
    @SneakyThrows
    void storeStringAsTripleObject() {
        Triple uriTriple = newTriple("https://site.example/#a", "https://site.example/#b", "This is a test string");
        Assertions.assertNotNull(uriTriple);
        Assertions.assertTrue(uriTriple.getObject().isLiteral());
    }

    @Test
    @DisplayName("Store DateTime object with new Triple helper")
    @SneakyThrows
    void storeDateTimeAsTripleObject() {
        Triple uriTriple = newTriple("https://site.example/#a", "https://site.example/#b", OffsetDateTime.now());
        Assertions.assertNotNull(uriTriple);
        Assertions.assertTrue(uriTriple.getObject().isLiteral());
    }

    @Test
    @DisplayName("Store Boolean object with new Triple helper")
    @SneakyThrows
    void storeBooleanAsTripleObject() {
        Triple uriTriple = newTriple("https://site.example/#a", "https://site.example/#b", Boolean.TRUE);
        Assertions.assertNotNull(uriTriple);
        Assertions.assertTrue(uriTriple.getObject().isLiteral());
    }

    @Test
    @DisplayName("Store Blank Node with new Triple helper")
    @SneakyThrows
    void storeBlankNodeAsTripleObject() {
        Triple uriTriple = newTriple("https://site.example/#a", "https://site.example/#b", NodeFactory.createBlankNode());
        Assertions.assertNotNull(uriTriple);
        Assertions.assertTrue(uriTriple.getObject().isBlank());
    }

    @Test
    @DisplayName("Fail to store Unsupported Type with new Triple helper")
    @SneakyThrows
    void failedToStoreUnsupportedTypeAsTripleObject() {
        Assertions.assertThrows(ShapeTreeException.class, () -> {
            newTriple("https://site.example/#a", "https://site.example/#b", (float) 35.6);
        });
    }

}
