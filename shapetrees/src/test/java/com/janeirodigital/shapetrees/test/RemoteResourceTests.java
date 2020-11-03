package com.janeirodigital.shapetrees.test;

import com.janeirodigital.shapetrees.RemoteResource;
import lombok.SneakyThrows;
import org.apache.jena.graph.Graph;
import org.apache.jena.graph.GraphUtil;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Triple;
import org.junit.jupiter.api.*;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class RemoteResourceTests extends BaseShapeTreeTest {

    public RemoteResourceTests() {
        super(new MockEcosystem());
    }
/*
    @Test
    @Order(1)
    @SneakyThrows
    @DisplayName("RemoteResource - Create Resource")
    void createResource() {
        putContent(new URI(ROOT_PATH+"testing.ttl"), false, "target/test-classes/test-data/apps/gh-deep/ericprud-user.ttl", null);
        RemoteResource resource = new RemoteResource(new URI(ROOT_PATH+"testing.ttl"), AUTH_HEADER_VALUE);
        assertEquals(resource.exists(), true);
    }

    @Test
    @Order(2)
    @SneakyThrows
    @DisplayName("RemoteResource - UpdateGraph with refresh")
    void updateResourceWithRefresh() {
        RemoteResource resource = new RemoteResource(new URI(ROOT_PATH+"testing.ttl"), AUTH_HEADER_VALUE);
        Graph existingGraph = resource.getGraph(new URI(ROOT_PATH+"testing.ttl"));
        List<Triple> triplesToAdd = new ArrayList<>();
        triplesToAdd.add(new Triple(NodeFactory.createURI(resource.getURI().toString()), NodeFactory.createURI("http://example.com/predicate"), NodeFactory.createLiteral("testing1")));
        GraphUtil.add(existingGraph, triplesToAdd);

        resource.updateGraph(existingGraph, true, AUTH_HEADER_VALUE);
        assertEquals(true, resource.getBody().contains("testing1"));
    }

    @Test
    @Order(3)
    @SneakyThrows
    @DisplayName("RemoteResource - UpdateGraph without refresh")
    void updateResourceWithoutRefresh() {
        RemoteResource resource = new RemoteResource(new URI(ROOT_PATH+"testing.ttl"), AUTH_HEADER_VALUE);
        Graph existingGraph = resource.getGraph(new URI(ROOT_PATH+"testing.ttl"));
        List<Triple> triplesToAdd = new ArrayList<>();
        triplesToAdd.add(new Triple(NodeFactory.createURI(resource.getURI().toString()), NodeFactory.createURI("http://example.com/predicate"), NodeFactory.createLiteral("testing2")));
        GraphUtil.add(existingGraph, triplesToAdd);

        resource.updateGraph(existingGraph, false, AUTH_HEADER_VALUE);
        assertEquals(true, resource.getBody().contains("testing2"));
    }
*/

}
