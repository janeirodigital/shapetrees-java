package com.janeirodigital.shapetrees.test;

import com.janeirodigital.shapetrees.ShapeTreeFactory;
import lombok.SneakyThrows;
import org.junit.jupiter.api.*;

import java.net.URI;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class EcoSystemTests extends BaseShapeTreeTest {

    @Order(1)
    @Test
    @DisplayName("Parse Ecosystem ShapeTree")
    @SneakyThrows
    void parseEcosystemShapeTree() {
        ShapeTreeFactory.getShapeTreeStep(new URI("http://localhost:9999/static/ecosystem/ecosystem-ShapeTree.ttl#agent-tree"));
        ShapeTreeFactory.getShapeTreeStep(new URI("http://localhost:9999/static/ecosystem/ecosystem-ShapeTree.ttl#registrar-tree"));
        ShapeTreeFactory.getShapeTreeStep(new URI("http://localhost:9999/static/ecosystem/ecosystem-ShapeTree.ttl#application-registry-set-tree"));
        ShapeTreeFactory.getShapeTreeStep(new URI("http://localhost:9999/static/ecosystem/ecosystem-ShapeTree.ttl#data-registry-set-tree"));
        ShapeTreeFactory.getShapeTreeStep(new URI("http://localhost:9999/static/ecosystem/ecosystem-ShapeTree.ttl#application-registry-tree"));
        ShapeTreeFactory.getShapeTreeStep(new URI("http://localhost:9999/static/ecosystem/ecosystem-ShapeTree.ttl#application-registration-tree"));
        ShapeTreeFactory.getShapeTreeStep(new URI("http://localhost:9999/static/ecosystem/ecosystem-ShapeTree.ttl#data-registry-tree"));
        ShapeTreeFactory.getShapeTreeStep(new URI("http://localhost:9999/static/ecosystem/ecosystem-ShapeTree.ttl#data-registration-tree"));
    }

}
