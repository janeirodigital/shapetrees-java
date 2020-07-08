package com.janeirodigital.solid.interoperability.tests;

import com.janeirodigital.shapetrees.ShapeTreeFactory;
import com.janeirodigital.shapetrees.test.BaseShapeTreeTest;
import com.janeirodigital.solid.interoperability.SolidInteroperabilityEcosystem;
import lombok.SneakyThrows;
import org.junit.jupiter.api.*;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class EcosystemTests extends BaseShapeTreeTest {

    public EcosystemTests() {
        super(new SolidInteroperabilityEcosystem());
    }

    @Order(1)
    @Test
    @DisplayName("Parse Ecosystem ShapeTree")
    @SneakyThrows
    void parseEcosystemShapeTree() {
        ShapeTreeFactory.getShapeTree(new URI("http://localhost:9999/static/ecosystem/ecosystem-ShapeTree.ttl#agent-tree"));
        ShapeTreeFactory.getShapeTree(new URI("http://localhost:9999/static/ecosystem/ecosystem-ShapeTree.ttl#registrar-tree"));
        ShapeTreeFactory.getShapeTree(new URI("http://localhost:9999/static/ecosystem/ecosystem-ShapeTree.ttl#application-registry-set-tree"));
        ShapeTreeFactory.getShapeTree(new URI("http://localhost:9999/static/ecosystem/ecosystem-ShapeTree.ttl#data-registry-set-tree"));
        ShapeTreeFactory.getShapeTree(new URI("http://localhost:9999/static/ecosystem/ecosystem-ShapeTree.ttl#application-registry-tree"));
        ShapeTreeFactory.getShapeTree(new URI("http://localhost:9999/static/ecosystem/ecosystem-ShapeTree.ttl#application-registration-tree"));
        ShapeTreeFactory.getShapeTree(new URI("http://localhost:9999/static/ecosystem/ecosystem-ShapeTree.ttl#data-registry-tree"));
        ShapeTreeFactory.getShapeTree(new URI("http://localhost:9999/static/ecosystem/ecosystem-ShapeTree.ttl#data-registration-tree"));
    }

    @Order(2)
    @Test
    @DisplayName("Plant Agent Tree")
    @SneakyThrows
    void plantAgentTree() {
        plant(new URI(ROOT_PATH), Collections.singletonList(new URI("http://localhost:9999/static/ecosystem/ecosystem-ShapeTree.ttl#profile-tree")), "profile", null);
    }

    @Order(3)
    @Test
    @DisplayName("POST Agent ID Doc - Validation FAILURE")
    @SneakyThrows
    void postAgentIdFailure() {
        postContent(new URI(ROOT_PATH+"profile/"), "id", false, "target/test-classes/test-data/ecosystem/agent-profile-bad.ttl", "#me", 422);
    }

    @Order(4)
    @Test
    @DisplayName("POST Agent ID Doc - Validation SUCCESS")
    @SneakyThrows
    void postAgentIdSuccess() {
        postContent(new URI(ROOT_PATH+"profile/"), "id", false, "target/test-classes/test-data/ecosystem/agent-profile.ttl", "#me", 201);
    }

    @Order(5)
    @Test
    @DisplayName("Plant Registrar Tree - Validation FAILURE")
    @SneakyThrows
    void plantRegistrarTreeFailure() {
        plantWithResourceContent(new URI(ROOT_PATH), Collections.singletonList(new URI("http://localhost:9999/static/ecosystem/ecosystem-ShapeTree.ttl#registrar-tree")), "registries", "target/test-classes/test-data/ecosystem/registries-bad.ttl", "text/turtle", "#registrar", 422 );
    }


    @Order(6)
    @Test
    @DisplayName("Plant Registrar Tree - Validation SUCCESS")
    @SneakyThrows
    void plantRegistrarTreeSuccess() {
        plantWithResourceContent(new URI(ROOT_PATH), Collections.singletonList(new URI("http://localhost:9999/static/ecosystem/ecosystem-ShapeTree.ttl#registrar-tree")), "registries", "target/test-classes/test-data/ecosystem/registries.ttl", "text/turtle", "#registrar", 201 );
    }

    @Order(7)
    @Test
    @DisplayName("Create Data Registry Set - Validation FAILURE")
    @SneakyThrows
    void putDataRegistrySetFailure() {
        putContent(new URI(ROOT_PATH+"registries/data"), false, "target/test-classes/test-data/ecosystem/data-registry-set-bad.ttl", "#set", 422);
    }

    @Order(8)
    @Test
    @DisplayName("Create Data Registry Set - Validation SUCCESS")
    @SneakyThrows
    void putDataRegistrySetSuccess() {
        putContent(new URI(ROOT_PATH+"registries/data"), false, "target/test-classes/test-data/ecosystem/data-registry-set.ttl", "#set", 201);
    }

    @Order(9)
    @Test
    @DisplayName("Create Data Registry - Validation FAILURE")
    @SneakyThrows
    void plantDataRegistryFailure() {
        plantWithResourceContent(new URI(ROOT_PATH), Collections.singletonList(new URI("http://localhost:9999/static/ecosystem/ecosystem-ShapeTree.ttl#data-registry-tree")), "data", "target/test-classes/test-data/ecosystem/empty-data-registry-bad.ttl", "text/turtle", "#registry", 422 );
    }

    @Order(10)
    @Test
    @DisplayName("Create Data Registry - Validation SUCCESS")
    @SneakyThrows
    void plantDataRegistrySuccess() {
        plantWithResourceContent(new URI(ROOT_PATH), Collections.singletonList(new URI("http://localhost:9999/static/ecosystem/ecosystem-ShapeTree.ttl#data-registry-tree")), "data", "target/test-classes/test-data/ecosystem/empty-data-registry.ttl", "text/turtle", "#registry", 201 );
    }

    @Order(11)
    @Test
    @DisplayName("Plant Git Orgs - Validation SUCCESS")
    @SneakyThrows
    void plantGitOrgsSuccess() {
        List<URI> shapeTreesToPlant = new ArrayList<>();
        shapeTreesToPlant.add(new URI("http://localhost:9999/static/gh-flat/gh-flat-ShapeTree.jsonld#orgs"));
        shapeTreesToPlant.add(new URI("http://localhost:9999/static/ecosystem/ecosystem-ShapeTree.ttl#data-registration-tree"));
        plant(new URI(ROOT_PATH+"data/"), shapeTreesToPlant, "gitorgs", "#registration");
    }

    @Order(12)
    @Test
    @DisplayName("Plant Git Orgs - Validation SUCCESS")
    @SneakyThrows
    void plantGitOrgsSuccessAgain() {
        List<URI> shapeTreesToPlant = new ArrayList<>();
        shapeTreesToPlant.add(new URI("http://localhost:9999/static/gh-flat/gh-flat-ShapeTree.jsonld#orgs"));
        shapeTreesToPlant.add(new URI("http://localhost:9999/static/ecosystem/ecosystem-ShapeTree.ttl#data-registration-tree"));
        plant(new URI(ROOT_PATH+"data/"), shapeTreesToPlant, "gitorgs", "#registration");
    }

}
