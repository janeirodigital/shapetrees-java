package com.janeirodigital.shapetrees.test;

import lombok.SneakyThrows;
import org.junit.jupiter.api.*;

import java.net.URI;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class GitHubDeepTests extends BaseShapeTreeTest {

    @Test
    @Order(1)
    @DisplayName("Create /Git/")
    @SneakyThrows
    void plantGit() {
        plant(new URI(ROOT_PATH), new URI("http://localhost:9999/static/gh-deep/gh-deep-ShapeTree.jsonld#root"), "Git");
        ensureExistsHasMetadataWithPredicateValue(new URI(ROOT_PATH + "Git/"), new URI(SHAPE_TREE_INSTANCE_PATH_PREDICATE), ".");
    }

    @Test
    @Order(2)
    @DisplayName("Create /Git/users/ericprud/")
    @SneakyThrows
    void createUserEricPrud() {
        postContent(new URI(ROOT_PATH + "Git/users/"), "ericprud", true, "target/test-classes/test-data/apps/gh-deep/ericprud-user.ttl", "#ericprud");
        ensureExistsWithPredicateValue(new URI(ROOT_PATH + "Git/users/ericprud/"), new URI("http://github.example/ns#node_id"), "MDQ6VXNlcjU3MzQ3OA==");
        ensureExists(new URI(ROOT_PATH + "Git/users/ericprud/subscriptions/"));
    }

    @Test
    @Order(3)
    @DisplayName("Create /Git/users/ericprud/subscriptions/")
    @SneakyThrows
    void createGitUserEricPrudSubscriptions() {
        postContent(new URI(ROOT_PATH + "Git/users/ericprud/subscriptions/"), "subscr1.ttl", false, "target/test-classes/test-data/apps/gh-deep/libxml-annot-repo.ttl", "#libxml-annot");
        ensureExistsWithPredicateValue(new URI(ROOT_PATH + "Git/users/ericprud/subscriptions/subscr1.ttl"), new URI("http://github.example/ns#subscription_url"), new URI("https://api.github.com/repos/ericprud/libxml-annot/subscription"));
    }

}
