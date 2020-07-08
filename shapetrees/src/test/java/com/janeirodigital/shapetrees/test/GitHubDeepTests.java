package com.janeirodigital.shapetrees.test;

import com.janeirodigital.shapetrees.ShapeTreeVocabulary;
import lombok.SneakyThrows;
import org.junit.jupiter.api.*;

import java.net.URI;
import java.util.Collections;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class GitHubDeepTests extends BaseShapeTreeTest {

    public GitHubDeepTests() {
        super(new MockEcosystem());
    }

    @Test
    @Order(1)
    @DisplayName("Create /Git/")
    @SneakyThrows
    void plantGit() {
        plant(new URI(ROOT_PATH), Collections.singletonList(new URI("http://localhost:9999/static/gh-deep/gh-deep-ShapeTree.jsonld#root")), "Git", null);
        ensureExistsHasMetadataWithPredicateValue(new URI(ROOT_PATH + "Git/"), new URI(ShapeTreeVocabulary.HAS_SHAPE_TREE_INSTANCE_PATH), ".");
        ensureExistsHasMetadataWithValues(new URI(ROOT_PATH+"Git/"), ".", new URI(ROOT_PATH+"Git/"));
        ensureExistsHasMetadataWithValues(new URI(ROOT_PATH+"Git/repos/"), "./repos/", new URI(ROOT_PATH+"Git/"));
        ensureExistsHasMetadataWithValues(new URI(ROOT_PATH+"Git/users/"), "./users/", new URI(ROOT_PATH+"Git/"));
    }

    @Test
    @Order(2)
    @DisplayName("Create /Git/users/ericprud/")
    @SneakyThrows
    void createUserEricPrud() {
        postContent(new URI(ROOT_PATH + "Git/users/"), "ericprud", true, "target/test-classes/test-data/apps/gh-deep/ericprud-user.ttl", "#ericprud");
        ensureExistsWithPredicateValue(new URI(ROOT_PATH + "Git/users/ericprud/"), new URI("http://github.example/ns#node_id"), "MDQ6VXNlcjU3MzQ3OA==");
        ensureExistsHasMetadataWithValues(new URI(ROOT_PATH+"Git/users/ericprud/"), "./users/ericprud/", new URI(ROOT_PATH+"Git/"));
        ensureExistsHasMetadataWithValues(new URI(ROOT_PATH+"Git/users/ericprud/followers/"), "./users/ericprud/followers/", new URI(ROOT_PATH+"Git/"));
        ensureExistsHasMetadataWithValues(new URI(ROOT_PATH+"Git/users/ericprud/orgs/"), "./users/ericprud/orgs/", new URI(ROOT_PATH+"Git/"));
        ensureExistsHasMetadataWithValues(new URI(ROOT_PATH+"Git/users/ericprud/received_events/"), "./users/ericprud/received_events/", new URI(ROOT_PATH+"Git/"));
        ensureExistsHasMetadataWithValues(new URI(ROOT_PATH+"Git/users/ericprud/repos/"), "./users/ericprud/repos/", new URI(ROOT_PATH+"Git/"));
        ensureExistsHasMetadataWithValues(new URI(ROOT_PATH+"Git/users/ericprud/subscriptions/"), "./users/ericprud/subscriptions/", new URI(ROOT_PATH+"Git/"));
    }

    @Test
    @Order(3)
    @DisplayName("Create /Git/users/ericprud/subscriptions/")
    @SneakyThrows
    void createGitUserEricPrudSubscriptions() {
        postContent(new URI(ROOT_PATH + "Git/users/ericprud/subscriptions/"), "subscr1.ttl", false, "target/test-classes/test-data/apps/gh-deep/libxml-annot-repo.ttl", "#libxml-annot");
        ensureExistsWithPredicateValue(new URI(ROOT_PATH + "Git/users/ericprud/subscriptions/subscr1.ttl"), new URI("http://github.example/ns#subscription_url"), new URI("https://api.github.com/repos/ericprud/libxml-annot/subscription"));
        // TODO where is the metadata for the subscr1.ttl?
    }

    @Test
    @Order(4)
    @DisplayName("Create /Git/repos/ericprud/ hierarchy")
    @SneakyThrows
    void createEricPrudRepo() {
        postContent(new URI(ROOT_PATH + "Git/repos/"), "ericprud", true, "target/test-classes/test-data/apps/gh-deep/ericprud-org.ttl", "#ericprud");
        ensureExistsHasMetadataWithPredicateValue(new URI(ROOT_PATH + "Git/repos/ericprud/"), new URI(ShapeTreeVocabulary.HAS_SHAPE_TREE_INSTANCE_PATH), "./repos/ericprud/");
        ensureExistsHasMetadataWithValues(new URI(ROOT_PATH+"Git/repos/ericprud/"), "./repos/ericprud/", new URI(ROOT_PATH+"Git/"));
    }

    @Test
    @Order(5)
    @DisplayName("Create /Git/repos/ericprud/jsg/")
    @SneakyThrows
    void createJSGRepo() {
        postContent(new URI(ROOT_PATH + "Git/repos/ericprud/"), "jsg", true, "target/test-classes/test-data/apps/gh-deep/jsg.ttl", "#jsg");
        ensureExistsWithPredicateValue(new URI(ROOT_PATH + "Git/repos/ericprud/jsg/"), new URI("http://github.example/ns#id"), 46061512);
        ensureExistsHasMetadataWithValues(new URI(ROOT_PATH+"Git/repos/ericprud/jsg/"), "./repos/ericprud/jsg/", new URI(ROOT_PATH+"Git/"));
        ensureExistsHasMetadataWithValues(new URI(ROOT_PATH+"Git/repos/ericprud/jsg/issues/"), "./repos/ericprud/jsg/issues/", new URI(ROOT_PATH+"Git/"));
        ensureExistsHasMetadataWithValues(new URI(ROOT_PATH+"Git/repos/ericprud/jsg/labels/"), "./repos/ericprud/jsg/labels/", new URI(ROOT_PATH+"Git/"));
        ensureExistsHasMetadataWithValues(new URI(ROOT_PATH+"Git/repos/ericprud/jsg/milestones/"), "./repos/ericprud/jsg/milestones/", new URI(ROOT_PATH+"Git/"));
    }

    @Test
    @Order(6)
    @DisplayName("Create /Git/repos/ericprud/jsg/issues/1")
    @SneakyThrows
    void createJSGIssue() {
        postContent(new URI(ROOT_PATH + "Git/repos/ericprud/jsg/issues/"), "1.ttl", false, "target/test-classes/test-data/apps/gh-deep/jsg-issue1.ttl", "#issue1");
        ensureExistsWithPredicateValue(new URI(ROOT_PATH + "Git/repos/ericprud/jsg/issues/1.ttl"), new URI("http://github.example/ns#author_association"), "OWNER");
        //ensureExistsHasMetadataWithValues(new URI(ROOT_PATH+"Git/repos/ericprud/jsg/issues/1.ttl"), "./repos/ericprud/jsg/", new URI(ROOT_PATH+"Git/"));
    }

}
