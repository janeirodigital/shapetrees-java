package com.janeirodigital.shapetrees.test;

import lombok.SneakyThrows;
import org.junit.jupiter.api.*;

import java.net.URI;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class ShapeTreeTests extends BaseShapeTreeTest {

    @Order(1)
    @SneakyThrows
    @DisplayName("Attempt Delete Root")
    @Test
    void attemptDeleteRoot() {
        delete(new URI(SERVER_ROOT), 403);
    }

    @Order(2)
    @SneakyThrows
    @DisplayName("Attempt Delete Nonexistent")
    @Test
    void attemptDeleteDoesNotExist() {
        delete(new URI(ROOT_PATH+"doesnotexist"), 404);
    }

    @Order(3)
    @SneakyThrows
    @DisplayName("PLANT - should fail with bad turtle")
    @Test
    void plantBadTurtle() {
        plantWithStringContent(new URI(ROOT_PATH), new URI("http://localhost:9999/static/cal/GoogleShapeTree.jsonld#top"), "ShouldNotExist", "@prefix x: <> @@bad Turtle@@", "text/turtle", "#", 422);
    }

    @Order(4)
    @SneakyThrows
    @DisplayName("PLANT - should fail with bad JSON")
    @Test
    void plantBadJSON() {
        plantWithStringContent(new URI(ROOT_PATH), new URI("http://localhost:9999/static/cal/GoogleShapeTree.jsonld#top"), "ShouldNotExist", "{ \"foo\": 1, \"@id\": 2@@bad JSON}", "application/ld+json", "#", 422);
    }

    @Order(5)
    @SneakyThrows
    @DisplayName("PLANT - should fail with bad JSONLD")
    @Test
    void plantBadJSONLD() {
        plantWithStringContent(new URI(ROOT_PATH), new URI("http://localhost:9999/static/cal/GoogleShapeTree.jsonld#top"), "ShouldNotExist", "{ \"foo\": 1, \"@id\": 2}", "application/ld+json", "#", 422);
    }

    @Order(6)
    @SneakyThrows
    @DisplayName("PUT Tests - plant ShapeMaps-PUT-tests/")
    @Test
    void putTestsplantPUTTests() {
        plant(new URI(ROOT_PATH), new URI("http://localhost:9999/static/gh-deep/gh-deep-ShapeTree.jsonld#root"), "ShapeMaps-PUT-tests");
        ensureExistsHasMetadataWithPredicateValue(new URI(ROOT_PATH+"ShapeMaps-PUT-tests/"), new URI(SHAPE_TREE_INSTANCE_PATH_PREDICATE), ".");
    }

    @Order(7)
    @SneakyThrows
    @DisplayName("PUT Tests - post ShapeMaps-PUT-tests/users/ericprud/")
    @Test
    void putTestspostUsersEricPrud() {
        postContent(new URI(ROOT_PATH+"ShapeMaps-PUT-tests/users/"), "ericprud", true, "target/test-classes/test-data/apps/gh-deep/ericprud-user.ttl", "#ericprud");
        ensureExistsWithPredicateValue(new URI(ROOT_PATH + "ShapeMaps-PUT-tests/users/ericprud/"), new URI("http://github.example/ns#node_id"), "MDQ6VXNlcjU3MzQ3OA==");
        ensureExistsHasMetadataWithValues(new URI(ROOT_PATH+"ShapeMaps-PUT-tests/users/ericprud/"), "./users/ericprud/", new URI(ROOT_PATH+"ShapeMaps-PUT-tests/"));
        ensureExistsHasMetadataWithValues(new URI(ROOT_PATH+"ShapeMaps-PUT-tests/users/ericprud/followers/"), "./users/ericprud/followers/", new URI(ROOT_PATH+"ShapeMaps-PUT-tests/"));
        ensureExistsHasMetadataWithValues(new URI(ROOT_PATH+"ShapeMaps-PUT-tests/users/ericprud/orgs/"), "./users/ericprud/orgs/", new URI(ROOT_PATH+"ShapeMaps-PUT-tests/"));
        ensureExistsHasMetadataWithValues(new URI(ROOT_PATH+"ShapeMaps-PUT-tests/users/ericprud/received_events/"), "./users/ericprud/received_events/", new URI(ROOT_PATH+"ShapeMaps-PUT-tests/"));
        ensureExistsHasMetadataWithValues(new URI(ROOT_PATH+"ShapeMaps-PUT-tests/users/ericprud/repos/"), "./users/ericprud/repos/", new URI(ROOT_PATH+"ShapeMaps-PUT-tests/"));
        ensureExistsHasMetadataWithValues(new URI(ROOT_PATH+"ShapeMaps-PUT-tests/users/ericprud/subscriptions/"), "./users/ericprud/subscriptions/", new URI(ROOT_PATH+"ShapeMaps-PUT-tests/"));
    }

    @Order(8)
    @SneakyThrows
    @DisplayName("PUT Tests - post ShapeMaps-PUT-tests/users/ericprud/subscriptions")
    @Test
    void putTestpostsubscription() {
        postContent(new URI(ROOT_PATH + "ShapeMaps-PUT-tests/users/ericprud/subscriptions/"), "subscr1.ttl", false, "target/test-classes/test-data/apps/gh-deep/libxml-annot-repo.ttl", "#libxml-annot");
        ensureExistsWithPredicateValue(new URI(ROOT_PATH + "ShapeMaps-PUT-tests/users/ericprud/subscriptions/subscr1.ttl"), new URI("http://github.example/ns#subscription_url"), new URI("https://api.github.com/repos/ericprud/libxml-annot/subscription"));
    }

    @Order(9)
    @SneakyThrows
    @DisplayName("PUT Tests - post ericprud-1")
    @Test
    void putTestPostEricPrud1() {
        // TODO:  describe(`post ${Path.join('/', installDir, '/')}ShapeMaps-PUT-tests/users/ericprud-1/`, () => {
        // Not sure this is an accurate test
    }

    @Test
    @Order(10)
    @DisplayName("PUT Tests - Create /ShapeMaps-PUT-tests/repos/ericprud/ hierarchy")
    @SneakyThrows
    void putTestCreateReposEricPrud() {
        postContent(new URI(ROOT_PATH + "ShapeMaps-PUT-tests/repos/"), "ericprud", true, "target/test-classes/test-data/apps/gh-deep/ericprud-org.ttl", "#ericprud");
        ensureExistsHasMetadataWithPredicateValue(new URI(ROOT_PATH + "ShapeMaps-PUT-tests/repos/ericprud/"), new URI(SHAPE_TREE_INSTANCE_PATH_PREDICATE), "./repos/ericprud/");
        ensureExistsHasMetadataWithValues(new URI(ROOT_PATH+"ShapeMaps-PUT-tests/repos/ericprud/"), "./repos/ericprud/", new URI(ROOT_PATH+"ShapeMaps-PUT-tests/"));
    }

    @Test
    @Order(11)
    @DisplayName("PUT Tests - Create /ShapeMaps-PUT-tests/repos/ericprud/jsg/")
    @SneakyThrows
    void putTestcreateJSGRepo() {
        postContent(new URI(ROOT_PATH + "ShapeMaps-PUT-tests/repos/ericprud/"), "jsg", true, "target/test-classes/test-data/apps/gh-deep/jsg.ttl", "#jsg");
        ensureExistsWithPredicateValue(new URI(ROOT_PATH + "ShapeMaps-PUT-tests/repos/ericprud/jsg/"), new URI("http://github.example/ns#id"), 46061512);
        ensureExistsHasMetadataWithValues(new URI(ROOT_PATH+"ShapeMaps-PUT-tests/repos/ericprud/jsg/"), "./repos/ericprud/jsg/", new URI(ROOT_PATH+"ShapeMaps-PUT-tests/"));
        ensureExistsHasMetadataWithValues(new URI(ROOT_PATH+"ShapeMaps-PUT-tests/repos/ericprud/jsg/issues/"), "./repos/ericprud/jsg/issues/", new URI(ROOT_PATH+"ShapeMaps-PUT-tests/"));
        ensureExistsHasMetadataWithValues(new URI(ROOT_PATH+"ShapeMaps-PUT-tests/repos/ericprud/jsg/labels/"), "./repos/ericprud/jsg/labels/", new URI(ROOT_PATH+"ShapeMaps-PUT-tests/"));
        ensureExistsHasMetadataWithValues(new URI(ROOT_PATH+"ShapeMaps-PUT-tests/repos/ericprud/jsg/milestones/"), "./repos/ericprud/jsg/milestones/", new URI(ROOT_PATH+"ShapeMaps-PUT-tests/"));
    }

    @Test
    @Order(12)
    @DisplayName("PUT Tests - Create /ShapeMaps-PUT-tests/repos/ericprud/jsg/issues/1")
    @SneakyThrows
    void putTestcreateJSGIssue() {
        postContent(new URI(ROOT_PATH + "ShapeMaps-PUT-tests/repos/ericprud/jsg/issues/"), "1.ttl", false, "target/test-classes/test-data/apps/gh-deep/jsg-issue1.ttl", "#issue1");
        ensureExistsWithPredicateValue(new URI(ROOT_PATH + "ShapeMaps-PUT-tests/repos/ericprud/jsg/issues/1.ttl"), new URI("http://github.example/ns#author_association"), "OWNER");
    }

    @Test
    @Order(13)
    @DisplayName("PUT Tests - Successful PUT to replace managed LDPR")
    @SneakyThrows
    void putTestReplaceManagedLDPR() {
        putContent(new URI(ROOT_PATH+"ShapeMaps-PUT-tests/repos/ericprud/jsg/issues/1.ttl"), false, "target/test-classes/test-data/shape-trees/jsg-issue1-03.ttl", "#issue1", 204);
        ensureExistsWithPredicateValue(new URI(ROOT_PATH+"ShapeMaps-PUT-tests/repos/ericprud/jsg/issues/1.ttl"), new URI("http://github.example/ns#updated_at"), "2019-12-18T03:00:00Z^^xsd:dateTime");
    }

    @Test
    @Order(14)
    @DisplayName("PUT Tests - Successful PUT to create managed LDRP")
    @SneakyThrows
    void putTestCreateManagedLDPR() {
        putContent(new URI(ROOT_PATH+"ShapeMaps-PUT-tests/repos/ericprud/jsg/issues/1-new.ttl"), false, "target/test-classes/test-data/apps/gh-deep/jsg-issue1.ttl", "#issue1", 201);
        ensureExistsWithPredicateValue(new URI(ROOT_PATH+"ShapeMaps-PUT-tests/repos/ericprud/jsg/issues/1-new.ttl"), new URI("http://github.example/ns#updated_at"), "2019-12-18T01:00:00Z^^xsd:dateTime");
    }

    @Test
    @Order(15)
    @DisplayName("PUT Tests - Successful PUT to create managed LDRC")
    @SneakyThrows
    void putTestReplacedManagedLDPC() {
        putContent(new URI(ROOT_PATH+"ShapeMaps-PUT-tests/repos/ericprud/jsg-put/"), true, "target/test-classes/test-data/shape-trees/jsg-put.ttl", "#jsg", 201);
        ensureExistsWithPredicateValue(new URI(ROOT_PATH+"ShapeMaps-PUT-tests/repos/ericprud/jsg-put/"), new URI("http://github.example/ns#updated_at"), "2019-12-18T03:00:00Z^^xsd:dateTime");
        ensureExistsHasMetadataWithValues(new URI(ROOT_PATH+"ShapeMaps-PUT-tests/repos/ericprud/jsg-put/"), "./repos/ericprud/jsg-put/", new URI(ROOT_PATH+"ShapeMaps-PUT-tests/"));
        ensureExistsHasMetadataWithValues(new URI(ROOT_PATH+"ShapeMaps-PUT-tests/repos/ericprud/jsg-put/issues/"), "./repos/ericprud/jsg-put/issues/", new URI(ROOT_PATH+"ShapeMaps-PUT-tests/"));
        ensureExistsHasMetadataWithValues(new URI(ROOT_PATH+"ShapeMaps-PUT-tests/repos/ericprud/jsg-put/labels/"), "./repos/ericprud/jsg-put/labels/", new URI(ROOT_PATH+"ShapeMaps-PUT-tests/"));
        ensureExistsHasMetadataWithValues(new URI(ROOT_PATH+"ShapeMaps-PUT-tests/repos/ericprud/jsg-put/milestones/"), "./repos/ericprud/jsg-put/milestones/", new URI(ROOT_PATH+"ShapeMaps-PUT-tests/"));
    }

    @Test
    @Order(16)
    @DisplayName("PUT Tests - to unmanaged container")
    @SneakyThrows
    void putTestUnmanagedContainer() {
        putContent(new URI(ROOT_PATH+"Unmanaged/issues/"), true, "target/test-classes/test-data/shape-trees/jsg-02.ttl", "#jsg", 201);
        ensureExistsWithPredicateValue(new URI(ROOT_PATH+"Unmanaged/issues/"), new URI("http://github.example/ns#updated_at"), "2019-12-18T02:00:00Z^^xsd:dateTime");
    }

    @Test
    @Order(17)
    @DisplayName("PUT Tests - to replace unmanaged container")
    @SneakyThrows
    void putTestReplaceUnmanagedContainer() {
        putContent(new URI(ROOT_PATH+"Unmanaged/issues/"), true, "target/test-classes/test-data/shape-trees/jsg-03.ttl", "#jsg", 201);
        ensureExistsWithPredicateValue(new URI(ROOT_PATH+"Unmanaged/issues/"), new URI("http://github.example/ns#updated_at"), "2019-12-18T03:00:00Z^^xsd:dateTime");
    }

    @Test
    @Order(18)
    @DisplayName("PUT Tests - to create unmanaged LDPR")
    @SneakyThrows
    void putTestCreateUnmanagedResource() {
        putContent(new URI(ROOT_PATH+"Unmanaged/issues/1.ttl"), false, "target/test-classes/test-data/apps/gh-deep/jsg-issue1.ttl", "#issue1", 201);
        ensureExistsWithPredicateValue(new URI(ROOT_PATH+"Unmanaged/issues/1.ttl"), new URI("http://github.example/ns#updated_at"), "2019-12-18T01:00:00Z^^xsd:dateTime");
    }

    @Test
    @Order(19)
    @DisplayName("PUT Tests - to replace unmanaged LDPR")
    @SneakyThrows
    void putTestUpdateUnmanagedResource() {
        putContent(new URI(ROOT_PATH+"Unmanaged/issues/1.ttl"), false, "target/test-classes/test-data/shape-trees/jsg-issue1-03.ttl", "#issue1", 204);
        ensureExistsWithPredicateValue(new URI(ROOT_PATH+"Unmanaged/issues/1.ttl"), new URI("http://github.example/ns#updated_at"), "2019-12-18T03:00:00Z^^xsd:dateTime");
    }




}
