package com.janeirodigital.shapetrees.test;

import lombok.SneakyThrows;
import org.junit.jupiter.api.*;

import java.net.URI;
import java.util.Collections;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class ContainerMembershipTests extends BaseShapeTreeTest {
    public ContainerMembershipTests() {
        super(new MockEcosystem());
    }

    @Order(1)
    @SneakyThrows
    @DisplayName("Default Behavior")
    @Test
    void plantDefaultExpectAllowNoneDefault() {
        plant(new URI(ROOT_PATH), Collections.singletonList(new URI("http://localhost:9999/static/containers/ContainerMembershipAllowShapeTree.ttl#things-default")), "things", null);
        postContent(new URI(ROOT_PATH+"things/"), "thing-stuff", false, "target/test-classes/test-data/apps/gh-deep/ericprud-user.ttl", "#ericprud");
        postContent(new URI(ROOT_PATH+"things/"), "stuff-resource", false, "target/test-classes/test-data/apps/gh-deep/ericprud-user.ttl", "#ericprud", 422);
        postContent(new URI(ROOT_PATH+"things/"), "stuff-container", true, "target/test-classes/test-data/apps/gh-deep/ericprud-user.ttl", "#ericprud", 422);
        postContent(new URI(ROOT_PATH+"things/"), "stuff-nonrdf", false, "target/test-classes/test-data/apps/nevernote/img-M33_IR.jpg", null, "image/jpeg", 422);
    }

    @Order(2)
    @SneakyThrows
    @DisplayName("AllowNone")
    @Test
    void plantDefaultExpectAllowNone() {
        plant(new URI(ROOT_PATH), Collections.singletonList(new URI("http://localhost:9999/static/containers/ContainerMembershipAllowShapeTree.ttl#things-allow-none")), "things-2", null);
        postContent(new URI(ROOT_PATH+"things-2/"), "thing-stuff", false, "target/test-classes/test-data/apps/gh-deep/ericprud-user.ttl", "#ericprud");
        postContent(new URI(ROOT_PATH+"things-2/"), "stuff-resource", false, "target/test-classes/test-data/apps/gh-deep/ericprud-user.ttl", "#ericprud", 422);
        postContent(new URI(ROOT_PATH+"things-2/"), "stuff-container", true, "target/test-classes/test-data/apps/gh-deep/ericprud-user.ttl", "#ericprud", 422);
        postContent(new URI(ROOT_PATH+"things-2/"), "stuff-nonrdf", false, "target/test-classes/test-data/apps/nevernote/img-M33_IR.jpg", null, "image/jpeg", 422);
    }

    @Order(3)
    @SneakyThrows
    @DisplayName("AllowAll")
    @Test
    void plantDefaultExpectAllowAll() {
        plant(new URI(ROOT_PATH), Collections.singletonList(new URI("http://localhost:9999/static/containers/ContainerMembershipAllowShapeTree.ttl#things-allow-all")), "things-3", null);
        postContent(new URI(ROOT_PATH+"things-3/"), "thing-stuff", false, "target/test-classes/test-data/apps/gh-deep/ericprud-user.ttl", "#ericprud");
        postContent(new URI(ROOT_PATH+"things-3/"), "stuff-resource", false, "target/test-classes/test-data/apps/gh-deep/ericprud-user.ttl", "#ericprud", 201);
        postContent(new URI(ROOT_PATH+"things-3/"), "stuff-container", true, "target/test-classes/test-data/apps/gh-deep/ericprud-user.ttl", "#ericprud", 201);
        postContent(new URI(ROOT_PATH+"things-3/"), "stuff-nonrdf", false, "target/test-classes/test-data/apps/nevernote/img-M33_IR.jpg", null, "image/jpeg", 201);
    }

    @Order(4)
    @SneakyThrows
    @DisplayName("AllowResources")
    @Test
    void plantDefaultExpectAllowResources() {
        plant(new URI(ROOT_PATH), Collections.singletonList(new URI("http://localhost:9999/static/containers/ContainerMembershipAllowShapeTree.ttl#things-allow-resources")), "things-4", null);
        postContent(new URI(ROOT_PATH+"things-4/"), "thing-stuff", false, "target/test-classes/test-data/apps/gh-deep/ericprud-user.ttl", "#ericprud");
        postContent(new URI(ROOT_PATH+"things-4/"), "stuff-resource", false, "target/test-classes/test-data/apps/gh-deep/ericprud-user.ttl", "#ericprud", 201);
        postContent(new URI(ROOT_PATH+"things-4/"), "stuff-container", true, "target/test-classes/test-data/apps/gh-deep/ericprud-user.ttl", "#ericprud", 422);
        postContent(new URI(ROOT_PATH+"things-4/"), "stuff-nonrdf", false, "target/test-classes/test-data/apps/nevernote/img-M33_IR.jpg", null, "image/jpeg",422);
    }

    @Order(5)
    @SneakyThrows
    @DisplayName("AllowContainers")
    @Test
    void plantDefaultExpectAllowContainers() {
        plant(new URI(ROOT_PATH), Collections.singletonList(new URI("http://localhost:9999/static/containers/ContainerMembershipAllowShapeTree.ttl#things-allow-containers")), "things-5", null);
        postContent(new URI(ROOT_PATH+"things-5/"), "thing-stuff", false, "target/test-classes/test-data/apps/gh-deep/ericprud-user.ttl", "#ericprud");
        postContent(new URI(ROOT_PATH+"things-5/"), "stuff-resource", false, "target/test-classes/test-data/apps/gh-deep/ericprud-user.ttl", "#ericprud", 422);
        postContent(new URI(ROOT_PATH+"things-5/"), "stuff-container", true, "target/test-classes/test-data/apps/gh-deep/ericprud-user.ttl", "#ericprud", 201);
        postContent(new URI(ROOT_PATH+"things-5/"), "stuff-nonrdf", false, "target/test-classes/test-data/apps/nevernote/img-M33_IR.jpg", null, "image/jpeg",422);
    }

    @Order(6)
    @SneakyThrows
    @DisplayName("AllowNonRDF")
    @Test
    void plantDefaultExpectAllowNonRDF() {
        plant(new URI(ROOT_PATH), Collections.singletonList(new URI("http://localhost:9999/static/containers/ContainerMembershipAllowShapeTree.ttl#things-allow-nonrdf")), "things-6", null);
        postContent(new URI(ROOT_PATH+"things-6/"), "thing-stuff", false, "target/test-classes/test-data/apps/gh-deep/ericprud-user.ttl", "#ericprud");
        postContent(new URI(ROOT_PATH+"things-6/"), "stuff-resource", false, "target/test-classes/test-data/apps/gh-deep/ericprud-user.ttl", "#ericprud", 422);
        postContent(new URI(ROOT_PATH+"things-6/"), "stuff-container", true, "target/test-classes/test-data/apps/gh-deep/ericprud-user.ttl", "#ericprud", 422);
        postContent(new URI(ROOT_PATH+"things-6/"), "stuff-nonrdf", false, "target/test-classes/test-data/apps/nevernote/img-M33_IR.jpg", null, "image/jpeg", 201);
    }

}
