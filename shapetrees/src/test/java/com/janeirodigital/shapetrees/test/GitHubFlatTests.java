package com.janeirodigital.shapetrees.test;

import com.janeirodigital.shapetrees.ShapeTreeVocabulary;
import lombok.SneakyThrows;
import org.junit.jupiter.api.*;

import java.net.URI;
import java.util.Collections;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class GitHubFlatTests extends BaseShapeTreeTest {

    public GitHubFlatTests() {
        super(new MockEcosystem());
    }

    @Test
    @DisplayName("Create Git-Orgs")
    @SneakyThrows
    @Order(1)
    void createGitOrgs()
    {
        plant(new URI(ROOT_PATH), Collections.singletonList(new URI("http://localhost:9999/static/gh-flat/gh-flat-ShapeTree.jsonld#orgs")), "Git-Orgs", null);
        ensureExistsHasMetadataWithPredicateValue(new URI(ROOT_PATH + "Git-Orgs/"), new URI(ShapeTreeVocabulary.HAS_SHAPE_TREE_INSTANCE_PATH), ".");
    }

    @Test
    @DisplayName("Create Git-Users")
    @SneakyThrows
    @Order(2)
    void createGitUsers() {
        plant(new URI(ROOT_PATH), Collections.singletonList(new URI("http://localhost:9999/static/gh-flat/gh-flat-ShapeTree.jsonld#users")), "Git-Users", null);
        ensureExistsHasMetadataWithPredicateValue(new URI(ROOT_PATH + "Git-Users/"), new URI(ShapeTreeVocabulary.HAS_SHAPE_TREE_INSTANCE_PATH), ".");
    }

    @Test
    @DisplayName("Create Git-Repos")
    @SneakyThrows
    @Order(3)
    void createGitRepos() {
        plant(new URI(ROOT_PATH), Collections.singletonList(new URI("http://localhost:9999/static/gh-flat/gh-flat-ShapeTree.jsonld#repos")), "Git-Repos", null);
        ensureExistsHasMetadataWithPredicateValue(new URI(ROOT_PATH + "Git-Repos/"), new URI(ShapeTreeVocabulary.HAS_SHAPE_TREE_INSTANCE_PATH), ".");
    }

    @Test
    @DisplayName("Create Git-Issues")
    @SneakyThrows
    @Order(4)
    void createGitIssues() {
        plant(new URI(ROOT_PATH), Collections.singletonList(new URI("http://localhost:9999/static/gh-flat/gh-flat-ShapeTree.jsonld#issues")), "Git-Issues", null);
        ensureExistsHasMetadataWithPredicateValue(new URI(ROOT_PATH + "Git-Issues/"), new URI(ShapeTreeVocabulary.HAS_SHAPE_TREE_INSTANCE_PATH), ".");
    }

    @Test
    @DisplayName("Create Git-Comments")
    @SneakyThrows
    @Order(5)
    void createGitComments() {
        plant(new URI(ROOT_PATH), Collections.singletonList(new URI("http://localhost:9999/static/gh-flat/gh-flat-ShapeTree.jsonld#cmnt_C")), "Git-Comments", null);
        ensureExistsHasMetadataWithPredicateValue(new URI(ROOT_PATH + "Git-Comments/"), new URI(ShapeTreeVocabulary.HAS_SHAPE_TREE_INSTANCE_PATH), ".");
    }

    @Test
    @DisplayName("Create Git-Events")
    @SneakyThrows
    @Order(6)
    void createGitEvents() {
        plant(new URI(ROOT_PATH), Collections.singletonList(new URI("http://localhost:9999/static/gh-flat/gh-flat-ShapeTree.jsonld#evt_C")), "Git-Events", null);
        ensureExistsHasMetadataWithPredicateValue(new URI(ROOT_PATH + "Git-Events/"), new URI(ShapeTreeVocabulary.HAS_SHAPE_TREE_INSTANCE_PATH), ".");
    }

    @Test
    @DisplayName("Create Git-Labels")
    @SneakyThrows
    @Order(7)
    void createGitLabels() {
        plant(new URI(ROOT_PATH), Collections.singletonList(new URI("http://localhost:9999/static/gh-flat/gh-flat-ShapeTree.jsonld#lbl_C")), "Git-Labels", null);
        ensureExistsHasMetadataWithPredicateValue(new URI(ROOT_PATH + "Git-Labels/"), new URI(ShapeTreeVocabulary.HAS_SHAPE_TREE_INSTANCE_PATH), ".");
    }

    @Test
    @DisplayName("Create Git-Milestones")
    @SneakyThrows
    @Order(8)
    void createGitMilestones() {
        plant(new URI(ROOT_PATH), Collections.singletonList(new URI("http://localhost:9999/static/gh-flat/gh-flat-ShapeTree.jsonld#mlt_C")), "Git-Milestones", null);
        ensureExistsHasMetadataWithPredicateValue(new URI(ROOT_PATH + "Git-Milestones/"), new URI(ShapeTreeVocabulary.HAS_SHAPE_TREE_INSTANCE_PATH), ".");
    }

    @Test
    @DisplayName("Create Git-Orgs/shapetrees.ttl")
    @SneakyThrows
    @Order(9)
    void createGitOrgs_ShapeTreesTTL() {
        postContent(new URI(ROOT_PATH + "Git-Orgs/"), "shapetrees.ttl", false, "target/test-classes/test-data/apps/gh-deep/shapetrees-org.ttl", "#shapetrees");
        ensureExistsWithPredicateValue(new URI(ROOT_PATH + "Git-Orgs/shapetrees.ttl"), new URI("http://github.example/ns#node_id"), "MDEyOk9yZ2FuaXphdGlvbjY0NDk0NjU5");
    }

    @Test
    @DisplayName("Create /Git-Users/ericprud.ttl")
    @SneakyThrows
    @Order(10)
    void createGitUsers_ericprudTTL() {
        postContent(new URI(ROOT_PATH + "Git-Users/"), "ericprud.ttl", false, "target/test-classes/test-data/apps/gh-deep/ericprud-user.ttl", "#ericprud");
        ensureExistsWithPredicateValue(new URI(ROOT_PATH + "Git-Users/ericprud.ttl"), new URI("http://github.example/ns#node_id"), "MDQ6VXNlcjU3MzQ3OA==");
    }

    @Test
    @DisplayName("Create Git-Repos/shapetree.js")
    @SneakyThrows
    @Order(11)
    void createGitRepos_shapetreejs() {
        postContent(new URI(ROOT_PATH + "Git-Repos/"), "shapetree.js.ttl", false, "target/test-classes/test-data/apps/gh-deep/shapetree.js-repo.ttl", "#shapetree.js");
        ensureExistsWithPredicateValue(new URI(ROOT_PATH + "Git-Repos/shapetree.js.ttl"), new URI("http://github.example/ns#node_id"),  "MDEwOlJlcG9zaXRvcnkyNTI0MDUwOTE=");
    }

    @Test
    @DisplayName("Add shapetree.js repo to /Git-Orgs/shapetrees")
    @SneakyThrows
    @Order(12)
    void addShapetreeJSRepoToGitOrgs_shapetrees() {
        patchContent(new URI(ROOT_PATH+"Git-Orgs/shapetrees.ttl"), false, "INSERT DATA { <" + ROOT_PATH + "Git-Orgs/shapetrees.ttl#shapetrees> <http://github.example/ns#repo> <../Git-Repos/shapetrees.js.ttl> }", "#shapetrees" );
        ensureExistsWithPredicateValue(new URI(ROOT_PATH+"Git-Orgs/shapetrees.ttl"), new URI("http://github.example/ns#repo"), new URI(ROOT_PATH+"Git-Repos/shapetrees.js.ttl"));
    }

    @Test
    @DisplayName("Create /Git-Repos/jsg/")
    @SneakyThrows
    @Order(13)
    void createGitReposJSG() {
        postContent(new URI(ROOT_PATH + "Git-Repos/"), "jsg.ttl", false, "target/test-classes/test-data/apps/gh-deep/jsg.ttl", "#jsg");
        ensureExistsWithPredicateValue(new URI(ROOT_PATH + "Git-Repos/jsg.ttl"), new URI("http://github.example/ns#node_id"),  "MDEwOlJlcG9zaXRvcnk0NjA2MTUxMg==");
    }

    @Test
    @DisplayName("Add jsg repository to /Git-Users/ericprud")
    @SneakyThrows
    @Order(14)
    void addjsgRepoToGitUsersEricPrud() {
        //patchContent(new URI(ROOT_PATH+"Git-Users/ericprud.ttl"), false, "INSERT DATA { <" + ROOT_PATH + "Git-Users/ericprud.ttl#ericprud> <http://github.example/ns#repo> <../Git-Repos/jsg.ttl> }", "#ericprud" );
        //ensureExistsWithPredicateValue(new URI(ROOT_PATH+"Git-Users/ericprud.ttl"), new URI("http://github.example/ns#repo"), new URI(ROOT_PATH+"Git-Repos/jsg.ttl"));
    }

    @Test
    @DisplayName("Add jsg subscription to /Git-Users/ericprud")
    @SneakyThrows
    @Order(15)
    void addjsgSubscriptionToGitUsersEricPrud() {
        //H.patch({path: `/${Shared}/Git-Users/ericprud.ttl`, mediaType: 'application/sparql-query',body: `INSERT DATA { <#ericprud> <${NS_gh}subscription> <../Git-Repos/jsg.ttl> }`,status: 204});
        //H.find([{path: `/${Shared}/Git-Users/ericprud.ttl`, accept: 'text/turtle', entries: ['gh:subscription <../Git-Repos/jsg.ttl>']},])
    }

    @Test
    @DisplayName("Create /Git-Repos/libxml-annot")
    @SneakyThrows
    @Order(16)
    void createGitReposLibxmlAnnot() {
        postContent(new URI(ROOT_PATH + "Git-Repos/"), "libxml-annot.ttl", false, "target/test-classes/test-data/apps/gh-deep/libxml-annot-repo.ttl", "#libxml-annot");
        ensureExistsWithPredicateValue(new URI(ROOT_PATH + "Git-Repos/libxml-annot.ttl"), new URI("http://github.example/ns#node_id"),  "MDc6TGljZW5zZTA=");
    }

    @Test
    @DisplayName("Add libxml-annot repository to /Git-Users/ericprud")
    @SneakyThrows
    @Order(17)
    void addLibxmlAnnotRepositoryToGitUsersEricPrud() {
        //H.patch({path: `/${Shared}/Git-Users/ericprud.ttl`, mediaType: 'application/sparql-query',body: `INSERT DATA { <#ericprud> <${NS_gh}repository> <../Git-Orgs/libxml-annot.ttl> }`,status: 204});
        //H.find([{path: `/${Shared}/Git-Users/ericprud.ttl`, accept: 'text/turtle', entries: ['gh:repository .* <../Git-Orgs/libxml-annot.ttl>']},])
    }

    @Test
    @DisplayName("Create /Git-Issues/issue1")
    @SneakyThrows
    @Order(18)
    void createGitIssuesIssue1() {
        postContent(new URI(ROOT_PATH+"Git-Issues/"), "issue1.ttl", false, "target/test-classes/test-data/apps/gh-deep/jsg-issue1.ttl", "#issue1");
        ensureExistsWithPredicateValue(new URI(ROOT_PATH + "Git-Issues/issue1.ttl"), new URI("http://github.example/ns#author_association"),  "OWNER");
    }

    @Test
    @DisplayName("Add issue issue1 to /Git-Users/ericprud")
    @SneakyThrows
    @Order(19)
    void addIssueIssue1ToGitUsersEricPrud() {
        //H.patch({path: `/${Shared}/Git-Users/ericprud.ttl`, mediaType: 'application/sparql-query',body: `INSERT DATA { <#ericprud> <${NS_gh}issue> <../Git-Orgs/issue1.ttl> }`,status: 204});
        //H.find([{path: `/${Shared}/Git-Users/ericprud.ttl`, accept: 'text/turtle', entries: ['gh:issue <../Git-Orgs/issue1.ttl>']},])
    }
}
