package com.janeirodigital.shapetrees.test;

import com.janeirodigital.shapetrees.vocabulary.ShapeTreeVocabulary;
import lombok.SneakyThrows;
import org.junit.jupiter.api.*;

import java.net.URI;
import java.util.Collections;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class GitHubFlatTests extends BaseShapeTreeTest {

    public GitHubFlatTests() {
        super(new MockEcosystem());
    }
/*
    @Test
    @DisplayName("Create Git-Orgs")
    @SneakyThrows
    @Order(1)
    void createGitOrgs()
    {
        this.shapeTreeClient.plantShapeTree(this.context, new URI(ROOT_PATH), Collections.singletonList(new URI("http://localhost:9999/static/gh-flat/gh-flat-ShapeTree.jsonld#orgs")), null, null, "Git-Orgs", null);
        ensureExistsHasMetadataWithPredicateValue(new URI(ROOT_PATH + "Git-Orgs/"), new URI(ShapeTreeVocabulary.HAS_SHAPE_TREE_INSTANCE_ROOT), new URI(ROOT_PATH + "Git-Orgs/"));
    }

    @Test
    @DisplayName("Create Git-Users")
    @SneakyThrows
    @Order(2)
    void createGitUsers() {
        this.shapeTreeClient.plantShapeTree(this.context, new URI(ROOT_PATH), Collections.singletonList(new URI("http://localhost:9999/static/gh-flat/gh-flat-ShapeTree.jsonld#users")), null, null, "Git-Users", null);
        ensureExistsHasMetadataWithPredicateValue(new URI(ROOT_PATH + "Git-Users/"), new URI(ShapeTreeVocabulary.HAS_SHAPE_TREE_INSTANCE_ROOT), new URI(ROOT_PATH + "Git-Users/"));
    }

    @Test
    @DisplayName("Create Git-Repos")
    @SneakyThrows
    @Order(3)
    void createGitRepos() {
        this.shapeTreeClient.plantShapeTree(this.context, new URI(ROOT_PATH), Collections.singletonList(new URI("http://localhost:9999/static/gh-flat/gh-flat-ShapeTree.jsonld#repos")), null, null, "Git-Repos", null);
        ensureExistsHasMetadataWithPredicateValue(new URI(ROOT_PATH + "Git-Repos/"), new URI(ShapeTreeVocabulary.HAS_SHAPE_TREE_INSTANCE_ROOT), new URI(ROOT_PATH + "Git-Repos/"));
    }

    @Test
    @DisplayName("Create Git-Issues")
    @SneakyThrows
    @Order(4)
    void createGitIssues() {
        this.shapeTreeClient.plantShapeTree(this.context, new URI(ROOT_PATH), Collections.singletonList(new URI("http://localhost:9999/static/gh-flat/gh-flat-ShapeTree.jsonld#issues")), null, null, "Git-Issues", null);
        ensureExistsHasMetadataWithPredicateValue(new URI(ROOT_PATH + "Git-Issues/"), new URI(ShapeTreeVocabulary.HAS_SHAPE_TREE_INSTANCE_ROOT), new URI(ROOT_PATH + "Git-Issues/"));
    }

    @Test
    @DisplayName("Create Git-Comments")
    @SneakyThrows
    @Order(5)
    void createGitComments() {
        this.shapeTreeClient.plantShapeTree(this.context, new URI(ROOT_PATH), Collections.singletonList(new URI("http://localhost:9999/static/gh-flat/gh-flat-ShapeTree.jsonld#cmnt_C")), null, null, "Git-Comments", null);
        ensureExistsHasMetadataWithPredicateValue(new URI(ROOT_PATH + "Git-Comments/"), new URI(ShapeTreeVocabulary.HAS_SHAPE_TREE_INSTANCE_ROOT), new URI(ROOT_PATH + "Git-Comments/"));
    }

    @Test
    @DisplayName("Create Git-Events")
    @SneakyThrows
    @Order(6)
    void createGitEvents() {
        this.shapeTreeClient.plantShapeTree(this.context, new URI(ROOT_PATH), Collections.singletonList(new URI("http://localhost:9999/static/gh-flat/gh-flat-ShapeTree.jsonld#evt_C")), null, null, "Git-Events", null);
        ensureExistsHasMetadataWithPredicateValue(new URI(ROOT_PATH + "Git-Events/"), new URI(ShapeTreeVocabulary.HAS_SHAPE_TREE_INSTANCE_ROOT), new URI(ROOT_PATH + "Git-Events/"));
    }

    @Test
    @DisplayName("Create Git-Labels")
    @SneakyThrows
    @Order(7)
    void createGitLabels() {
        this.shapeTreeClient.plantShapeTree(this.context, new URI(ROOT_PATH), Collections.singletonList(new URI("http://localhost:9999/static/gh-flat/gh-flat-ShapeTree.jsonld#lbl_C")), null, null, "Git-Labels", null);
        ensureExistsHasMetadataWithPredicateValue(new URI(ROOT_PATH + "Git-Labels/"), new URI(ShapeTreeVocabulary.HAS_SHAPE_TREE_INSTANCE_ROOT), new URI(ROOT_PATH + "Git-Labels/"));
    }

    @Test
    @DisplayName("Create Git-Milestones")
    @SneakyThrows
    @Order(8)
    void createGitMilestones() {
        this.shapeTreeClient.plantShapeTree(this.context, new URI(ROOT_PATH), Collections.singletonList(new URI("http://localhost:9999/static/gh-flat/gh-flat-ShapeTree.jsonld#mlt_C")), null, null, "Git-Milestones", null);
        ensureExistsHasMetadataWithPredicateValue(new URI(ROOT_PATH + "Git-Milestones/"), new URI(ShapeTreeVocabulary.HAS_SHAPE_TREE_INSTANCE_ROOT), new URI(ROOT_PATH + "Git-Milestones/"));
    }

    @Test
    @DisplayName("Create Git-Orgs/shapetrees.ttl")
    @SneakyThrows
    @Order(9)
    void createGitOrgs_ShapeTreesTTL() {
        this.shapeTreeClient.createDataInstance(this.context, new URI(ROOT_PATH + "Git-Orgs/"), "#shapetrees", null, "shapetrees.ttl", false, getResourceContent("target/test-classes/test-data/apps/gh-deep/shapetrees-org.ttl"), "text/turtle");
        ensureExistsWithPredicateValue(new URI(ROOT_PATH + "Git-Orgs/shapetrees.ttl"), new URI("http://github.example/ns#node_id"), "MDEyOk9yZ2FuaXphdGlvbjY0NDk0NjU5");
    }

    @Test
    @DisplayName("Create /Git-Users/ericprud.ttl")
    @SneakyThrows
    @Order(10)
    void createGitUsers_ericprudTTL() {
        this.shapeTreeClient.createDataInstance(this.context, new URI(ROOT_PATH + "Git-Users/"), "#ericprud", null, "ericprud.ttl", false, getResourceContent("target/test-classes/test-data/apps/gh-deep/ericprud-user.ttl"), "text/turtle");
        ensureExistsWithPredicateValue(new URI(ROOT_PATH + "Git-Users/ericprud.ttl"), new URI("http://github.example/ns#node_id"), "MDQ6VXNlcjU3MzQ3OA==");
    }

    @Test
    @DisplayName("Create Git-Repos/shapetree.js")
    @SneakyThrows
    @Order(11)
    void createGitRepos_shapetreejs() {
        this.shapeTreeClient.createDataInstance(this.context, new URI(ROOT_PATH + "Git-Repos/"), "#shapetree.js", null, "shapetree.js.ttl", false, getResourceContent("target/test-classes/test-data/apps/gh-deep/shapetree.js-repo.ttl"), "text/turtle");
        ensureExistsWithPredicateValue(new URI(ROOT_PATH + "Git-Repos/shapetree.js.ttl"), new URI("http://github.example/ns#node_id"),  "MDEwOlJlcG9zaXRvcnkyNTI0MDUwOTE=");
    }

    @Test
    @DisplayName("Add shapetree.js repo to /Git-Orgs/shapetrees")
    @SneakyThrows
    @Order(12)
    void addShapetreeJSRepoToGitOrgs_shapetrees() {
        this.shapeTreeClient.updateDataInstanceWithPatch(this.context, new URI(ROOT_PATH+"Git-Orgs/shapetrees.ttl"), "#shapetrees", null, "INSERT DATA { <" + ROOT_PATH + "Git-Orgs/shapetrees.ttl#shapetrees> <http://github.example/ns#repo> <../Git-Repos/shapetrees.js.ttl> }", "application/sparql-update");
        ensureExistsWithPredicateValue(new URI(ROOT_PATH+"Git-Orgs/shapetrees.ttl"), new URI("http://github.example/ns#repo"), new URI(ROOT_PATH+"Git-Repos/shapetrees.js.ttl"));
    }

    @Test
    @DisplayName("Create /Git-Repos/jsg/")
    @SneakyThrows
    @Order(13)
    void createGitReposJSG() {
        this.shapeTreeClient.createDataInstance(this.context, new URI(ROOT_PATH + "Git-Repos/"), "#jsg", null, "jsg.ttl", false, getResourceContent("target/test-classes/test-data/apps/gh-deep/jsg.ttl"), "text/turtle");
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
        this.shapeTreeClient.createDataInstance(this.context, new URI(ROOT_PATH + "Git-Repos/"), "#libxml-annot", null, "libxml-annot.ttl", false, getResourceContent("target/test-classes/test-data/apps/gh-deep/libxml-annot-repo.ttl"), "text/turtle");
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
        this.shapeTreeClient.createDataInstance(this.context, new URI(ROOT_PATH + "Git-Issues/"), "#issue1", null, "issue1.ttl", false, getResourceContent("target/test-classes/test-data/apps/gh-deep/jsg-issue1.ttl"), "text/turtle");
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

 */
}
