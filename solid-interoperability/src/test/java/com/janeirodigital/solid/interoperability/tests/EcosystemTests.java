package com.janeirodigital.solid.interoperability.tests;

import com.janeirodigital.shapetrees.ShapeTreeFactory;
import com.janeirodigital.shapetrees.test.BaseShapeTreeTest;
import com.janeirodigital.shapetrees.vocabulary.ShapeTreeVocabulary;
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

    @Test
    @DisplayName("Create Git-Orgs")
    @SneakyThrows
    @Order(11)
    void createGitOrgs()
    {
        List<URI> shapeTreesToPlant = new ArrayList<>();
        shapeTreesToPlant.add(new URI("http://localhost:9999/static/gh-flat/gh-flat-ShapeTree.jsonld#orgs"));
        shapeTreesToPlant.add(new URI("http://localhost:9999/static/ecosystem/ecosystem-ShapeTree.ttl#data-registration-tree"));

        plant(new URI(ROOT_PATH+"data/"), shapeTreesToPlant, "Git-Orgs", "#registration");
        ensureExistsHasMetadataWithPredicateValue(new URI(ROOT_PATH + "data/Git-Orgs/"), new URI(ShapeTreeVocabulary.HAS_SHAPE_TREE_INSTANCE_PATH), ".");
    }

    @Test
    @DisplayName("Create Git-Users")
    @SneakyThrows
    @Order(12)
    void createGitUsers() {
        List<URI> shapeTreesToPlant = new ArrayList<>();
        shapeTreesToPlant.add(new URI("http://localhost:9999/static/gh-flat/gh-flat-ShapeTree.jsonld#users"));
        shapeTreesToPlant.add(new URI("http://localhost:9999/static/ecosystem/ecosystem-ShapeTree.ttl#data-registration-tree"));

        plant(new URI(ROOT_PATH+"data/"), shapeTreesToPlant, "Git-Users", "#registration");
        ensureExistsHasMetadataWithPredicateValue(new URI(ROOT_PATH + "data/Git-Users/"), new URI(ShapeTreeVocabulary.HAS_SHAPE_TREE_INSTANCE_PATH), ".");
    }

    @Test
    @DisplayName("Create Git-Repos")
    @SneakyThrows
    @Order(13)
    void createGitRepos() {
        List<URI> shapeTreesToPlant = new ArrayList<>();
        shapeTreesToPlant.add(new URI("http://localhost:9999/static/gh-flat/gh-flat-ShapeTree.jsonld#repos"));
        shapeTreesToPlant.add(new URI("http://localhost:9999/static/ecosystem/ecosystem-ShapeTree.ttl#data-registration-tree"));

        plant(new URI(ROOT_PATH+"data/"), shapeTreesToPlant, "Git-Repos", "#registration");
        ensureExistsHasMetadataWithPredicateValue(new URI(ROOT_PATH + "data/Git-Repos/"), new URI(ShapeTreeVocabulary.HAS_SHAPE_TREE_INSTANCE_PATH), ".");
    }

    @Test
    @DisplayName("Create Git-Issues")
    @SneakyThrows
    @Order(14)
    void createGitIssues() {
        List<URI> shapeTreesToPlant = new ArrayList<>();
        shapeTreesToPlant.add(new URI("http://localhost:9999/static/gh-flat/gh-flat-ShapeTree.jsonld#issues"));
        shapeTreesToPlant.add(new URI("http://localhost:9999/static/ecosystem/ecosystem-ShapeTree.ttl#data-registration-tree"));

        plant(new URI(ROOT_PATH+"data/"), shapeTreesToPlant, "Git-Issues", "#registration");
        ensureExistsHasMetadataWithPredicateValue(new URI(ROOT_PATH + "data/Git-Issues/"), new URI(ShapeTreeVocabulary.HAS_SHAPE_TREE_INSTANCE_PATH), ".");
    }

    @Test
    @DisplayName("Create Git-Comments")
    @SneakyThrows
    @Order(15)
    void createGitComments() {
        List<URI> shapeTreesToPlant = new ArrayList<>();
        shapeTreesToPlant.add(new URI("http://localhost:9999/static/gh-flat/gh-flat-ShapeTree.jsonld#cmnt_C"));
        shapeTreesToPlant.add(new URI("http://localhost:9999/static/ecosystem/ecosystem-ShapeTree.ttl#data-registration-tree"));

        plant(new URI(ROOT_PATH+"data/"), shapeTreesToPlant, "Git-Comments", "#registration");
        ensureExistsHasMetadataWithPredicateValue(new URI(ROOT_PATH + "data/Git-Comments/"), new URI(ShapeTreeVocabulary.HAS_SHAPE_TREE_INSTANCE_PATH), ".");
    }

    @Test
    @DisplayName("Create Git-Events")
    @SneakyThrows
    @Order(16)
    void createGitEvents() {
        List<URI> shapeTreesToPlant = new ArrayList<>();
        shapeTreesToPlant.add(new URI("http://localhost:9999/static/gh-flat/gh-flat-ShapeTree.jsonld#evt_C"));
        shapeTreesToPlant.add(new URI("http://localhost:9999/static/ecosystem/ecosystem-ShapeTree.ttl#data-registration-tree"));

        plant(new URI(ROOT_PATH+"data/"), shapeTreesToPlant, "Git-Events", "#registration");
        ensureExistsHasMetadataWithPredicateValue(new URI(ROOT_PATH + "data/Git-Events/"), new URI(ShapeTreeVocabulary.HAS_SHAPE_TREE_INSTANCE_PATH), ".");
    }

    @Test
    @DisplayName("Create Git-Labels")
    @SneakyThrows
    @Order(17)
    void createGitLabels() {
        List<URI> shapeTreesToPlant = new ArrayList<>();
        shapeTreesToPlant.add(new URI("http://localhost:9999/static/gh-flat/gh-flat-ShapeTree.jsonld#lbl_C"));
        shapeTreesToPlant.add(new URI("http://localhost:9999/static/ecosystem/ecosystem-ShapeTree.ttl#data-registration-tree"));

        plant(new URI(ROOT_PATH+"data/"), shapeTreesToPlant, "Git-Labels", "#registration");
        ensureExistsHasMetadataWithPredicateValue(new URI(ROOT_PATH + "data/Git-Labels/"), new URI(ShapeTreeVocabulary.HAS_SHAPE_TREE_INSTANCE_PATH), ".");
    }

    @Test
    @DisplayName("Create Git-Milestones")
    @SneakyThrows
    @Order(18)
    void createGitMilestones() {
        List<URI> shapeTreesToPlant = new ArrayList<>();
        shapeTreesToPlant.add(new URI("http://localhost:9999/static/gh-flat/gh-flat-ShapeTree.jsonld#mlt_C"));
        shapeTreesToPlant.add(new URI("http://localhost:9999/static/ecosystem/ecosystem-ShapeTree.ttl#data-registration-tree"));

        plant(new URI(ROOT_PATH+"data/"), shapeTreesToPlant, "Git-Milestones", "#registration");
        ensureExistsHasMetadataWithPredicateValue(new URI(ROOT_PATH + "data/Git-Milestones/"), new URI(ShapeTreeVocabulary.HAS_SHAPE_TREE_INSTANCE_PATH), ".");
    }

    @Test
    @DisplayName("Create Git-Orgs/shapetrees.ttl")
    @SneakyThrows
    @Order(19)
    void createGitOrgs_ShapeTreesTTL() {
        postContent(new URI(ROOT_PATH + "data/Git-Orgs/"), "shapetrees.ttl", false, "target/test-classes/test-data/apps/gh-deep/shapetrees-org.ttl", "#shapetrees");
        ensureExistsWithPredicateValue(new URI(ROOT_PATH + "data/Git-Orgs/shapetrees.ttl"), new URI("http://github.example/ns#node_id"), "MDEyOk9yZ2FuaXphdGlvbjY0NDk0NjU5");
    }

    @Test
    @DisplayName("Create /Git-Users/ericprud.ttl")
    @SneakyThrows
    @Order(20)
    void createGitUsers_ericprudTTL() {
        postContent(new URI(ROOT_PATH + "data/Git-Users/"), "ericprud.ttl", false, "target/test-classes/test-data/apps/gh-deep/ericprud-user.ttl", "#ericprud");
        ensureExistsWithPredicateValue(new URI(ROOT_PATH + "data/Git-Users/ericprud.ttl"), new URI("http://github.example/ns#node_id"), "MDQ6VXNlcjU3MzQ3OA==");
    }

    @Test
    @DisplayName("Create Git-Repos/shapetree.js")
    @SneakyThrows
    @Order(21)
    void createGitRepos_shapetreejs() {
        postContent(new URI(ROOT_PATH + "data/Git-Repos/"), "shapetree.js.ttl", false, "target/test-classes/test-data/apps/gh-deep/shapetree.js-repo.ttl", "#shapetree.js");
        ensureExistsWithPredicateValue(new URI(ROOT_PATH + "data/Git-Repos/shapetree.js.ttl"), new URI("http://github.example/ns#node_id"),  "MDEwOlJlcG9zaXRvcnkyNTI0MDUwOTE=");
    }

    @Test
    @DisplayName("Add shapetree.js repo to /Git-Orgs/shapetrees")
    @SneakyThrows
    @Order(22)
    void addShapetreeJSRepoToGitOrgs_shapetrees() {
        patchContent(new URI(ROOT_PATH+"data/Git-Orgs/shapetrees.ttl"), false, "INSERT DATA { <" + ROOT_PATH + "data/Git-Orgs/shapetrees.ttl#shapetrees> <http://github.example/ns#repo> <../Git-Repos/shapetrees.js.ttl> }", "#shapetrees" );
        ensureExistsWithPredicateValue(new URI(ROOT_PATH+"data/Git-Orgs/shapetrees.ttl"), new URI("http://github.example/ns#repo"), new URI(ROOT_PATH+"data/Git-Repos/shapetrees.js.ttl"));
    }

    @Test
    @DisplayName("Create /Git-Repos/jsg/")
    @SneakyThrows
    @Order(23)
    void createGitReposJSG() {
        postContent(new URI(ROOT_PATH + "data/Git-Repos/"), "jsg.ttl", false, "target/test-classes/test-data/apps/gh-deep/jsg.ttl", "#jsg");
        ensureExistsWithPredicateValue(new URI(ROOT_PATH + "data/Git-Repos/jsg.ttl"), new URI("http://github.example/ns#node_id"),  "MDEwOlJlcG9zaXRvcnk0NjA2MTUxMg==");
    }

    @Test
    @DisplayName("Add jsg repository to /Git-Users/ericprud")
    @SneakyThrows
    @Order(24)
    void addjsgRepoToGitUsersEricPrud() {
        //patchContent(new URI(ROOT_PATH+"Git-Users/ericprud.ttl"), false, "INSERT DATA { <" + ROOT_PATH + "data/Git-Users/ericprud.ttl#ericprud> <http://github.example/ns#repo> <../Git-Repos/jsg.ttl> }", "#ericprud" );
        //ensureExistsWithPredicateValue(new URI(ROOT_PATH+"Git-Users/ericprud.ttl"), new URI("http://github.example/ns#repo"), new URI(ROOT_PATH+"Git-Repos/jsg.ttl"));
    }

    @Test
    @DisplayName("Add jsg subscription to /Git-Users/ericprud")
    @SneakyThrows
    @Order(25)
    void addjsgSubscriptionToGitUsersEricPrud() {
        //H.patch({path: `/${Shared}/Git-Users/ericprud.ttl`, mediaType: 'application/sparql-query',body: `INSERT DATA { <#ericprud> <${NS_gh}subscription> <../Git-Repos/jsg.ttl> }`,status: 204});
        //H.find([{path: `/${Shared}/Git-Users/ericprud.ttl`, accept: 'text/turtle', entries: ['gh:subscription <../Git-Repos/jsg.ttl>']},])
    }

    @Test
    @DisplayName("Create /Git-Repos/libxml-annot")
    @SneakyThrows
    @Order(26)
    void createGitReposLibxmlAnnot() {
        postContent(new URI(ROOT_PATH + "data/Git-Repos/"), "libxml-annot.ttl", false, "target/test-classes/test-data/apps/gh-deep/libxml-annot-repo.ttl", "#libxml-annot");
        ensureExistsWithPredicateValue(new URI(ROOT_PATH + "data/Git-Repos/libxml-annot.ttl"), new URI("http://github.example/ns#node_id"),  "MDc6TGljZW5zZTA=");
    }

    @Test
    @DisplayName("Add libxml-annot repository to /Git-Users/ericprud")
    @SneakyThrows
    @Order(27)
    void addLibxmlAnnotRepositoryToGitUsersEricPrud() {
        //H.patch({path: `/${Shared}/Git-Users/ericprud.ttl`, mediaType: 'application/sparql-query',body: `INSERT DATA { <#ericprud> <${NS_gh}repository> <../Git-Orgs/libxml-annot.ttl> }`,status: 204});
        //H.find([{path: `/${Shared}/Git-Users/ericprud.ttl`, accept: 'text/turtle', entries: ['gh:repository .* <../Git-Orgs/libxml-annot.ttl>']},])
    }

    @Test
    @DisplayName("Create /Git-Issues/issue1")
    @SneakyThrows
    @Order(28)
    void createGitIssuesIssue1() {
        postContent(new URI(ROOT_PATH+"data/Git-Issues/"), "issue1.ttl", false, "target/test-classes/test-data/apps/gh-deep/jsg-issue1.ttl", "#issue1");
        ensureExistsWithPredicateValue(new URI(ROOT_PATH + "data/Git-Issues/issue1.ttl"), new URI("http://github.example/ns#author_association"),  "OWNER");
    }

    @Test
    @DisplayName("Add issue issue1 to /Git-Users/ericprud")
    @SneakyThrows
    @Order(29)
    void addIssueIssue1ToGitUsersEricPrud() {
        //H.patch({path: `/${Shared}/Git-Users/ericprud.ttl`, mediaType: 'application/sparql-query',body: `INSERT DATA { <#ericprud> <${NS_gh}issue> <../Git-Orgs/issue1.ttl> }`,status: 204});
        //H.find([{path: `/${Shared}/Git-Users/ericprud.ttl`, accept: 'text/turtle', entries: ['gh:issue <../Git-Orgs/issue1.ttl>']},])
    }

}
