package com.janeirodigital.shapetrees.test;

import com.janeirodigital.shapetrees.RemoteResource;
import com.janeirodigital.shapetrees.ShapeTreeFactory;
import com.janeirodigital.shapetrees.client.ShapeTreeValidatingClientBuilder;
import com.janeirodigital.shapetrees.enums.Namespaces;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import okhttp3.*;
import org.apache.commons.io.IOUtils;
import org.apache.jena.graph.Graph;
import org.apache.jena.graph.Node;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Triple;
import org.jetbrains.annotations.NotNull;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.opentest4j.AssertionFailedError;

import static org.junit.jupiter.api.Assertions.*;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

@Slf4j
public class ShapeTreeParsingTests {

    private static final String ROOT_PATH = "https://ldp.local-ess.inrupt.com/aHR0cDovL2RldnNlcnZlcjozMDA4Mi9hdXRoL3JlYWxtcy9tYXN0ZXJiMzg4YmJlMV85ZjYzXzRlYmNfYmEzMF80MWY4ZmJjZmM0NTc/shapetree-testing/";
    private static final String TOKEN = "eyJraWQiOiJyc2ExIiwiYWxnIjoiUlMyNTYifQ.eyJzdWIiOiJodHRwczpcL1wvbGRwLmxvY2FsLWVzcy5pbnJ1cHQuY29tXC9hSFIwY0RvdkwyUmxkbk5sY25abGNqb3pNREE0TWk5aGRYUm9MM0psWVd4dGN5OXRZWE4wWlhKaU16ZzRZbUpsTVY4NVpqWXpYelJsWW1OZlltRXpNRjgwTVdZNFptSmpabU0wTlRjXC9wcm9maWxlXC9jYXJkI21lIiwiYXpwIjoiaHR0cDpcL1wvbG9jYWxob3N0OjQyMDAiLCJpc3MiOiJodHRwczpcL1wvb2lkYy5sb2NhbC1lc3MuaW5ydXB0LmNvbVwvIiwiY25mIjp7ImprdCI6Ik01RUlYX0hMbGxvazhOR3U2b0FURGtFaTJNNVFsMEt2TVV4c0pVOTEtSzgifSwiZXhwIjoxNTkzNjg2NTEzLCJpYXQiOjE1OTMwODE3MTMsImp0aSI6ImM3ZTM3NWIyLWViYzEtNDllYi05YjM4LTczNDg1NzdlM2JjZCJ9.3foE2Irffgd3H3KOHPpNSh5kk_egUL4fZ-TnIk5sR4Jh4SL5a8s9VniqDtMPmvLpqU2gOUBy-kPTNStuKqPzfhrkXXr3BOnvu-b5sP_bcObOGI8aafXNkfK2gSyvth2IsmoyhgHRU-6yIZVLO5KEk8sDqo0ot1BxUffu-2udoV4wcZx62je5PuSCRmja4HuvM_jH-mbEx-5dIh1acFSm7uFq34rTKeWFkDKQ8RCm22Pn890-XSQMwXYoVgKYWULdcXolFBa-75D8WqsKxIsDHs96nEeghLy7DZvkSunpKWQ7_Py-TWkFYtPj3mUcD0Zl1cVjhiHzw6GEzzwjEomGOQ";
    private static final String AUTH_HEADER_VALUE = "Bearer " + TOKEN;
    private static final String SHAPE_TREE_ROOT_PREDICATE = Namespaces.SHAPETREE_NAMESPACE.getValue() + "shapeTreeRoot";
    private static final String SHAPE_TREE_INSTANCE_PATH_PREDICATE = Namespaces.SHAPETREE_NAMESPACE.getValue() + "shapeTreeInstancePath";
    private static final String SHAPE_TREE_INSTANCE_ROOT_PREDICATE = Namespaces.SHAPETREE_NAMESPACE.getValue() + "shapeTreeInstanceRoot";
    private static final String SHAPE_TREE_STEP_PREDICATE = Namespaces.SHAPETREE_NAMESPACE.getValue() + "shapeTreeStep";


    @SneakyThrows
    @Test
    @DisplayName("Parse Tree")
    void parseShapeTree() {
        System.out.println(new File("").getAbsolutePath());
        ShapeTreeFactory.getShapeTreeStep(new URI("http://localhost:9999/static/gh-flat/gh-flat-shapetree.jsonld#orgs"));
        ShapeTreeFactory.getShapeTreeStep(new URI("http://localhost:9999/static/gh-flat/gh-flat-shapetree.jsonld#users"));
        ShapeTreeFactory.getShapeTreeStep(new URI("http://localhost:9999/static/gh-flat/gh-flat-shapetree.jsonld#user"));
        System.out.println();
    }

    @SneakyThrows
    @BeforeAll
    static void ensureTestPath() {
        ensureExists(new URI(ROOT_PATH));
    }

    @Test
    void GitHubFlat() throws URISyntaxException, IOException {
        // Create Git-Orgs
        plant(new URI(ROOT_PATH), new URI("http://localhost:9999/static/gh-flat/gh-flat-ShapeTree.jsonld#orgs"), "Git-Orgs");
        ensureExistsHasMetadataWithPredicateValue(new URI(ROOT_PATH + "Git-Orgs/"), new URI(SHAPE_TREE_INSTANCE_PATH_PREDICATE), ".");

        // Create Git-Users
        plant(new URI(ROOT_PATH), new URI("http://localhost:9999/static/gh-flat/gh-flat-ShapeTree.jsonld#users"), "Git-Users");
        ensureExistsHasMetadataWithPredicateValue(new URI(ROOT_PATH + "Git-Users/"), new URI(SHAPE_TREE_INSTANCE_PATH_PREDICATE), ".");

        // Create Git-Repos
        plant(new URI(ROOT_PATH), new URI("http://localhost:9999/static/gh-flat/gh-flat-ShapeTree.jsonld#repos"), "Git-Repos");
        ensureExistsHasMetadataWithPredicateValue(new URI(ROOT_PATH + "Git-Repos/"), new URI(SHAPE_TREE_INSTANCE_PATH_PREDICATE), ".");

        // Create Git-Issues
        plant(new URI(ROOT_PATH), new URI("http://localhost:9999/static/gh-flat/gh-flat-ShapeTree.jsonld#issues"), "Git-Issues");
        ensureExistsHasMetadataWithPredicateValue(new URI(ROOT_PATH + "Git-Issues/"), new URI(SHAPE_TREE_INSTANCE_PATH_PREDICATE), ".");

        // Create Git-Comments
        plant(new URI(ROOT_PATH), new URI("http://localhost:9999/static/gh-flat/gh-flat-ShapeTree.jsonld#cmnt_C"), "Git-Comments");
        ensureExistsHasMetadataWithPredicateValue(new URI(ROOT_PATH + "Git-Comments/"), new URI(SHAPE_TREE_INSTANCE_PATH_PREDICATE), ".");

        // Create Git-Events
        plant(new URI(ROOT_PATH), new URI("http://localhost:9999/static/gh-flat/gh-flat-ShapeTree.jsonld#evt_C"), "Git-Events");
        ensureExistsHasMetadataWithPredicateValue(new URI(ROOT_PATH + "Git-Events/"), new URI(SHAPE_TREE_INSTANCE_PATH_PREDICATE), ".");

        // Create Git-Labels
        plant(new URI(ROOT_PATH), new URI("http://localhost:9999/static/gh-flat/gh-flat-ShapeTree.jsonld#lbl_C"), "Git-Labels");
        ensureExistsHasMetadataWithPredicateValue(new URI(ROOT_PATH + "Git-Labels/"), new URI(SHAPE_TREE_INSTANCE_PATH_PREDICATE), ".");

        // Create Git-Milestones
        plant(new URI(ROOT_PATH), new URI("http://localhost:9999/static/gh-flat/gh-flat-ShapeTree.jsonld#mlt_C"), "Git-Milestones");
        ensureExistsHasMetadataWithPredicateValue(new URI(ROOT_PATH + "Git-Milestones/"), new URI(SHAPE_TREE_INSTANCE_PATH_PREDICATE), ".");

        // Create Git-Orgs/shapetrees.ttl
        postContent(new URI(ROOT_PATH + "Git-Orgs/"), "shapetrees.ttl", false, "target/test-classes/test-data/apps/gh-deep/shapetrees-org.ttl", "#shapetrees");
        ensureExistsWithPredicateValue(new URI(ROOT_PATH + "Git-Orgs/shapetrees.ttl"), new URI("http://github.example/ns#node_id"), "MDEyOk9yZ2FuaXphdGlvbjY0NDk0NjU5");

        // Create /Git-Users/ericprud.ttl
        postContent(new URI(ROOT_PATH + "Git-Users/"), "ericprud.ttl", false, "target/test-classes/test-data/apps/gh-deep/ericprud-user.ttl", "#ericprud");
        ensureExistsWithPredicateValue(new URI(ROOT_PATH + "Git-Users/ericprud.ttl"), new URI("http://github.example/ns#node_id"), "MDQ6VXNlcjU3MzQ3OA==");

        // Create Git-Repos/shapetree.js
        postContent(new URI(ROOT_PATH + "Git-Repos/"), "shapetree.js.ttl", false, "target/test-classes/test-data/apps/gh-deep/shapetree.js-repo.ttl", "#shapetree.js");
        ensureExistsWithPredicateValue(new URI(ROOT_PATH + "Git-Repos/shapetree.js.ttl"), new URI("http://github.example/ns#node_id"),  "MDEwOlJlcG9zaXRvcnkyNTI0MDUwOTE=");

        // Add shapetree.js repo to /Git-Orgs/shapetrees
        // SOME PATCH
        //H.patch({path: `/${Shared}/Git-Orgs/shapetrees.ttl`, mediaType: 'application/sparql-query', body: `INSERT DATA { <#shapetrees> <${NS_gh}repository> <../Git-Repos/shapetrees.js.ttl> }`,status: 204});
        //H.find([{path: `/${Shared}/Git-Orgs/shapetrees.ttl`, accept: 'text/turtle', entries: ['gh:repository <../Git-Repos/shapetrees.js.ttl>']},])

        // Create /Git-Repos/jsg/
        postContent(new URI(ROOT_PATH + "Git-Repos/"), "jsg.ttl", false, "target/test-classes/test-data/apps/gh-deep/jsg.ttl", "#jsg");
        ensureExistsWithPredicateValue(new URI(ROOT_PATH + "Git-Repos/jsg.ttl"), new URI("http://github.example/ns#node_id"),  "MDEwOlJlcG9zaXRvcnk0NjA2MTUxMg==");

        // Add jsg repository to /Git-Users/ericprud
        //H.patch({path: `/${Shared}/Git-Users/ericprud.ttl`, mediaType: 'application/sparql-query',body: `INSERT DATA { <#ericprud> <${NS_gh}repository> <../Git-Repos/jsg.ttl> }`,status: 204});
        //H.find([{path: `/${Shared}/Git-Users/ericprud.ttl`, accept: 'text/turtle', entries: ['gh:repository <../Git-Repos/jsg.ttl>']},])

        // Add jsg subscription to /Git-Users/ericprud
        //H.patch({path: `/${Shared}/Git-Users/ericprud.ttl`, mediaType: 'application/sparql-query',body: `INSERT DATA { <#ericprud> <${NS_gh}subscription> <../Git-Repos/jsg.ttl> }`,status: 204});
        //H.find([{path: `/${Shared}/Git-Users/ericprud.ttl`, accept: 'text/turtle', entries: ['gh:subscription <../Git-Repos/jsg.ttl>']},])

        // Create /Git-Repos/libxml-annot
        postContent(new URI(ROOT_PATH + "Git-Repos/"), "libxml-annot.ttl", false, "target/test-classes/test-data/apps/gh-deep/libxml-annot-repo.ttl", "#libxml-annot");
        ensureExistsWithPredicateValue(new URI(ROOT_PATH + "Git-Repos/libxml-annot.ttl"), new URI("http://github.example/ns#node_id"),  "MDc6TGljZW5zZTA=");

        // Add libxml-annot repository to /Git-Users/ericprud
        //H.patch({path: `/${Shared}/Git-Users/ericprud.ttl`, mediaType: 'application/sparql-query',body: `INSERT DATA { <#ericprud> <${NS_gh}repository> <../Git-Orgs/libxml-annot.ttl> }`,status: 204});
        //H.find([{path: `/${Shared}/Git-Users/ericprud.ttl`, accept: 'text/turtle', entries: ['gh:repository .* <../Git-Orgs/libxml-annot.ttl>']},])

        // Create /Git-Issues/issue1
        postContent(new URI(ROOT_PATH+"Git-Issues/"), "issue1.ttl", false, "target/test-classes/test-data/apps/gh-deep/jsg-issue1.ttl", "#issue1");
        ensureExistsWithPredicateValue(new URI(ROOT_PATH + "Git-Issues/issue1.ttl"), new URI("http://github.example/ns#author_association"),  "OWNER");

        // Add issue issue1 to /Git-Users/ericprud
        //H.patch({path: `/${Shared}/Git-Users/ericprud.ttl`, mediaType: 'application/sparql-query',body: `INSERT DATA { <#ericprud> <${NS_gh}issue> <../Git-Orgs/issue1.ttl> }`,status: 204});
        //H.find([{path: `/${Shared}/Git-Users/ericprud.ttl`, accept: 'text/turtle', entries: ['gh:issue <../Git-Orgs/issue1.ttl>']},])

    }

    @AfterAll
    static void tearDownAll() {
        List<String> urls = new ArrayList<>();
        urls = SolidTestHelper.getRecursiveContainerContents(AUTH_HEADER_VALUE, ROOT_PATH, urls);

        Collections.reverse(urls);
        for (String url : urls) {
            log.info("Deleting " + url);
            SolidTestHelper.deleteResource(AUTH_HEADER_VALUE, url);
        }
    }

    static Response postContent(URI parentContainer, String slug, boolean isContainer, String bodyResourcePath, String focusNode) throws IOException {
        return postContent(parentContainer, slug, isContainer, bodyResourcePath, focusNode, 201);
    }

    static Response postContent(URI parentContainer, String slug, boolean isContainer, String bodyResourcePath, String focusNode, Integer expectedCode) throws IOException {
        OkHttpClient client = new ShapeTreeValidatingClientBuilder(new MockEcosystem()).get();

        String resourceTypeUri = isContainer ? "http://www.w3.org/ns/ldp#Container" : "http://www.w3.org/ns/ldp#Resource";

        FileInputStream inputStream = new FileInputStream(bodyResourcePath);
        String bodyString = IOUtils.toString(inputStream, "UTF-8");

        Request post = new Request.Builder()
                .url(parentContainer.toString())
                .addHeader("Authorization", AUTH_HEADER_VALUE)
                .addHeader("Link", "<" + resourceTypeUri + ">; rel=\"type\"")
                .addHeader("Slug", slug)
                .addHeader("Link", "<" + focusNode + ">; rel=\"focusNode\"")
                .addHeader("Content-Type", "text/turtle")
                .post(RequestBody.create(bodyString, MediaType.get("text/turtle")))
                .build();

        Response response = client.newCall(post).execute();
        assertEquals(expectedCode, response.code());
        return response;
    }

    static Response plant(URI parentContainer, URI shapeTreeStepURI, String slug) throws IOException {
        return plant(parentContainer, shapeTreeStepURI, slug, 201);
    }

    static Response plant(URI parentContainer, URI shapeTreeStepURI, String slug, Integer expectedCode) throws IOException {
        OkHttpClient client = new ShapeTreeValidatingClientBuilder(new MockEcosystem()).get();

        Request plantPost = new Request.Builder()
                .url(parentContainer.toString())
                .addHeader("Authorization", AUTH_HEADER_VALUE)
                .addHeader("Link", "<" + shapeTreeStepURI.toString() + ">; rel=\"ShapeTree\"")
                .addHeader("Link", "<http://www.w3.org/ns/ldp#Container>; rel=\"type\"")
                .addHeader("Slug", slug)
                .post(RequestBody.create(new byte[]{}))
                .build();

        Response response = client.newCall(plantPost).execute();
        assertEquals(expectedCode, response.code());
        return response;
    }

    static void ensureExists(URI uri) throws IOException {
        RemoteResource resource = new RemoteResource(uri, AUTH_HEADER_VALUE);
        if (!resource.exists()) {
            throw new AssertionFailedError("Resource " + uri + " doesn't exist");
        }
    }

    static void ensureExistsWithPredicate(URI uri, URI predicate) throws IOException, URISyntaxException {
        RemoteResource resource = new RemoteResource(uri, AUTH_HEADER_VALUE);
        if (!resource.exists()) {
            throw new AssertionFailedError("Resource " + uri + " doesn't exist");
        }
        ensureExistingTriple(resource.getGraph(), null, NodeFactory.createURI(predicate.toString()), null);
    }

    static void ensureExistsHasMetadataWithPredicateValue(URI uri, URI predicate, Object value) throws IOException, URISyntaxException {
        ensureExistsWithPredicateValue(getMetadataResourceURI(uri), predicate, value);
    }

    static void ensureExistsWithPredicateValue(URI uri, URI predicate, Object value) throws IOException, URISyntaxException {
        RemoteResource resource = new RemoteResource(uri, AUTH_HEADER_VALUE);
        if (!resource.exists()) {
            throw new AssertionFailedError("Resource " + uri + " doesn't exist");
        }
        ensureExistingTriple(resource.getGraph(), null, NodeFactory.createURI(predicate.toString()), value);
    }

    private static void ensureExistingTriple(Graph graph, Object subject, Object predicate, Object object) {
        if (graph == null) {
            throw new AssertionFailedError("Graph is null");
        }

        Node subjectNode = createNodeFromInput(subject);
        Node predicateNode = createNodeFromInput(predicate);
        Node objectNode = createNodeFromInput(object);

        if (!graph.contains(subjectNode, predicateNode, objectNode)) {
            throw new AssertionFailedError("Specified Triple not found");
        } else {
            List<Triple> triples = graph.find(subjectNode, predicateNode, objectNode).toList();
            log.trace("Found: " + triples.size());
        }
    }

    private static Node createNodeFromInput(Object input) {
        if (input == null) {
            return null;
        } else if (input instanceof Node) {
            return (Node)input;
        } else if (input instanceof URI) {
            return NodeFactory.createURI(((URI)input).toString());
        } else if (input instanceof String) {
            return NodeFactory.createLiteral((String)input);
        } else {
            log.warn("Received an input of type " + input.getClass().getSimpleName() + " treating it as a string literal");
            return NodeFactory.createLiteral((String)input);
        }
    }

    @SneakyThrows
    static void ensureNotExists(URI uri) {
        RemoteResource resource = new RemoteResource(uri, AUTH_HEADER_VALUE);
        if (resource.exists()) {
            throw new AssertionFailedError("Resource " + uri + " unexpectedly exists");
        }
    }

    @NotNull
    @SneakyThrows
    static URI getMetadataResourceURI(URI primaryResourceURI) {
        return new URI(primaryResourceURI.toString() + ".meta");
    }
}
