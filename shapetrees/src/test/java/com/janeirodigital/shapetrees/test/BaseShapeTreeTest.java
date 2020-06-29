package com.janeirodigital.shapetrees.test;

import com.janeirodigital.shapetrees.RemoteResource;
import com.janeirodigital.shapetrees.client.ShapeTreeValidatingClientBuilder;
import com.janeirodigital.shapetrees.enums.Namespaces;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import okhttp3.*;
import org.apache.commons.io.IOUtils;
import org.apache.jena.datatypes.TypeMapper;
import org.apache.jena.datatypes.xsd.XSDDatatype;
import org.apache.jena.graph.Graph;
import org.apache.jena.graph.Node;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Triple;
import org.jetbrains.annotations.NotNull;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.opentest4j.AssertionFailedError;

import static org.junit.jupiter.api.Assertions.*;

import java.io.FileInputStream;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

@Slf4j
public abstract class BaseShapeTreeTest {

    protected static final String SERVER_ROOT = "https://ldp.local-ess.inrupt.com/";
    protected static final String ROOT_PATH = SERVER_ROOT+"aHR0cDovL2RldnNlcnZlcjozMDA4Mi9hdXRoL3JlYWxtcy9tYXN0ZXJiMzg4YmJlMV85ZjYzXzRlYmNfYmEzMF80MWY4ZmJjZmM0NTc/shapetree-testing/";
    protected static final String TOKEN = "eyJraWQiOiJyc2ExIiwiYWxnIjoiUlMyNTYifQ.eyJzdWIiOiJodHRwczpcL1wvbGRwLmxvY2FsLWVzcy5pbnJ1cHQuY29tXC9hSFIwY0RvdkwyUmxkbk5sY25abGNqb3pNREE0TWk5aGRYUm9MM0psWVd4dGN5OXRZWE4wWlhKaU16ZzRZbUpsTVY4NVpqWXpYelJsWW1OZlltRXpNRjgwTVdZNFptSmpabU0wTlRjXC9wcm9maWxlXC9jYXJkI21lIiwiYXpwIjoiaHR0cDpcL1wvbG9jYWxob3N0OjQyMDAiLCJpc3MiOiJodHRwczpcL1wvb2lkYy5sb2NhbC1lc3MuaW5ydXB0LmNvbVwvIiwiY25mIjp7ImprdCI6Ik01RUlYX0hMbGxvazhOR3U2b0FURGtFaTJNNVFsMEt2TVV4c0pVOTEtSzgifSwiZXhwIjoxNTkzNjg2NTEzLCJpYXQiOjE1OTMwODE3MTMsImp0aSI6ImM3ZTM3NWIyLWViYzEtNDllYi05YjM4LTczNDg1NzdlM2JjZCJ9.3foE2Irffgd3H3KOHPpNSh5kk_egUL4fZ-TnIk5sR4Jh4SL5a8s9VniqDtMPmvLpqU2gOUBy-kPTNStuKqPzfhrkXXr3BOnvu-b5sP_bcObOGI8aafXNkfK2gSyvth2IsmoyhgHRU-6yIZVLO5KEk8sDqo0ot1BxUffu-2udoV4wcZx62je5PuSCRmja4HuvM_jH-mbEx-5dIh1acFSm7uFq34rTKeWFkDKQ8RCm22Pn890-XSQMwXYoVgKYWULdcXolFBa-75D8WqsKxIsDHs96nEeghLy7DZvkSunpKWQ7_Py-TWkFYtPj3mUcD0Zl1cVjhiHzw6GEzzwjEomGOQ";
    protected static final String AUTH_HEADER_VALUE = "Bearer " + TOKEN;
    protected static final String SHAPE_TREE_ROOT_PREDICATE = Namespaces.SHAPETREE_NAMESPACE.getValue() + "shapeTreeRoot";
    protected static final String SHAPE_TREE_INSTANCE_PATH_PREDICATE = Namespaces.SHAPETREE_NAMESPACE.getValue() + "shapeTreeInstancePath";
    protected static final String SHAPE_TREE_INSTANCE_ROOT_PREDICATE = Namespaces.SHAPETREE_NAMESPACE.getValue() + "shapeTreeInstanceRoot";
    protected static final String SHAPE_TREE_STEP_PREDICATE = Namespaces.SHAPETREE_NAMESPACE.getValue() + "shapeTreeStep";

    @SneakyThrows
    @BeforeAll
    public static void ensureTestPath() {
        ensureExists(new URI(ROOT_PATH));
    }

    @AfterAll
    public static void tearDownAll() {
        List<String> urls = new ArrayList<>();
        urls = SolidTestHelper.getRecursiveContainerContents(AUTH_HEADER_VALUE, ROOT_PATH, urls);

        Collections.reverse(urls);
        for (String url : urls) {
            log.info("Deleting " + url);
            SolidTestHelper.deleteResource(AUTH_HEADER_VALUE, url);
        }
    }

    static Response patchContent(URI patchResource, boolean isContainer, String sparqlUpdate, String focusNode) throws IOException {
        return patchContent(patchResource, isContainer, sparqlUpdate, focusNode, 204);
    }

    static Response patchContent(URI patchResource, boolean isContainer, String sparqlUpdate, String focusNode, Integer expectedCode) throws IOException {
        OkHttpClient client = new ShapeTreeValidatingClientBuilder((new MockEcosystem())).get();

        byte[] sparqlUpdateBytes = sparqlUpdate.getBytes();

        Request patch = new Request.Builder()
                .url(patchResource.toString())
                .addHeader("Authorization", AUTH_HEADER_VALUE)
                .addHeader("Link", "<" + focusNode + ">; rel=\"focusNode\"")
                .addHeader("Content-Type", "application/sparql-update")
                .patch(RequestBody.create(sparqlUpdateBytes))
                .build();

        Response response = client.newCall(patch).execute();
        assertEquals(expectedCode, response.code(), response.message());
        return response;
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

    static Response putContent(URI resourceURI, boolean isContainer, String bodyResourcePath, String focusNode) throws IOException {
        return putContent(resourceURI, isContainer, bodyResourcePath, focusNode, 201);
    }

    static Response putContent(URI resourceURI, boolean isContainer, String bodyResourcePath, String focusNode, Integer expectedCode) throws IOException {
        OkHttpClient client = new ShapeTreeValidatingClientBuilder(new MockEcosystem()).get();

        String resourceTypeUri = isContainer ? "http://www.w3.org/ns/ldp#Container" : "http://www.w3.org/ns/ldp#Resource";

        FileInputStream inputStream = new FileInputStream(bodyResourcePath);
        String bodyString = IOUtils.toString(inputStream, "UTF-8");

        Request post = new Request.Builder()
                .url(resourceURI.toString())
                .addHeader("Authorization", AUTH_HEADER_VALUE)
                .addHeader("Link", "<" + resourceTypeUri + ">; rel=\"type\"")
                .addHeader("Link", "<" + focusNode + ">; rel=\"focusNode\"")
                .addHeader("Content-Type", "text/turtle")
                .put(RequestBody.create(bodyString, MediaType.get("text/turtle")))
                .build();

        Response response = client.newCall(post).execute();
        assertEquals(expectedCode, response.code());
        return response;
    }

    static Response plant(URI parentContainer, URI shapeTreeStepURI, String slug) throws IOException {
        return plant(parentContainer, shapeTreeStepURI, slug, null, "text/turtle", 201);
    }

    static Response plant(URI parentContainer, URI shapeTreeStepURI, String slug, String content, String contentType, Integer expectedCode) throws IOException {
        OkHttpClient client = new ShapeTreeValidatingClientBuilder(new MockEcosystem()).get();

        byte[] bytes = new byte[]{};
        if (content != null) {
            bytes = content.getBytes();
        }

        Request plantPost = new Request.Builder()
                .url(parentContainer.toString())
                .addHeader("Authorization", AUTH_HEADER_VALUE)
                .addHeader("Link", "<" + shapeTreeStepURI.toString() + ">; rel=\"ShapeTree\"")
                .addHeader("Link", "<http://www.w3.org/ns/ldp#Container>; rel=\"type\"")
                .addHeader("Slug", slug)
                .addHeader("Content-Type", contentType)
                .post(RequestBody.create(bytes))
                .build();

        Response response = client.newCall(plantPost).execute();
        assertEquals(expectedCode, response.code(), response.message());
        return response;
    }

    static Response delete(URI resourceURI) throws IOException {
        return delete(resourceURI, 204);
    }

    static Response delete(URI resourceURI, Integer expectedCode) throws IOException
    {
        OkHttpClient client = new ShapeTreeValidatingClientBuilder(new MockEcosystem()).get();

        Request delete = new Request.Builder()
                .url(resourceURI.toString())
                .addHeader("Authorization", AUTH_HEADER_VALUE)
                .delete()
                .build();

        Response response = client.newCall(delete).execute();
        assertEquals(expectedCode, response.code(), response.message());
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

    static void ensureExistsHasMetadataWithValues(URI uri, String instancePathValue, URI instanceRootValue) throws IOException, URISyntaxException{
        RemoteResource resource = new RemoteResource(getMetadataResourceURI(uri), AUTH_HEADER_VALUE);
        if (!resource.exists()) {
            throw new AssertionFailedError("Resource " + uri + " doesn't exist");
        }
        ensureExistingTriple(resource.getGraph(), null, NodeFactory.createURI(SHAPE_TREE_INSTANCE_PATH_PREDICATE), instancePathValue);
        ensureExistingTriple(resource.getGraph(), null, NodeFactory.createURI(SHAPE_TREE_INSTANCE_ROOT_PREDICATE), instanceRootValue);
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
            return NodeFactory.createURI((input).toString());
        } else if (input instanceof String) {
            String inputString = (String)input;
            if (inputString.contains("^^")) {
                String inputValue = inputString.substring(0, inputString.indexOf("^^"));
                String dataType = inputString.substring(inputString.indexOf("^^")+2);
                if (dataType.startsWith("xsd:")) {
                    dataType = dataType.replace("xsd:","http://www.w3.org/2001/XMLSchema#");
                }
                return NodeFactory.createLiteral(inputValue, TypeMapper.getInstance().getTypeByName(dataType));
            }
            return NodeFactory.createLiteral((String)input);
        } else if (input instanceof Integer) {
            return NodeFactory.createLiteral(input.toString(), XSDDatatype.XSDint);
        }
        else {
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
