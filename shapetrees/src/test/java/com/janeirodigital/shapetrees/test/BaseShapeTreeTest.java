package com.janeirodigital.shapetrees.test;

import com.janeirodigital.shapetrees.RemoteResource;
import com.janeirodigital.shapetrees.ShapeTreeEcosystem;
import com.janeirodigital.shapetrees.enums.LinkRelations;
import com.janeirodigital.shapetrees.vocabulary.ShapeTreeVocabulary;
import com.janeirodigital.shapetrees.client.ShapeTreeValidatingClientBuilder;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import okhttp3.*;
import org.apache.commons.io.IOUtils;
import org.apache.http.auth.AUTH;
import org.apache.http.client.methods.RequestBuilder;
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
import java.util.Set;

@Slf4j
public abstract class BaseShapeTreeTest {

    protected static final String SERVER_ROOT = "https://ldp.local-ess.inrupt.com/";
    protected static final String ROOT_PATH = SERVER_ROOT+"aHR0cDovL2RldnNlcnZlcjozMDA4Mi9hdXRoL3JlYWxtcy9tYXN0ZXJiMzg4YmJlMV85ZjYzXzRlYmNfYmEzMF80MWY4ZmJjZmM0NTc/shapetree-testing/";
    protected static final String TOKEN = "eyJraWQiOiJyc2ExIiwiYWxnIjoiUlMyNTYifQ.eyJzdWIiOiJodHRwczpcL1wvbGRwLmxvY2FsLWVzcy5pbnJ1cHQuY29tXC9hSFIwY0RvdkwyUmxkbk5sY25abGNqb3pNREE0TWk5aGRYUm9MM0psWVd4dGN5OXRZWE4wWlhKaU16ZzRZbUpsTVY4NVpqWXpYelJsWW1OZlltRXpNRjgwTVdZNFptSmpabU0wTlRjXC9wcm9maWxlXC9jYXJkI21lIiwiYXpwIjoiaHR0cDpcL1wvbG9jYWxob3N0OjQyMDAiLCJpc3MiOiJodHRwczpcL1wvb2lkYy5sb2NhbC1lc3MuaW5ydXB0LmNvbVwvIiwiY25mIjp7ImprdCI6Ik01RUlYX0hMbGxvazhOR3U2b0FURGtFaTJNNVFsMEt2TVV4c0pVOTEtSzgifSwiZXhwIjoxNTk2NTQwNzM1LCJpYXQiOjE1OTU5MzU5MzUsImp0aSI6IjFhZDIyZjQzLTAwNmUtNDA3My1iN2U1LTNkYzU5ZGU0YWI3MyJ9.qhAS7qN3cQJxVxqpwdxxS3I5juak5sw06Ghiy9sijF956TSLSPyTubLgxthWzPtArc9Az3xckbk5ZjFvxgK83GkHbVvF0emzaM8p9gaISrfc7r8_z9uNa0RQ5IcK75F_T2oAIkKZVTet6YXW_ZiFUIN3TkyNb-5TIiBEdjUHKVr0ZFZaRfj_sL6n88C4qNTmNtizt7ijYjN_py06IJfDJvGASqqcd1YQs7lPcgnWHya0jIwMtKiz0Q2uWo-FYL1TpALFuLf6irUmgzpyLbjzT3uksA0H6iTYusOYmMsvLalN2FVkc9IAZTVYweJ8Q6lLBa7EB2cW9UWzgMeqgTWeTw";
    protected static final String AUTH_HEADER_VALUE = "Bearer " + TOKEN;
    private final ShapeTreeEcosystem ecosystem;

    public BaseShapeTreeTest(ShapeTreeEcosystem ecosystem) {
        this.ecosystem = ecosystem;
    }


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

    protected Response patchContent(URI patchResource, boolean isContainer, String sparqlUpdate, String focusNode) throws IOException {
        return patchContent(patchResource, isContainer, sparqlUpdate, focusNode, 204);
    }

    protected Response patchContent(URI patchResource, boolean isContainer, String sparqlUpdate, String focusNode, Integer expectedCode) throws IOException {
        OkHttpClient client = new ShapeTreeValidatingClientBuilder((this.ecosystem)).get();

        byte[] sparqlUpdateBytes = sparqlUpdate.getBytes();

        Request patch = new Request.Builder()
                .url(patchResource.toString())
                .addHeader("Authorization", AUTH_HEADER_VALUE)
                .addHeader("Link", "<" + focusNode + ">; rel=\"" + LinkRelations.FOCUS_NODE.getValue() + "\"")
                .addHeader("Content-Type", "application/sparql-update")
                .patch(RequestBody.create(sparqlUpdateBytes))
                .build();

        Response response = client.newCall(patch).execute();
        assertEquals(expectedCode, response.code(), response.message());
        return response;
    }

    protected Response postContent(URI parentContainer, String slug, boolean isContainer, String bodyResourcePath, String focusNode) throws IOException {
        return postContent(parentContainer, slug, isContainer, bodyResourcePath, focusNode, 201);
    }

    protected Response postContent(URI parentContainer, String slug, boolean isContainer, String bodyResourcePath, String focusNode, Integer expectedCode) throws IOException {
        return postContent(parentContainer, slug, isContainer, bodyResourcePath, focusNode, "text/turtle", expectedCode, null);
    }

    protected Response postContent(URI parentContainer, String slug, boolean isContainer, String bodyResourcePath, String focusNode, String contentType, Integer expectedCode, String shapeTreeHint) throws IOException {
        OkHttpClient client = new ShapeTreeValidatingClientBuilder(this.ecosystem).get();

        String resourceTypeUri = isContainer ? "http://www.w3.org/ns/ldp#Container" : "http://www.w3.org/ns/ldp#Resource";
        RequestBody body;
        FileInputStream inputStream = new FileInputStream(bodyResourcePath);
        if (!Collections.unmodifiableSet(Set.of("text/turtle", "application/rdf+xml", "application/n-triples", "application/ld+json")).contains(contentType)) {
            resourceTypeUri = "http://www.w3.org/ns/ldp#NonRDFSource";
            body = RequestBody.create(inputStream.readAllBytes(), MediaType.get(contentType));
        } else {
            String bodyString = IOUtils.toString(inputStream, "UTF-8");
            body = RequestBody.create(bodyString, MediaType.get("text/turtle"));
        }

        Request.Builder postBuilder = new Request.Builder()
                .url(parentContainer.toString())
                .addHeader("Authorization", AUTH_HEADER_VALUE)
                .addHeader("Link", "<" + resourceTypeUri + ">; rel=\"type\"")
                .addHeader("Slug", slug)
                .addHeader("Link", "<" + focusNode + ">; rel=\"" + LinkRelations.FOCUS_NODE.getValue() + "\"")
                .addHeader("Content-Type", contentType)
                .post(body);

        if (shapeTreeHint != null) {
            postBuilder.addHeader("Link", "<" + shapeTreeHint + ">; rel=\"" + LinkRelations.TARGET_SHAPETREE.getValue() + "\"");
        }

        Request post = postBuilder.build();

        Response response = client.newCall(post).execute();
        assertEquals(expectedCode, response.code());
        return response;
    }

    protected Response putContent(URI resourceURI, boolean isContainer, String bodyResourcePath, String focusNode) throws IOException {
        return putContent(resourceURI, isContainer, bodyResourcePath, focusNode, 201);
    }

    protected Response putContent(URI resourceURI, boolean isContainer, String bodyResourcePath, String focusNode, Integer expectedCode) throws IOException {
        OkHttpClient client = new ShapeTreeValidatingClientBuilder(this.ecosystem).get();

        String resourceTypeUri = isContainer ? "http://www.w3.org/ns/ldp#Container" : "http://www.w3.org/ns/ldp#Resource";

        FileInputStream inputStream = new FileInputStream(bodyResourcePath);
        String bodyString = IOUtils.toString(inputStream, "UTF-8");

        Request post = new Request.Builder()
                .url(resourceURI.toString())
                .addHeader("Authorization", AUTH_HEADER_VALUE)
                .addHeader("Link", "<" + resourceTypeUri + ">; rel=\"type\"")
                .addHeader("Link", "<" + focusNode + ">; rel=\"" + LinkRelations.FOCUS_NODE.getValue() + "\"")
                .addHeader("Content-Type", "text/turtle")
                .put(RequestBody.create(bodyString, MediaType.get("text/turtle")))
                .build();

        Response response = client.newCall(post).execute();
        assertEquals(expectedCode, response.code());
        return response;
    }

    protected Response plant(URI parentContainer, List<URI> shapeTreeURIs, String slug, String focusNode) throws IOException {
        return plantWithStringContent(parentContainer, shapeTreeURIs, slug, null, "text/turtle", focusNode, 201);
    }

    protected Response plantWithResourceContent(URI parentContainer, List<URI> shapeTreeURIs, String slug, String bodyResourcePath, String contentType, String focusNode, Integer expectedCode) throws IOException {
        FileInputStream inputStream = new FileInputStream(bodyResourcePath);
        String bodyString = IOUtils.toString(inputStream, "UTF-8");

        return plantWithStringContent(parentContainer, shapeTreeURIs, slug, bodyString, contentType, focusNode, expectedCode);
    }

    protected Response plantWithStringContent(URI parentContainer, List<URI> shapeTreeURIs, String slug, String content, String contentType, String focusNode, Integer expectedCode) throws IOException {
        OkHttpClient client = new ShapeTreeValidatingClientBuilder(this.ecosystem).get();

        byte[] bytes = new byte[]{};
        if (content != null) {
            bytes = content.getBytes();
        }

        Request.Builder builder = new Request.Builder()
                .url(parentContainer.toString())
                .addHeader("Authorization", AUTH_HEADER_VALUE);

        for (URI shapeTreeUri : shapeTreeURIs) {
            builder.addHeader("Link", "<" + shapeTreeUri.toString() + ">; rel=\"" + LinkRelations.SHAPETREE.getValue() + "\"");
        }

        Request plantPost = builder
                .addHeader("Link", "<http://www.w3.org/ns/ldp#Container>; rel=\"type\"")
                .addHeader("Link", "<" + focusNode + ">; rel=\"" + LinkRelations.FOCUS_NODE.getValue() + "\"")
                .addHeader("Slug", slug)
                .addHeader("Content-Type", contentType)
                .post(RequestBody.create(bytes))
                .build();

        Response response = client.newCall(plantPost).execute();
        assertEquals(expectedCode, response.code(), response.message());
        return response;
    }

    protected Response delete(URI resourceURI) throws IOException {
        return delete(resourceURI, 204);
    }

    protected Response delete(URI resourceURI, Integer expectedCode) throws IOException
    {
        OkHttpClient client = new ShapeTreeValidatingClientBuilder(this.ecosystem).get();

        Request delete = new Request.Builder()
                .url(resourceURI.toString())
                .addHeader("Authorization", AUTH_HEADER_VALUE)
                .delete()
                .build();

        Response response = client.newCall(delete).execute();
        assertEquals(expectedCode, response.code(), response.message());
        return response;

    }
    protected static void ensureExists(URI uri) throws IOException {
        RemoteResource resource = new RemoteResource(uri, AUTH_HEADER_VALUE);
        if (!resource.exists()) {
            throw new AssertionFailedError("Resource " + uri + " doesn't exist");
        }
    }

    protected static void ensureExistsWithPredicate(URI uri, URI predicate) throws IOException, URISyntaxException {
        RemoteResource resource = new RemoteResource(uri, AUTH_HEADER_VALUE);
        if (!resource.exists()) {
            throw new AssertionFailedError("Resource " + uri + " doesn't exist");
        }
        ensureExistingTriple(resource.getGraph(uri), null, NodeFactory.createURI(predicate.toString()), null);
    }

    protected static void ensureExistsHasMetadataWithPredicateValue(URI uri, URI predicate, Object value) throws IOException, URISyntaxException {
        ensureExistsWithPredicateValue(getMetadataResourceURI(uri), uri, predicate, value);
    }

    protected static void ensureExistsWithPredicateValue(URI uri, URI predicate, Object value) throws IOException, URISyntaxException {
        ensureExistsWithPredicateValue(uri, uri, predicate, value);
    }


    protected static void ensureExistsWithPredicateValue(URI uri, URI baseURI, URI predicate, Object value) throws IOException, URISyntaxException {
        RemoteResource resource = new RemoteResource(uri, AUTH_HEADER_VALUE);
        if (!resource.exists()) {
            throw new AssertionFailedError("Resource " + uri + " doesn't exist");
        }
        ensureExistingTriple(resource.getGraph(baseURI), null, NodeFactory.createURI(predicate.toString()), value);
    }

    protected static void ensureExistsHasMetadataWithValues(URI uri, URI instanceRootValue) throws IOException, URISyntaxException{
        RemoteResource resource = new RemoteResource(getMetadataResourceURI(uri), AUTH_HEADER_VALUE);
        if (!resource.exists()) {
            throw new AssertionFailedError("Resource " + uri + " doesn't exist");
        }
        ensureExistingTriple(resource.getGraph(uri), null, NodeFactory.createURI(ShapeTreeVocabulary.HAS_SHAPE_TREE_INSTANCE_ROOT), instanceRootValue);
    }

    protected static void ensureExistingTriple(Graph graph, Object subject, Object predicate, Object object) {
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
    protected static void ensureNotExists(URI uri) {
        RemoteResource resource = new RemoteResource(uri, AUTH_HEADER_VALUE);
        if (resource.exists()) {
            throw new AssertionFailedError("Resource " + uri + " unexpectedly exists");
        }
    }

    @NotNull
    @SneakyThrows
    protected static URI getMetadataResourceURI(URI primaryResourceURI) {
        RemoteResource resource = new RemoteResource(primaryResourceURI, AUTH_HEADER_VALUE);
        return new URI(resource.getMetadataURI());
    }
}
