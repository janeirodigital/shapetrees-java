package com.janeirodigital.shapetrees.test;

import com.janeirodigital.shapetrees.RemoteResource;
import com.janeirodigital.shapetrees.ShapeTreeEcosystem;
import com.janeirodigital.shapetrees.client.ShapeTreeClient;
import com.janeirodigital.shapetrees.client.impl.ShapeTreeClientImpl;
import com.janeirodigital.shapetrees.enums.LinkRelations;
import com.janeirodigital.shapetrees.model.ShapeTreeContext;
import com.janeirodigital.shapetrees.vocabulary.ShapeTreeVocabulary;
import com.janeirodigital.shapetrees.client.impl.ShapeTreeValidatingClientBuilder;
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
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;

@Slf4j
public abstract class BaseShapeTreeTest {

    protected static final String SERVER_ROOT = "https://ldp.accenture-jd-internal.janeirodigital.net/";
    protected static final String ROOT_PATH = SERVER_ROOT+"aHR0cHM6Ly94ZmgtYXV0aC5hY2NlbnR1cmUtamQtaW50ZXJuYWwuamFuZWlyb2RpZ2l0YWwubmV0L2F1dGgvcmVhbG1zL21hc3RlcjdjZGU5YzkxX2ZmZjBfNDIzOF84MDc1XzkwNDljYzhlMTFjNA/";
    protected static final String TOKEN = "eyJraWQiOiJmOGJjNTc3MGNlNjUyOTYxNDc4Y2ZhYTJlNzQ0MTZhOCIsImFsZyI6IlJTMjU2In0.eyJzdWIiOiJodHRwczpcL1wvbGRwLmFjY2VudHVyZS1qZC1pbnRlcm5hbC5qYW5laXJvZGlnaXRhbC5uZXRcL2FIUjBjSE02THk5NFptZ3RZWFYwYUM1aFkyTmxiblIxY21VdGFtUXRhVzUwWlhKdVlXd3VhbUZ1WldseWIyUnBaMmwwWVd3dWJtVjBMMkYxZEdndmNtVmhiRzF6TDIxaGMzUmxjamRqWkdVNVl6a3hYMlptWmpCZk5ESXpPRjg0TURjMVh6a3dORGxqWXpobE1URmpOQVwvcHJvZmlsZVwvY2FyZCNtZSIsImNsaWVudF93ZWJpZCI6Imh0dHA6XC9cL2xvY2FsaG9zdDo0MjAwIiwiYXpwIjoiODMyZTE3YWUtYmJhZi00OWJlLWJkNzItYWYxNGJlYWIyYmQwIiwiaXNzIjoiaHR0cHM6XC9cL29pZGMuYWNjZW50dXJlLWpkLWludGVybmFsLmphbmVpcm9kaWdpdGFsLm5ldFwvIiwiY25mIjp7ImprdCI6IjI3ZFM2b0tSRE1xUUxVQUVxM1lyeklZdnZweTdINk9DV0V2aEU1SG9IZ28ifSwiZXhwIjoxNTk5MTY3NjQ2LCJpYXQiOjE1OTg1NjI4NDYsImp0aSI6Ijc4MTc5MjJlLWRjMGYtNDk0Ny04MGY1LWNkMDM2ZWY3YzQzMyJ9.Z_ro_pypNjTIru0kc3egQU8OqvW6ed3wzIYBfW2PtnJcQgT1XH0VoCMIKjQ9WNHeqlG0Ve0ZYHOhMeQ4qVnPxybTQQQVSFXJr056MzWu14v4WS3tHH--0cGGbk9YdHVl8MsYv2a7b86rCijIGA4I77-U38tRUsh0M-mFWD75daKQ8pBtYbzOXhOVb6Nn6X1Y_vDmV01FYGKREH26LohsT8UwvS6eIjbVGIRX8PeIU9Dtev13SQvBK1cQPKqd1LDPFUGzhvbby_ZR01-u-A7_GBn7eAXUG-59TkGgRlwmCmYtdP3qDmzIyoBpFx2zPgMFdj6LO6l51nplzEL_8ohz7xWh2YKuDCqvz2v_nZhNPaYMMiw9fAvGtWT02E937cyGKynojUTrCZsEBD_OrZiMV7D788E2E-V_0I0hAu1IAtzHKE2XuGY6MLUy5lx5fmodQWLj-faZCz1yOe0O_p6RrQ2E4rQhLQ-nEpxTRBiUOtzvUeAKMwbKkmE8dXRyQFW9m36vZFu3T8ckBCD_fH_cazAlP_uRDICgJlBiEyCEc83sa7J1dSAVeSRT_BsgyJyyJ5VJ-Hx42-jjU-VwCBiq-fPjY_U6JaEN57brs_YPGNM35sBlsTJoPMi2soB0ahfim-UsTAlWsB_9iqqFF7p0KfP3UweStYWa8HPvCGEmDzU";
    protected static final String AUTH_HEADER_VALUE = "Bearer " + TOKEN;
    private final ShapeTreeEcosystem ecosystem;
    protected final ShapeTreeClient shapeTreeClient;

    public BaseShapeTreeTest(ShapeTreeEcosystem ecosystem) {
        this.ecosystem = ecosystem;

        ShapeTreeContext context = new ShapeTreeContext();
        context.setAuthorizationHeaderValue(AUTH_HEADER_VALUE);

        this.shapeTreeClient = new ShapeTreeClientImpl(this.ecosystem, context);
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

    protected String getResourceContent(String bodyResourcePath) throws IOException {
        FileInputStream inputStream = new FileInputStream(bodyResourcePath);
        return IOUtils.toString(inputStream, StandardCharsets.UTF_8.name());
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
