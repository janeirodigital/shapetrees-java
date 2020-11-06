package com.janeirodigital.shapetrees.test;

import com.janeirodigital.shapetrees.client.impl.ShapeTreeClientConfiguration;
import com.janeirodigital.shapetrees.client.impl.ShapeTreeHttpClientHolder;
import com.janeirodigital.shapetrees.enums.HttpHeaders;
import com.janeirodigital.shapetrees.enums.LinkRelations;
import com.janeirodigital.shapetrees.test.fixtures.DispatcherEntry;
import com.janeirodigital.shapetrees.test.fixtures.RequestMatchingFixtureDispatcher;
import lombok.SneakyThrows;
import okhttp3.*;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.jupiter.api.*;

import java.net.URI;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class RawPlantTests  extends BaseShapeTreeTest {

    public RawPlantTests() {
        super(new MockEcosystem());
    }

    private static RequestMatchingFixtureDispatcher dispatcher = null;

    @BeforeAll
    static void beforeAll() {
        dispatcher = new RequestMatchingFixtureDispatcher(List.of(
                new DispatcherEntry(List.of("shapetrees/medical-record-shapetree-ttl"), "GET", "/static/shapetrees/medical-record/shapetree", null),
                new DispatcherEntry(List.of("shapetrees/interop-shapetree-ttl"), "GET", "/static/shapetrees/solid-interop/shapetree", null),
                new DispatcherEntry(List.of("schemas/github-shex"), "GET", "/static/shex/github/shex", null),
                new DispatcherEntry(List.of("schemas/fhir-shex"), "GET", "/static/shex/fhir/r4/shex", null),
                new DispatcherEntry(List.of("schemas/solid-interop-shex"), "GET", "/static/shex/solid-interop/shex", null),

                new DispatcherEntry(List.of("rawPlant/ldp-container"), "GET", "/ldp/", null),
                new DispatcherEntry(List.of("rawPlant/raw-container"), "GET", "/ldp/raw/", null),
                new DispatcherEntry(List.of("rawPlant/conditions-container"), "GET", "/ldp/conditions/", null),
                new DispatcherEntry(List.of("rawPlant/conditions-container-metadata"), "GET", "/ldp/conditions/?ext=shapetree", null),
                new DispatcherEntry(List.of("rawPlant/invalid-container"), "GET", "/ldp/invalid/", null),
                new DispatcherEntry(List.of("rawPlant/invalid-container-metadata"), "GET", "/ldp/invalid/?ext=shapetree", null),
                new DispatcherEntry(List.of("rawPlant/invalid-container"), "GET", "/ldp/invalid", null),
                new DispatcherEntry(List.of("rawPlant/condition1-create-response"), "POST", "/ldp/conditions/", Map.of(HttpHeaders.SLUG.getValue(), List.of("condition1")))
        ));
    }

    @Order(1)
    @SneakyThrows
    @Test
    void testPlantFailRootNotContainer() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        ShapeTreeClientConfiguration validatingConfig = new ShapeTreeClientConfiguration(new MockEcosystem(), true, false);
        OkHttpClient client = ShapeTreeHttpClientHolder.getForConfig(validatingConfig);

        // This shapetree is expectsType=ShapeTreeResource, so it is invalid to be the shapetree at the root of a plant operation
        List<URI> shapeTreeURIs = List.of(getURI(server, "/static/shapetrees/medical-record/shapetree#condition"));

        byte[] bytes = new byte[]{};

        Request.Builder builder = new Request.Builder().url(getURI(server, "/ldp/raw/").toString());

        for (URI shapeTreeUri : shapeTreeURIs) {
            builder.addHeader(HttpHeaders.LINK.getValue(), "<" + shapeTreeUri.toString() + ">; rel=\"" + LinkRelations.SHAPETREE.getValue() + "\"");
        }

        applyCommonHeaders(context, builder, null, null, true, "conditions", "text/turtle");

        Request plantPost = builder
                .post(RequestBody.create(bytes))
                .build();

        Response response = client.newCall(plantPost).execute();
        assertEquals(400, response.code());
    }

    @Order(2)
    @SneakyThrows
    @Test
    void testPlantFailMultipleValidatesBy() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        ShapeTreeClientConfiguration validatingConfig = new ShapeTreeClientConfiguration(new MockEcosystem(), true, false);
        OkHttpClient client = ShapeTreeHttpClientHolder.getForConfig(validatingConfig);

        // Both of these shapetrees have a validatedBy property which is not allowed
        List<URI> shapeTreeURIs = List.of(getURI(server, "/static/shapetrees/medical-record/shapetree#condition"),
                getURI(server, "/static/shapetrees/medical-record/shapetree#patient"));

        byte[] bytes = new byte[]{};

        Request.Builder builder = new Request.Builder().url(getURI(server, "/ldp/raw/").toString());

        for (URI shapeTreeUri : shapeTreeURIs) {
            builder.addHeader(HttpHeaders.LINK.getValue(), "<" + shapeTreeUri.toString() + ">; rel=\"" + LinkRelations.SHAPETREE.getValue() + "\"");
        }

        applyCommonHeaders(context, builder, null, null, true, "conditions", "text/turtle");

        Request plantPost = builder
                .post(RequestBody.create(bytes))
                .build();

        Response response = client.newCall(plantPost).execute();
        assertEquals(400, response.code());
    }

    @Order(3)
    @SneakyThrows
    @Test
    void testPlantFailMultipleContains() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        ShapeTreeClientConfiguration validatingConfig = new ShapeTreeClientConfiguration(new MockEcosystem(), true, false);
        OkHttpClient client = ShapeTreeHttpClientHolder.getForConfig(validatingConfig);

        // Both of these shapetrees have a contains property which is not allowed
        List<URI> shapeTreeURIs = List.of(getURI(server, "/static/shapetrees/medical-record/shapetree#conditions"),
                getURI(server, "/static/shapetrees/medical-record/shapetree#patients"));

        byte[] bytes = new byte[]{};

        Request.Builder builder = new Request.Builder().url(getURI(server, "/ldp/raw/").toString());

        for (URI shapeTreeUri : shapeTreeURIs) {
            builder.addHeader(HttpHeaders.LINK.getValue(), "<" + shapeTreeUri.toString() + ">; rel=\"" + LinkRelations.SHAPETREE.getValue() + "\"");
        }

        applyCommonHeaders(context, builder, null, null, true, "conditions", "text/turtle");

        Request plantPost = builder
                .post(RequestBody.create(bytes))
                .build();

        Response response = client.newCall(plantPost).execute();
        assertEquals(400, response.code());
    }

    @Order(4)
    @SneakyThrows
    @Test
    void testManagedByNonexistingShapeTree() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        ShapeTreeClientConfiguration validatingConfig = new ShapeTreeClientConfiguration(new MockEcosystem(), true, false);
        OkHttpClient client = ShapeTreeHttpClientHolder.getForConfig(validatingConfig);

        // Both of these shapetrees have a contains property which is not allowed
        List<URI> shapeTreeURIs = List.of(getURI(server, "/static/shapetrees/medical-record/shapetree#conditions"));

        byte[] bytes = new byte[]{};

        Request.Builder builder = new Request.Builder().url(getURI(server, "/ldp/").toString());

        for (URI shapeTreeUri : shapeTreeURIs) {
            builder.addHeader(HttpHeaders.LINK.getValue(), "<" + shapeTreeUri.toString() + ">; rel=\"" + LinkRelations.SHAPETREE.getValue() + "\"");
        }

        applyCommonHeaders(context, builder, null, null, true, "invalid", "text/turtle");

        Request plantPost = builder
                .post(RequestBody.create(bytes))
                .build();

        Response response = client.newCall(plantPost).execute();
        assertEquals(500, response.code());
    }

    @Order(5)
    @SneakyThrows
    @Test
    void testPlantWithBody() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        ShapeTreeClientConfiguration validatingConfig = new ShapeTreeClientConfiguration(new MockEcosystem(), true, false);
        OkHttpClient client = ShapeTreeHttpClientHolder.getForConfig(validatingConfig);

        // Both of these shapetrees have a contains property which is not allowed
        List<URI> shapeTreeURIs = List.of(getURI(server, "/static/shapetrees/medical-record/shapetree#conditions"),
                getURI(server, "/static/shapetrees/solid-interop/shapetree#data-registration-tree"));

        byte[] bytes = "<#registration> <#b> <#c> .".getBytes();

        Request.Builder builder = new Request.Builder().url(getURI(server, "/ldp/").toString());

        for (URI shapeTreeUri : shapeTreeURIs) {
            builder.addHeader(HttpHeaders.LINK.getValue(), "<" + shapeTreeUri.toString() + ">; rel=\"" + LinkRelations.SHAPETREE.getValue() + "\"");
        }

        applyCommonHeaders(context, builder, "#registration", null, true, "withbody", "text/turtle");

        Request plantPost = builder
                .post(RequestBody.create(bytes))
                .build();

        Response response = client.newCall(plantPost).execute();
        assertEquals(422, response.code());
    }

    @Order(6)
    @SneakyThrows
    @Test
    void testPlantNonURIValue() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        ShapeTreeClientConfiguration validatingConfig = new ShapeTreeClientConfiguration(new MockEcosystem(), true, false);
        OkHttpClient client = ShapeTreeHttpClientHolder.getForConfig(validatingConfig);

        byte[] bytes = "<#registration> <#b> <#c> .".getBytes();

        Request.Builder builder = new Request.Builder().url(getURI(server, "/ldp/").toString());

        // This is not a valid URI, will fail validation as a shapetree
        builder.addHeader(HttpHeaders.LINK.getValue(), "<:invalid>; rel=\"" + LinkRelations.SHAPETREE.getValue() + "\"");

        applyCommonHeaders(context, builder, "#registration", null, true, "withbody", "text/turtle");

        Request plantPost = builder
                .post(RequestBody.create(bytes))
                .build();

        Response response = client.newCall(plantPost).execute();
        assertEquals(400, response.code());
    }

    @Order(7)
    @SneakyThrows
    @Test
    void testVanillaPostToManagedContainer() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        ShapeTreeClientConfiguration validatingConfig = new ShapeTreeClientConfiguration(new MockEcosystem(), true, false);
        OkHttpClient client = ShapeTreeHttpClientHolder.getForConfig(validatingConfig);

        byte[] bytes = MedicalRecordTests.getConditionTtl().getBytes();

        Request.Builder builder = new Request.Builder().url(getURI(server, "/ldp/conditions/").toString());

        // Note that no shapetree link header is provided, meaning we're just doing a post, not planting
        applyCommonHeaders(context, builder, "http://hl7.org/fhir/Condition/example", null, false, "condition1", "text/turtle");

        Request plantPost = builder
                .post(RequestBody.create(bytes))
                .build();

        Response response = client.newCall(plantPost).execute();
        assertEquals(201, response.code());

    }

}