package com.janeirodigital.shapetrees.test;

import com.janeirodigital.shapetrees.test.fixtures.DispatcherEntry;
import com.janeirodigital.shapetrees.test.fixtures.RequestMatchingFixtureDispatcher;
import com.janeirodigital.shapetrees.vocabulary.ShapeTreeVocabulary;
import jdk.jfr.Label;
import lombok.SneakyThrows;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.jupiter.api.*;

import java.net.URI;
import java.util.List;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class MedicalRecordTests extends BaseShapeTreeTest {

    public MedicalRecordTests() {
        super(new MockEcosystem());
    }

    private static RequestMatchingFixtureDispatcher dispatcher = null;

    @BeforeAll
    static void beforeAll() {
        dispatcher = new RequestMatchingFixtureDispatcher(List.of(
                new DispatcherEntry(List.of("medical-record-shapetree-ttl"), "GET", "/static/shapetrees/medical-record/shapetree", null),
                new DispatcherEntry(List.of("data-container"), "GET", "/ldp/data/", null),
                new DispatcherEntry(List.of("404","medical-record-container-metadata"), "GET", "/ldp/data/medical-record/?ext=shapetree", null),
                new DispatcherEntry(List.of("medical-record-plant-response"), "POST", "/ldp/data/", null),
                new DispatcherEntry(List.of("data-container-metadata"), "GET", "/ldp/data/?ext=shapetree", null),
                new DispatcherEntry(List.of("medical-record-container"), "GET", "/ldp/data/medical-record", null)
        ));
    }


    @Order(1)
    @SneakyThrows
    @Test
    @Label("Plant Medical Record")
    void plantMedicalRecord() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        this.shapeTreeClient.plantShapeTree(this.context,
                getURI(server,"/ldp/data/"),
                List.of(getURI(server,"/static/shapetrees/medical-record/shapetree#medicalRecords")),
                null,
                null,
                "medical-record",
                null,
                TEXT_TURTLE);

        ensureExistsHasMetadataWithPredicateValue(getURI(server, "/ldp/data/medical-record"), new URI(ShapeTreeVocabulary.HAS_SHAPE_TREE_INSTANCE_ROOT), getURI(server, "/ldp/data/medical-record/"));
    }

}
