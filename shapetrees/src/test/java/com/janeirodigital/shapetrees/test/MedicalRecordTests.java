package com.janeirodigital.shapetrees.test;

import com.janeirodigital.shapetrees.RemoteResource;
import com.janeirodigital.shapetrees.model.ShapeTreeLocator;
import com.janeirodigital.shapetrees.test.fixtures.DispatcherEntry;
import com.janeirodigital.shapetrees.test.fixtures.RequestMatchingFixtureDispatcher;
import com.janeirodigital.shapetrees.vocabulary.ShapeTreeVocabulary;
import jdk.jfr.Label;
import lombok.SneakyThrows;
import okhttp3.Response;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.jupiter.api.*;

import java.net.URI;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

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
                new DispatcherEntry(List.of("fhir-shex"), "GET", "/static/shex/fhir/r4/shex", null),
                new DispatcherEntry(List.of("data-container"), "GET", "/ldp/data/", null),
                new DispatcherEntry(List.of("data-container-metadata"), "GET", "/ldp/data/?ext=shapetree", null),
                new DispatcherEntry(List.of("medical-record-container"), "GET", "/ldp/data/medical-record", null),
                new DispatcherEntry(List.of("condition-container"), "GET", "/ldp/data/conditions", null),
                new DispatcherEntry(List.of("condition-container"), "GET", "/ldp/data/conditions/", null),
                new DispatcherEntry(List.of("condition-1"), "GET", "/ldp/data/conditions/condition1.ttl", null),
                new DispatcherEntry(List.of("condition-1-create-response"), "PUT", "/ldp/data/conditions/condition1.ttl", null),
                new DispatcherEntry(List.of("condition-3-create-response"), "PUT", "/ldp/data/conditions/condition3.ttl", null),
                new DispatcherEntry(List.of("404","medical-record-container-metadata"), "GET", "/ldp/data/medical-record/?ext=shapetree", null),
                new DispatcherEntry(List.of("404","conditions-container-metadata"), "GET", "/ldp/data/conditions/?ext=shapetree", null),
                new DispatcherEntry(List.of("medical-record-plant-response"), "POST", "/ldp/data/", null),
                new DispatcherEntry(List.of("conditions-plant-response"), "POST", "/ldp/data/", null)
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

    @Order(2)
    @SneakyThrows
    @Test
    @Label("Discover shape tree metadata")
    void discoverPlantedShapeTree() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        List<ShapeTreeLocator> locators = this.shapeTreeClient.discoverShapeTree(this.context, getURI(server, "/ldp/data/medical-record"));
        assertNotNull(locators);
        boolean foundExpected = false;
        for (ShapeTreeLocator locator : locators) {
            if (locator.getShapeTree().equals(getURI(server,"/static/shapetrees/medical-record/shapetree#medicalRecords").toString())) {
                foundExpected = true;
            }
        }
        assertTrue(foundExpected);
    }

    @Order(3)
    @SneakyThrows
    @Test
    @Label("Plant Condition Shape tree")
    void plantConditionShapeTree() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        this.shapeTreeClient.plantShapeTree(this.context,
                getURI(server,"/ldp/data/"),
                List.of(getURI(server,"/static/shapetrees/medical-record/shapetree#conditions")),
                null,
                null,
                "conditions",
                "<#a> <#b> <#c>",
                TEXT_TURTLE);

        ensureExistsHasMetadataWithPredicateValue(getURI(server, "/ldp/data/conditions"), new URI(ShapeTreeVocabulary.HAS_SHAPE_TREE_INSTANCE_ROOT), getURI(server, "/ldp/data/conditions/"));
    }

    @Order(4)
    @SneakyThrows
    @Test
    @Label("Create Data Instance")
    void createCondition() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        Response response = this.shapeTreeClient.createDataInstance(this.context,
                getURI(server,"/ldp/data/conditions/"),
                "http://hl7.org/fhir/Condition/example",
                null,
                "condition1.ttl",
                false,
                getConditionTtl(),
                "text/turtle"
                );
        assertEquals(201, response.code());
        RemoteResource resource = new RemoteResource(getURI(server,"/ldp/data/conditions/condition1.ttl"), null);
        assertTrue(resource.exists());
    }

    @Order(5)
    @SneakyThrows
    @Test
    @Label("Create Data Instance - fail validation")
    void createConditionInvalid() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        Response response = this.shapeTreeClient.createDataInstance(this.context,
                getURI(server,"/ldp/data/conditions/"),
                "http://hl7.org/fhir/Condition/example",
                null,
                "condition3.ttl",
                false,
                getInvalidConditionTtl(),
                "text/turtle"
        );
        assertEquals(422, response.code());
    }

    @Order(6)
    @SneakyThrows
    @Test
    @Label("Create Data Instance - fail validation - but skip validation")
    void createConditionInvalidSkipValidation() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        this.shapeTreeClient.setSkipValidation(true);
        Response response = this.shapeTreeClient.createDataInstance(this.context,
                getURI(server,"/ldp/data/conditions/"),
                "http://hl7.org/fhir/Condition/example",
                null,
                "condition3.ttl",
                false,
                getInvalidConditionTtl(),
                "text/turtle"
        );
        assertEquals(201, response.code());
        this.shapeTreeClient.setSkipValidation(true);

    }


    private String getConditionTtl() {
        return "@prefix fhir: <http://hl7.org/fhir/> .\n" +
                "@prefix owl: <http://www.w3.org/2002/07/owl#> .\n" +
                "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n" +
                "@prefix sct: <http://snomed.info/id/> .\n" +
                "@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n" +
                "\n" +
                "\n" +
                "<http://hl7.org/fhir/Condition/example> a fhir:Condition;\n" +
                "  fhir:nodeRole fhir:treeRoot;\n" +
                "  fhir:Resource.id [ fhir:value \"example\"];\n" +
                "  fhir:DomainResource.text [\n" +
                "     fhir:Narrative.status [ fhir:value \"generated\" ];\n" +
                "     fhir:Narrative.div \"<div xmlns=\\\"http://www.w3.org/1999/xhtml\\\">Severe burn of left ear (Date: 24-May 2012)</div>\"\n" +
                "  ];\n" +
                "  fhir:Condition.clinicalStatus [\n" +
                "     fhir:CodeableConcept.coding [\n" +
                "       fhir:index 0;\n" +
                "       fhir:Coding.system [ fhir:value \"http://terminology.hl7.org/CodeSystem/condition-clinical\" ];\n" +
                "       fhir:Coding.code [ fhir:value \"active\" ]\n" +
                "     ]\n" +
                "  ];\n" +
                "  fhir:Condition.verificationStatus [\n" +
                "     fhir:CodeableConcept.coding [\n" +
                "       fhir:index 0;\n" +
                "       fhir:Coding.system [ fhir:value \"http://terminology.hl7.org/CodeSystem/condition-ver-status\" ];\n" +
                "       fhir:Coding.code [ fhir:value \"confirmed\" ]\n" +
                "     ]\n" +
                "  ];\n" +
                "  fhir:Condition.category [\n" +
                "     fhir:index 0;\n" +
                "     fhir:CodeableConcept.coding [\n" +
                "       fhir:index 0;\n" +
                "       fhir:Coding.system [ fhir:value \"http://terminology.hl7.org/CodeSystem/condition-category\" ];\n" +
                "       fhir:Coding.code [ fhir:value \"encounter-diagnosis\" ];\n" +
                "       fhir:Coding.display [ fhir:value \"Encounter Diagnosis\" ]\n" +
                "     ]\n" +
                "  ];\n" +
                "  fhir:Condition.severity [\n" +
                "     fhir:CodeableConcept.coding [\n" +
                "       fhir:index 0;\n" +
                "       a sct:24484000;\n" +
                "       fhir:Coding.system [ fhir:value \"http://snomed.info/sct\" ];\n" +
                "       fhir:Coding.code [ fhir:value \"24484000\" ];\n" +
                "       fhir:Coding.display [ fhir:value \"Severe\" ]\n" +
                "     ]\n" +
                "  ];\n" +
                "  fhir:Condition.code [\n" +
                "     fhir:CodeableConcept.coding [\n" +
                "       fhir:index 0;\n" +
                "       a sct:39065001;\n" +
                "       fhir:Coding.system [ fhir:value \"http://snomed.info/sct\" ];\n" +
                "       fhir:Coding.code [ fhir:value \"39065001\" ];\n" +
                "       fhir:Coding.display [ fhir:value \"Burn of ear\" ]\n" +
                "     ];\n" +
                "     fhir:CodeableConcept.text [ fhir:value \"Burnt Ear\" ]\n" +
                "  ];\n" +
                "  fhir:Condition.bodySite [\n" +
                "     fhir:index 0;\n" +
                "     fhir:CodeableConcept.coding [\n" +
                "       fhir:index 0;\n" +
                "       a sct:49521004;\n" +
                "       fhir:Coding.system [ fhir:value \"http://snomed.info/sct\" ];\n" +
                "       fhir:Coding.code [ fhir:value \"49521004\" ];\n" +
                "       fhir:Coding.display [ fhir:value \"Left external ear structure\" ]\n" +
                "     ];\n" +
                "     fhir:CodeableConcept.text [ fhir:value \"Left Ear\" ]\n" +
                "  ];\n" +
                "  fhir:Condition.subject [\n" +
                "     fhir:link <http://hl7.org/fhir/Patient/example>;\n" +
                "     fhir:Reference.reference [ fhir:value \"Patient/example\" ]\n" +
                "  ];\n" +
                "  fhir:Condition.onsetDateTime [ fhir:value \"2012-05-24\"^^xsd:date] .\n";
    }

    private String getInvalidConditionTtl() {
        return  "@prefix fhir: <http://hl7.org/fhir/> .\n" +
                "@prefix owl: <http://www.w3.org/2002/07/owl#> .\n" +
                "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n" +
                "@prefix sct: <http://snomed.info/id/> .\n" +
                "@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n" +
                "\n" +
                "\n" +
                "<http://hl7.org/fhir/Condition/example> \n" +
                "  fhir:nodeRole fhir:treeRoot;\n" +
                "  fhir:Resource.id [ fhir:value \"example\"];\n" +
                "  fhir:DomainResource.text [\n" +
                "     fhir:Narrative.status [ fhir:value \"generated\" ];\n" +
                "     fhir:Narrative.div \"<div xmlns=\\\"http://www.w3.org/1999/xhtml\\\">Severe burn of left ear (Date: 24-May 2012)</div>\"\n" +
                "  ];\n" +
                "  fhir:Condition.clinicalStatus [\n" +
                "     fhir:CodeableConcept.coding [\n" +
                "       fhir:index 0;\n" +
                "       fhir:Coding.system [ fhir:value \"http://terminology.hl7.org/CodeSystem/condition-clinical\" ];\n" +
                "       fhir:Coding.code [ fhir:value \"active\" ]\n" +
                "     ]\n" +
                "  ];\n" +
                "  fhir:Condition.verificationStatus [\n" +
                "     fhir:CodeableConcept.coding [\n" +
                "       fhir:index 0;\n" +
                "       fhir:Coding.system [ fhir:value \"http://terminology.hl7.org/CodeSystem/condition-ver-status\" ];\n" +
                "       fhir:Coding.code [ fhir:value \"confirmed\" ]\n" +
                "     ]\n" +
                "  ];\n" +
                "  fhir:Condition.category [\n" +
                "     fhir:index 0;\n" +
                "     fhir:CodeableConcept.coding [\n" +
                "       fhir:index 0;\n" +
                "       fhir:Coding.system [ fhir:value \"http://terminology.hl7.org/CodeSystem/condition-category\" ];\n" +
                "       fhir:Coding.code [ fhir:value \"encounter-diagnosis\" ];\n" +
                "       fhir:Coding.display [ fhir:value \"Encounter Diagnosis\" ]\n" +
                "     ]\n" +
                "  ];\n" +
                "  fhir:Condition.severity [\n" +
                "     fhir:CodeableConcept.coding [\n" +
                "       fhir:index 0;\n" +
                "       a sct:24484000;\n" +
                "       fhir:Coding.system [ fhir:value \"http://snomed.info/sct\" ];\n" +
                "       fhir:Coding.code [ fhir:value \"24484000\" ];\n" +
                "       fhir:Coding.display [ fhir:value \"Severe\" ]\n" +
                "     ]\n" +
                "  ];\n" +
                "  fhir:Condition.code [\n" +
                "     fhir:CodeableConcept.coding [\n" +
                "       fhir:index 0;\n" +
                "       a sct:39065001;\n" +
                "       fhir:Coding.system [ fhir:value \"http://snomed.info/sct\" ];\n" +
                "       fhir:Coding.code [ fhir:value \"39065001\" ];\n" +
                "       fhir:Coding.display [ fhir:value \"Burn of ear\" ]\n" +
                "     ];\n" +
                "     fhir:CodeableConcept.text [ fhir:value \"Burnt Ear\" ]\n" +
                "  ];\n" +
                "  fhir:Condition.bodySite [\n" +
                "     fhir:index 0;\n" +
                "     fhir:CodeableConcept.coding [\n" +
                "       fhir:index 0;\n" +
                "       a sct:49521004;\n" +
                "       fhir:Coding.system [ fhir:value \"http://snomed.info/sct\" ];\n" +
                "       fhir:Coding.code [ fhir:value \"49521004\" ];\n" +
                "       fhir:Coding.display [ fhir:value \"Left external ear structure\" ]\n" +
                "     ];\n" +
                "     fhir:CodeableConcept.text [ fhir:value \"Left Ear\" ]\n" +
                "  ];\n" +
                "  fhir:Condition.subject [\n" +
                "     fhir:link <http://hl7.org/fhir/Patient/example>;\n" +
                "     fhir:Reference.reference [ fhir:value \"Patient/example\" ]\n" +
                "  ];\n" +
                "  fhir:Condition.onsetDateTime [ fhir:value \"2012-05-24\"^^xsd:date] .\n";
    }

}
