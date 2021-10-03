package com.janeirodigital.shapetrees.okhttp;

import com.janeirodigital.shapetrees.core.DocumentResponse;
import com.janeirodigital.shapetrees.okhttp.fixtures.DispatcherEntry;
import com.janeirodigital.shapetrees.okhttp.fixtures.RequestMatchingFixtureDispatcher;
import com.janeirodigital.shapetrees.core.models.ShapeTreeLocation;
import com.janeirodigital.shapetrees.core.models.ShapeTreeLocator;
import jdk.jfr.Label;
import lombok.SneakyThrows;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.jupiter.api.*;

import java.util.ArrayList;
import java.util.List;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class MedicalRecordTests extends BaseShapeTreeTest {

    public MedicalRecordTests() {
        super();
    }

    private static RequestMatchingFixtureDispatcher dispatcher = null;

    @BeforeEach
    void beforeEach() {

        List dispatcherList = new ArrayList();

        dispatcherList.add(new DispatcherEntry(List.of("medicalRecord/medical-records-container"), "GET", "/ldp/data/medical-records/", null));
        dispatcherList.add(new DispatcherEntry(List.of("shapetrees/medical-record-shapetree-ttl"), "GET", "/static/shapetrees/medical-record/shapetree", null));
        dispatcherList.add(new DispatcherEntry(List.of("medicalRecord/conditions-container"), "GET", "/ldp/data/conditions/", null));
        dispatcherList.add(new DispatcherEntry(List.of("schemas/fhir-shex"), "GET", "/static/shex/fhir/r4/shex", null));

        dispatcher = new RequestMatchingFixtureDispatcher(dispatcherList);

    }

    @SneakyThrows
    @Test
    @Label("Plant Medical Record")
    void plantMedicalRecord() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/ldp/data/medical-records/?ext=shapetree", null));

        // Plant medical record on /ldp/data/medical-record
        DocumentResponse response = this.shapeTreeClient.plantShapeTree(this.context, getURI(server,"/ldp/data/medical-records/"), getURI(server,"/static/shapetrees/medical-record/shapetree#medicalRecords"), null);
        Assertions.assertEquals(201, response.getStatusCode());

    }

    @SneakyThrows
    @Test
    @Label("Discover shape tree locator on medical record")
    void discoverLocatorForMedicalRecord() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("medicalRecord/medical-records-container-locator"), "GET", "/ldp/data/medical-records/?ext=shapetree", null));

        ShapeTreeLocator locator = this.shapeTreeClient.discoverShapeTree(this.context, getURI(server,"/ldp/data/medical-records/")).orElse(null);

        Assertions.assertNotNull(locator);
        Assertions.assertEquals(getURI(server, "/ldp/data/medical-records/?ext=shapetree"), locator.getURI());
        Assertions.assertNotNull(locator.getLocations());
        Assertions.assertEquals(1, locator.getLocations().size());

        ShapeTreeLocation location = locator.getLocations().get(0);
        Assertions.assertEquals(getURI(server, "/static/shapetrees/medical-record/shapetree#medicalRecords").toString(), location.getShapeTree());
        Assertions.assertEquals(getURI(server, "/ldp/data/medical-records/").toString(), location.getManagedResource());
        Assertions.assertEquals(getURI(server, "/ldp/data/medical-records/.shapetree#ln1"), location.getRootShapeTreeLocation());

    }

    @SneakyThrows
    @Test
    @Label("Plant Condition Shape tree")
    void plantConditionShapeTree() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/ldp/data/conditions/.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/ldp/data/conditions/?ext=shapetree", null));

        // Plant medical record on /ldp/data/medical-record
        DocumentResponse response = this.shapeTreeClient.plantShapeTree(this.context, getURI(server,"/ldp/data/conditions/"), getURI(server,"/static/shapetrees/medical-record/shapetree#conditions"), null);
        Assertions.assertEquals(201, response.getStatusCode());

    }

    @SneakyThrows
    @Test
    @Label("Create Data Instance")
    void createCondition() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("medicalRecord/conditions-container-locator"), "GET", "/ldp/data/conditions/?ext=shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("medicalRecord/condition-1-create-response"), "POST", "/ldp/data/conditions/condition1.ttl", null));
//        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/404"), "GET", "/ldp/data/conditions/condition1.ttl", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/ldp/data/conditions/condition1.ttl?ext=shapetree", null));

        DocumentResponse response = this.shapeTreeClient.postShapeTreeInstance(this.context,
                getURI(server, "/ldp/data/conditions/"),
                getURI(server, "http://hl7.org/fhir/Condition/example"),
                getURI(server, "/static/shapetrees/medical-record/shapetree#condition"),
                "condition1.ttl",
                false,
                getConditionTtl(),
                TEXT_TURTLE
        );
        Assertions.assertEquals(201, response.getStatusCode());

    }

    @SneakyThrows
    @Test
    @Label("Create Data Instance - fail validation")
    void createConditionInvalid() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("medicalRecord/conditions-container-locator"), "GET", "/ldp/data/conditions/?ext=shapetree", null));

        DocumentResponse response = this.shapeTreeClient.postShapeTreeInstance(this.context,
                getURI(server, "/ldp/data/conditions/"),
                getURI(server, "http://hl7.org/fhir/Condition/example"),
                getURI(server, "/static/shapetrees/medical-record/shapetree#condition"),
                "condition1.ttl",
                false,
                getInvalidConditionTtl(),
                TEXT_TURTLE
        );
        Assertions.assertEquals(422, response.getStatusCode());
    }

    @SneakyThrows
    @Test
    @Label("Create Data Instance - fail validation - but skip validation")
    void createConditionInvalidSkipValidation() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("medicalRecord/conditions-container-locator"), "GET", "/ldp/data/conditions/?ext=shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("medicalRecord/condition-1-create-response"), "POST", "/ldp/data/conditions/", null));

        // create invalid instance but skip validation
        this.shapeTreeClient.skipShapeTreeValidation(true);
        Assertions.assertTrue(this.shapeTreeClient.isShapeTreeValidationSkipped());
        DocumentResponse response = this.shapeTreeClient.postShapeTreeInstance(this.context,
                getURI(server, "/ldp/data/conditions/"),
                getURI(server, "http://hl7.org/fhir/Condition/example"),
                getURI(server, "/static/shapetrees/medical-record/shapetree#condition"),
                "condition1.ttl",
                false,
                getInvalidConditionTtl(),
                TEXT_TURTLE
        );
        Assertions.assertEquals(201, response.getStatusCode());
        this.shapeTreeClient.skipShapeTreeValidation(false);
        Assertions.assertFalse(this.shapeTreeClient.isShapeTreeValidationSkipped());
    }

    @SneakyThrows
    @Test
    @Label("Update Data Instance - Valid")
    void updateConditionValid() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("medicalRecord/condition-1"), "GET", "/ldp/data/conditions/condition1.ttl", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("medicalRecord/condition-1-locator"), "GET", "/ldp/data/conditions/condition1.ttl?ext=shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("medicalRecord/condition-1-update-response"), "PUT", "/ldp/data/conditions/condition1.ttl", null));

        // update condition instance

        DocumentResponse response = this.shapeTreeClient.putShapeTreeInstance(this.context,
                getURI(server,"/ldp/data/conditions/condition1.ttl"),
                getURI(server,"http://hl7.org/fhir/Condition/example"),
                getConditionTtl(),
                "text/turtle"
        );
        Assertions.assertEquals(200, response.getStatusCode());
    }

    @SneakyThrows
    @Test
    @Label("Update Data Instance via Patch")
    void updateConditionPatch() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // patch condition instance
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("medicalRecord/condition-1"), "GET", "/ldp/data/conditions/condition1.ttl", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("medicalRecord/condition-1-locator"), "GET", "/ldp/data/conditions/condition1.ttl?ext=shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("medicalRecord/condition-1-patch-response"), "PATCH", "/ldp/data/conditions/condition1.ttl", null));

        DocumentResponse response = this.shapeTreeClient.patchShapeTreeInstance(this.context,
                getURI(server,"/ldp/data/conditions/condition1.ttl"),
                getURI(server,"http://hl7.org/fhir/Condition/example"),
                "INSERT DATA { <#a> <#b> <#c> . }"
        );
        Assertions.assertEquals(204, response.getStatusCode());
    }

    @SneakyThrows
    @Test
    @Label("Fail to update condition with invalid PATCH")
    void failToUpdateConditionWithPatch() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("medicalRecord/condition-1"), "GET", "/ldp/data/conditions/condition1.ttl", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("medicalRecord/condition-1-locator"), "GET", "/ldp/data/conditions/condition1.ttl?ext=shapetree", null));

        // fail to validate on update with patch
        DocumentResponse response = this.shapeTreeClient.patchShapeTreeInstance(this.context,
                getURI(server,"/ldp/data/conditions/condition1.ttl"),
                getURI(server,"http://hl7.org/fhir/Condition/example"),
                "DELETE DATA { <http://hl7.org/fhir/Condition/example> a <http://hl7.org/fhir/Condition> . }"
        );
        Assertions.assertEquals(422, response.getStatusCode());
    }

    @SneakyThrows
    @Test
    @Label("Delete Data Instance ")
    void deleteDataInstance() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("medicalRecord/condition-1"), "GET", "/ldp/data/conditions/condition1.ttl", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("medicalRecord/condition-1-locator"), "GET", "/ldp/data/conditions/condition1.ttl?ext=shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("medicalRecord/condition-1-delete-response"), "DELETE", "/ldp/data/conditions/condition1.ttl", null));

        // delete data instance
        DocumentResponse response = this.shapeTreeClient.deleteShapeTreeInstance(this.context, getURI(server,"/ldp/data/conditions/condition1.ttl"));
        Assertions.assertEquals(204, response.getStatusCode());
    }

    public static String getConditionTtl() {
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

    public static String getInvalidConditionTtl() {
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
