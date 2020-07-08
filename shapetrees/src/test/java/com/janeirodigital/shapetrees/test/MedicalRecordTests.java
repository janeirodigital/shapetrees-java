package com.janeirodigital.shapetrees.test;

import com.janeirodigital.shapetrees.ShapeTreeVocabulary;
import jdk.jfr.Label;
import lombok.SneakyThrows;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.net.URI;
import java.util.Collections;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class MedicalRecordTests extends BaseShapeTreeTest {

    public MedicalRecordTests() {
        super(new MockEcosystem());
    }

    @Order(1)
    @SneakyThrows
    @Test
    @Label("Plant Medical Record")
    void plantMedicalRecord() {
        plant(new URI(ROOT_PATH), Collections.singletonList(new URI("http://localhost:9999/static/medicalrecord/nhs-shapetree.jsonld#medicalRecord")), "medical-record", null);
        ensureExistsHasMetadataWithPredicateValue(new URI(ROOT_PATH + "medical-record"), new URI(ShapeTreeVocabulary.HAS_SHAPE_TREE_INSTANCE_PATH), ".");

    }

}
