package com.janeirodigital.shapetrees.test;

import jdk.jfr.Label;
import lombok.SneakyThrows;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.net.URI;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class MedicalRecordTests extends BaseShapeTreeTest {

    @Order(1)
    @SneakyThrows
    @Test
    @Label("Plant Medical Record")
    void plantMedicalRecord() {
        plant(new URI(ROOT_PATH), new URI("http://localhost:9999/static/medicalrecord/nhs-shapetree.jsonld#medicalRecord"), "medical-record");
        ensureExistsHasMetadataWithPredicateValue(new URI(ROOT_PATH + "medical-record"), new URI(SHAPE_TREE_INSTANCE_PATH_PREDICATE), ".");

    }

}
