package com.janeirodigital.shapetrees.test;

import com.janeirodigital.shapetrees.ShapeTreeFactory;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.net.URI;

@Slf4j
public class ShapeTreeParsingTests {

    @SneakyThrows
    @Test
    @DisplayName("Parse Tree")
    void parseShapeTree() {
        ShapeTreeFactory.getShapeTreeStep(new URI("http://localhost:9999/static/gh-flat/gh-flat-shapetree.jsonld#orgs"));
        ShapeTreeFactory.getShapeTreeStep(new URI("http://localhost:9999/static/gh-flat/gh-flat-shapetree.jsonld#users"));
        ShapeTreeFactory.getShapeTreeStep(new URI("http://localhost:9999/static/gh-flat/gh-flat-shapetree.jsonld#user"));
    }

}
