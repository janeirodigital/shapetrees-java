package com.janeirodigital.shapetrees.test;

import com.janeirodigital.shapetrees.ShapeTreeFactory;
import com.janeirodigital.shapetrees.enums.RecursionMethod;
import com.janeirodigital.shapetrees.model.ReferencedShapeTreeStep;
import com.janeirodigital.shapetrees.model.ShapeTreeStep;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.util.Iterator;
import java.util.List;

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

    @SneakyThrows
    @Test
    @DisplayName("Get ReferencedSteps DepthFirst")
    void traverseShapeTreeStepsDFS() {
        log.info("Depth-First:");
        ShapeTreeStep step = ShapeTreeFactory.getShapeTreeStep(new URI("http://localhost:9999/static/gh-flat/gh-flat-shapetree.jsonld#org"));
        Iterator<ReferencedShapeTreeStep> steps = step.getReferencedSteps(RecursionMethod.DEPTH_FIRST);
        while (steps.hasNext()) {
            ReferencedShapeTreeStep refStep = steps.next();
            log.info(refStep.getReferencedStep().toString());
        }
    }

    @SneakyThrows
    @Test
    @DisplayName("Get ReferencedSteps BreadthFirst")
    void traverseShapeTreeStepsBFS() {
        log.info("Breadth-First:");
        ShapeTreeStep step = ShapeTreeFactory.getShapeTreeStep(new URI("http://localhost:9999/static/gh-flat/gh-flat-shapetree.jsonld#org"));
        Iterator<ReferencedShapeTreeStep> steps = step.getReferencedSteps(RecursionMethod.BREADTH_FIRST);
        while (steps.hasNext()) {
            ReferencedShapeTreeStep refStep = steps.next();
            log.info(refStep.getReferencedStep().toString());
        }
    }


}
