package com.janeirodigital.shapetrees.test;

import com.janeirodigital.shapetrees.ShapeTreeFactory;
import com.janeirodigital.shapetrees.enums.RecursionMethods;
import com.janeirodigital.shapetrees.model.ReferencedShapeTree;
import com.janeirodigital.shapetrees.model.ShapeTree;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.util.Iterator;

@Slf4j
public class ShapeTreeParsingTests {

    @SneakyThrows
    @Test
    @DisplayName("Parse Tree")
    void parseShapeTree() {
        ShapeTreeFactory.getShapeTree(new URI("http://localhost:9999/static/gh-flat/gh-flat-shapetree.jsonld#orgs"));
        ShapeTreeFactory.getShapeTree(new URI("http://localhost:9999/static/gh-flat/gh-flat-shapetree.jsonld#users"));
        ShapeTreeFactory.getShapeTree(new URI("http://localhost:9999/static/gh-flat/gh-flat-shapetree.jsonld#user"));
    }

    @SneakyThrows
    @Test
    @DisplayName("Get ReferencedShapeTrees DepthFirst")
    void traverseShapeTreeDFS() {
        log.info("Depth-First:");
        ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(new URI("http://localhost:9999/static/gh-flat/gh-flat-shapetree.jsonld#org"));
        Iterator<ReferencedShapeTree> referencedShapeTrees = shapeTree.getReferencedShapeTrees(RecursionMethods.DEPTH_FIRST);
        while (referencedShapeTrees.hasNext()) {
            ReferencedShapeTree shapeTreeReference = referencedShapeTrees.next();
            log.info(shapeTreeReference.getReferencedShapeTree().toString());
        }
    }

    @SneakyThrows
    @Test
    @DisplayName("Get ReferencedShapeTrees BreadthFirst")
    void traverseShapeTreeBFS() {
        log.info("Breadth-First:");
        ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(new URI("http://localhost:9999/static/gh-flat/gh-flat-shapetree.jsonld#org"));
        Iterator<ReferencedShapeTree> referencedShapeTrees = shapeTree.getReferencedShapeTrees(RecursionMethods.BREADTH_FIRST);
        while (referencedShapeTrees.hasNext()) {
            ReferencedShapeTree shapeTreeReference = referencedShapeTrees.next();
            log.info(shapeTreeReference.getReferencedShapeTree().toString());
        }
    }


}
