package com.janeirodigital.shapetrees.core.models;

import com.janeirodigital.shapetrees.core.vocabularies.ShapeTreeVocabulary;
import lombok.AllArgsConstructor;
import lombok.Getter;
import org.apache.jena.graph.Graph;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Triple;

import java.util.ArrayList;
import java.util.List;

@Getter @AllArgsConstructor
public class ShapeTreeLocator {
    private final String rootShapeTree;
    private final String shapeTree;
    private final String shapeTreeRoot;

    public static List<ShapeTreeLocator> getShapeTreeLocatorsFromGraph(Graph shapeTreeMetadataGraph) {
        List<ShapeTreeLocator> locators = new ArrayList<>();
        // ericP: could shapeTreeMetadataGraph be null if RemoteResource.getGraph() was called when !this._exists
        List<Triple> hasShapeTreeLocatorTriples = shapeTreeMetadataGraph.find(null, NodeFactory.createURI(ShapeTreeVocabulary.HAS_SHAPE_TREE_LOCATOR), null).toList();
        for (Triple hasShapeTreeLocatorTriple : hasShapeTreeLocatorTriples) {
            String locatorURI = hasShapeTreeLocatorTriple.getObject().getURI();

            List<Triple> locatorTriples = shapeTreeMetadataGraph.find(NodeFactory.createURI(locatorURI), null, null).toList();
            String shapeTreeRoot = null;
            String rootShapeTree = null;
            String shapeTree = null;
            for (Triple locatorTriple : locatorTriples) {
                switch (locatorTriple.getPredicate().getURI()) {
                    case ShapeTreeVocabulary.HAS_SHAPE_TREE:
                        shapeTree = locatorTriple.getObject().getURI();
                        break;
                    case ShapeTreeVocabulary.HAS_SHAPE_TREE_INSTANCE_ROOT:
                        shapeTreeRoot = locatorTriple.getObject().getURI();
                        break;
                    case ShapeTreeVocabulary.HAS_ROOT_SHAPE_TREE:
                        rootShapeTree = locatorTriple.getObject().getURI();
                        break;
                    default:
                        throw new IllegalStateException("Unexpected value: " + locatorTriple.getPredicate().getURI());
                }
            }
            locators.add(new ShapeTreeLocator(rootShapeTree, shapeTree, shapeTreeRoot));
        }

        return locators;
    }
}
