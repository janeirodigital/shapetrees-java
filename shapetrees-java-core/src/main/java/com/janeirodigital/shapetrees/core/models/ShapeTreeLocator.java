package com.janeirodigital.shapetrees.core.models;

import com.janeirodigital.shapetrees.core.vocabularies.ShapeTreeVocabulary;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.apache.jena.graph.Graph;
import org.apache.jena.graph.Node;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Triple;

import java.util.ArrayList;
import java.util.List;

/**
 * ShapeTreeLocator
 *
 * Shape Trees, §3:  A shape tree locator associates a managed resource with one or more shape trees. No more
 * than one shape tree locator may be associated with a managed resource. A shape tree locator includes
 * one or more shape tree locations via st:location.
 * https://shapetrees.org/TR/specification/#locator
*/
@Getter @Setter @AllArgsConstructor @NoArgsConstructor
public class ShapeTreeLocator {

    private List<ShapeTreeLocation> locations;     // Each ShapeTreeLocator has one or more ShapeTreeLocations

    public static ShapeTreeLocator getShapeTreeLocatorFromGraph(Graph shapeTreeMetadataGraph) {

        ShapeTreeLocator locator = new ShapeTreeLocator();

        // Each ShapeTreeLocator has one or more ShapeTreeLocations
        List<ShapeTreeLocation> locations = new ArrayList<>();

        // Look up the ShapeTreeLocator in the Metadata Graph via (any subject node, rdf:type, st:ShapeTreeLocator)
        List<Triple> shapeTreeLocatorTriples = shapeTreeMetadataGraph.find(Node.ANY,
                                                                           NodeFactory.createURI(ShapeTreeVocabulary.HAS_SHAPE_TREE_LOCATOR),
                                                                           Node.ANY).toList();

        // Shape Trees, §3: No more than one shape tree locator may be associated with a managed resource.
        // https://shapetrees.org/TR/specification/#locator
        if (shapeTreeLocatorTriples.size() > 1) {
            throw new IllegalStateException("Multiple ShapeTreeLocator instances found: " + shapeTreeLocatorTriples.size());
        } else if (shapeTreeLocatorTriples.size() < 1) {
            // Given the fact that a locator resource exists, there should never be a case where the locator resource
            // exists but no locator is found inside of it.
            throw new IllegalStateException("No ShapeTreeLocator instances found: " + shapeTreeLocatorTriples.size());
        }

        // Get the URI of the ShapeTreeLocator subject node
        String locatorURI = shapeTreeLocatorTriples.get(0).getObject().getURI();

        // Look up ShapeTreeLocation nested blank nodes (locator subject node, st:location, any st:location nodes).
        // There should be one result per nested ShapeTreeLocation, each identified by a blank node.
        // Shape Trees, §3: A shape tree locator includes one or more shape tree locations via st:location
        // https://shapetrees.org/TR/specification/#locator
        List<Triple> locationNodes = shapeTreeMetadataGraph.find(NodeFactory.createURI(locatorURI),
                                                        NodeFactory.createURI(ShapeTreeVocabulary.SHAPETREE_LOCATION),
                                                        Node.ANY).toList();

        // For each st:location blank node, extract a new ShapeTreeLocation
        for (Triple locationNode : locationNodes) {

            // Find all of the triples for the current location blank node
            List<Triple> locationTriples = shapeTreeMetadataGraph.find(NodeFactory.createBlankNode(locationNode.getObject().toString()),
                                                                        Node.ANY,
                                                                        Node.ANY).toList();

            String shapeTree = null;
            String rootShapeTree = null;
            String rootShapeTreeInstance = null;
            String shape = null;
            String focusNode = null;

            // Lookup and assign each triple in the nested ShapeTreeLocation
            for (Triple locationTriple : locationTriples) {

                switch (locationTriple.getPredicate().getURI()) {
                    case ShapeTreeVocabulary.HAS_SHAPE_TREE:
                        shapeTree = locationTriple.getObject().getURI();
                        break;
                    case ShapeTreeVocabulary.HAS_ROOT_SHAPE_TREE:
                        rootShapeTree = locationTriple.getObject().getURI();
                        break;
                    case ShapeTreeVocabulary.HAS_ROOT_SHAPE_TREE_INSTANCE:
                        rootShapeTreeInstance = locationTriple.getObject().getURI();
                        break;
                    case ShapeTreeVocabulary.SHAPE:
                        shape = locationTriple.getObject().getURI();
                        break;
                    case ShapeTreeVocabulary.FOCUS_NODE:
                        focusNode = locationTriple.getObject().getURI();
                        break;
                    default:
                        throw new IllegalStateException("Unexpected value: " + locationTriple.getPredicate().getURI());
                }

            }

            // Create a new Shape Tree Location from the assigned triples and add it to the array of locations
            locations.add(new ShapeTreeLocation(shapeTree, rootShapeTree, rootShapeTreeInstance, focusNode, shape));

        }

        // Add the array of Shape Tree Locations to the locator
        locator.setLocations(locations);

        return locator;
    }

}
