package com.janeirodigital.shapetrees.core.models;

import com.janeirodigital.shapetrees.core.ShapeTreeFactory;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.helpers.GraphHelper;
import com.janeirodigital.shapetrees.core.vocabularies.ShapeTreeVocabulary;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.apache.jena.graph.Graph;
import org.apache.jena.graph.Node;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Triple;
import org.apache.jena.vocabulary.RDF;

import java.net.URI;
import java.net.URISyntaxException;
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

    private String id;
    private List<ShapeTreeLocation> locations = new ArrayList<>();   // Each ShapeTreeLocator has one or more ShapeTreeLocations

    public ShapeTreeLocator(String id) {
        this.id = id;
    }

    public URI getURI() throws URISyntaxException {
        return new URI(this.id);
    }

    public Graph getGraph() throws URISyntaxException {

        Graph locatorGraph = GraphHelper.getEmptyGraph();
        String resourceBase = this.getURI().toString() + "#";
        String locatorSubject = resourceBase + "locator";

        // <> st:hasShapeTreeLocator <#locator>
        locatorGraph.add(GraphHelper.newTriple(this.getURI().toString(), ShapeTreeVocabulary.HAS_SHAPE_TREE_LOCATOR, URI.create(locatorSubject)));

        // <#locator> a st:ShapeTreeLocator
        locatorGraph.add(GraphHelper.newTriple(locatorSubject, RDF.type.toString(), URI.create(ShapeTreeVocabulary.SHAPETREE_LOCATOR)));

        // For each location create a blank node and populate
        for (ShapeTreeLocation location : this.locations) {

            // <#locator> st:contains [ location1, location2 ]
            Node locationNode = NodeFactory.createBlankNode();
            locatorGraph.add(GraphHelper.newTriple(locatorSubject, ShapeTreeVocabulary.LOCATION, locationNode));

            locatorGraph.add(new Triple(locationNode, NodeFactory.createURI(ShapeTreeVocabulary.HAS_SHAPE_TREE), NodeFactory.createURI(location.getShapeTree())));
            locatorGraph.add(new Triple(locationNode, NodeFactory.createURI(ShapeTreeVocabulary.HAS_ROOT_SHAPE_TREE), NodeFactory.createURI(location.getRootShapeTree())));
            locatorGraph.add(new Triple(locationNode, NodeFactory.createURI(ShapeTreeVocabulary.HAS_ROOT_SHAPE_TREE_INSTANCE), NodeFactory.createURI(location.getRootShapeTreeInstance())));

            if (location.getShape() != null) {
                locatorGraph.add(new Triple(locationNode, NodeFactory.createURI(ShapeTreeVocabulary.SHAPE), NodeFactory.createURI(location.getShape())));
            }

            if (location.getFocusNode() != null) {
                locatorGraph.add(new Triple(locationNode, NodeFactory.createURI(ShapeTreeVocabulary.FOCUS_NODE), NodeFactory.createURI(location.getFocusNode())));
            }

        }

        return locatorGraph;
    }

    public void addShapeTreeLocation(ShapeTreeLocation location) {
        // Must check to ensure that there isn't already an equivalent location
        // TODO - Implement Add Shape Tree Location
        // Should probably generate an exception is the same one already exists
    }

    public ShapeTreeLocation getContainingShapeTreeLocation() throws URISyntaxException, ShapeTreeException {

        for (ShapeTreeLocation location : this.locations) {
            ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(URI.create(location.getShapeTree()));
            if (shapeTree.getContains() != null && !shapeTree.getContains().isEmpty()) {
                return location;
            }
        }

        return null;
    }

    public List<ShapeTreeLocation> getUpdatedShapeTreeLocations(ShapeTreeLocator otherLocator) throws URISyntaxException {

        ArrayList<ShapeTreeLocation> updatedLocations = new ArrayList<ShapeTreeLocation>();

        for (ShapeTreeLocation existingLocation : this.getLocations()) {
            for (ShapeTreeLocation otherLocation : otherLocator.getLocations()) {
                if (!existingLocation.equals(otherLocation)) {
                    updatedLocations.add(otherLocation);
                }
            }
        }
        return updatedLocations;
    }

    public static ShapeTreeLocator getShapeTreeLocatorFromGraph(String id, Graph shapeTreeMetadataGraph) {

        ShapeTreeLocator locator = new ShapeTreeLocator();

        // Assign the ID of the locator
        locator.setId(id);

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
                                                        NodeFactory.createURI(ShapeTreeVocabulary.LOCATION),
                                                        Node.ANY).toList();

        // For each st:location blank node, extract a new ShapeTreeLocation
        for (Triple locationNode : locationNodes) {

            // Find all of the triples for the current location blank node
            List<Triple> locationTriples = shapeTreeMetadataGraph.find(locationNode.getObject(), Node.ANY, Node.ANY).toList();

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
            locator.locations.add(new ShapeTreeLocation(shapeTree, rootShapeTree, rootShapeTreeInstance, focusNode, shape));

        }

        return locator;

    }



}
