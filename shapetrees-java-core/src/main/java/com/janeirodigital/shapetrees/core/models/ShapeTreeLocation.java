package com.janeirodigital.shapetrees.core.models;

import com.janeirodigital.shapetrees.core.ShapeTreeFactory;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.vocabularies.ShapeTreeVocabulary;
import lombok.*;
import org.apache.jena.graph.Graph;
import org.apache.jena.graph.Node;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Triple;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;

/**
 * ShapeTreeLocation
 *
 * Shape Trees, §3:  Each shape tree location identifies a shape tree associated with the managed resource,
 * the focus node for shape validation, and the information needed to navigate the physical hierarchy in
 * which that managed resource resides.
 * https://shapetrees.org/TR/specification/#locator
*/
@Getter @Setter
@AllArgsConstructor @NoArgsConstructor @EqualsAndHashCode
public class ShapeTreeLocation {

    private String shapeTree;               // Identifies the shape tree to be associated with the managed resource
    private String managedResource;         // Identifies the resource that the shape tree location is managing
    private URI rootShapeTreeLocation;      // Identifies the root shape tree location
    private String focusNode;               // Identifies the focus node for shape validation in the managed resource
    private String shape;                   // Identifies the shape to which focusNode must conform
    private URI uri;

    // Provider a constructor that can lookup shape by
    public ShapeTreeLocation(String shapeTree, String rootShapeTree, String rootShapeTreeInstance, String focusNode) throws URISyntaxException, ShapeTreeException {

        ShapeTree remoteShapeTree = ShapeTreeFactory.getShapeTree(URI.create(shapeTree));

        if (remoteShapeTree.getShape() == null && focusNode != null) {
            throw new ShapeTreeException(400, "Focus node provided but no shape is present for validation");
        }

        if (remoteShapeTree != null) { this.shape = remoteShapeTree.getShape(); }

        this.shapeTree = shapeTree;
        this.managedResource = null;
        this.rootShapeTreeLocation = null;
        this.focusNode = focusNode;

    }

    public URI getBaseUri () throws URISyntaxException {

        if (this.uri == null || this.uri.getScheme() == null || this.uri.getSchemeSpecificPart() == null) {
            return null;
        }
        // Get the base URI of the resource
        URI base = new URI(this.uri.getScheme(), this.uri.getSchemeSpecificPart(), null);

        return base;

    }

    public static ShapeTreeLocation getShapeTreeLocationFromGraph(URI uri, Graph shapeTreeMetadataGraph) {

        ShapeTreeLocation location = new ShapeTreeLocation();

        location.setUri(uri);

        // Look up the ShapeTreeLocation in the Metadata Graph via its URI
        List<Triple> locationTriples = shapeTreeMetadataGraph.find(NodeFactory.createURI(uri.toString()), Node.ANY, Node.ANY).toList();

        // A valid location must have at least a shape tree, managed resource, and root location uri
        if (locationTriples.size() < 3) {
            throw new IllegalStateException("Incomplete shape tree location, Only " + locationTriples.size() + " attributes found");
        }

        // Lookup and assign each triple in the nested ShapeTreeLocation
        for (Triple locationTriple : locationTriples) {

            switch (locationTriple.getPredicate().getURI()) {
                case ShapeTreeVocabulary.HAS_SHAPE_TREE:
                    location.shapeTree = locationTriple.getObject().getURI();
                    break;
                case ShapeTreeVocabulary.HAS_ROOT_SHAPE_TREE_LOCATION:
                    location.rootShapeTreeLocation = URI.create(locationTriple.getObject().getURI());
                    break;
                case ShapeTreeVocabulary.HAS_MANAGED_RESOURCE:
                    location.managedResource = locationTriple.getObject().getURI();
                    break;
                case ShapeTreeVocabulary.SHAPE:
                    location.shape = locationTriple.getObject().getURI();
                    break;
                case ShapeTreeVocabulary.FOCUS_NODE:
                    location.focusNode = locationTriple.getObject().getURI();
                    break;
                default:
                    throw new IllegalStateException("Unexpected value: " + locationTriple.getPredicate().getURI());
            }
        }
        return location;
    }

    public boolean isRootLocation() {
        return this.getUri().equals(this.getRootShapeTreeLocation());
    }

}
