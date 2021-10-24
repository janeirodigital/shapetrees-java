package com.janeirodigital.shapetrees.core.models;

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
@Getter
@AllArgsConstructor @EqualsAndHashCode
public class ShapeTreeLocation {

    private String shapeTree;               // Identifies the shape tree to be associated with the managed resource
    private String managedResource;         // Identifies the resource that the shape tree location is managing
    private URI rootShapeTreeLocation;      // Identifies the root shape tree location
    private String focusNode;               // Identifies the focus node for shape validation in the managed resource
    private String shape;                   // Identifies the shape to which focusNode must conform
    private URI uri;

    public URI getBaseUri () throws URISyntaxException {

        if (this.uri == null || this.uri.getScheme() == null || this.uri.getSchemeSpecificPart() == null) {
            return null;
        }
        // Get the base URI of the resource
        return new URI(this.uri.getScheme(), this.uri.getSchemeSpecificPart(), null);

    }

    public void setUri(final URI uri) {
        this.uri = uri;
    }

    public static ShapeTreeLocation getShapeTreeLocationFromGraph(URI uri, Graph shapeTreeMetadataGraph) {

        String shapeTree = null;
        String managedResource = null;
        URI rootShapeTreeLocation = null;
        String focusNode = null;
        String shape = null;

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
                    shapeTree = locationTriple.getObject().getURI();
                    break;
                case ShapeTreeVocabulary.HAS_ROOT_SHAPE_TREE_LOCATION:
                    rootShapeTreeLocation = URI.create(locationTriple.getObject().getURI());
                    break;
                case ShapeTreeVocabulary.HAS_MANAGED_RESOURCE:
                    managedResource = locationTriple.getObject().getURI();
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
        return new ShapeTreeLocation(shapeTree,managedResource,rootShapeTreeLocation,focusNode,shape, uri);
    }

    public boolean isRootLocation() {
        return this.getUri().equals(this.getRootShapeTreeLocation());
    }

}
