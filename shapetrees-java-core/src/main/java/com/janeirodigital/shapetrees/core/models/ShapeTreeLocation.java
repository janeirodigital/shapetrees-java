package com.janeirodigital.shapetrees.core.models;

import com.janeirodigital.shapetrees.core.vocabularies.ShapeTreeVocabulary;
import lombok.*;
import org.apache.jena.graph.Graph;
import org.apache.jena.graph.Node;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Triple;

import java.net.URL;
import java.net.MalformedURLException;
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

    private URL shapeTree;               // Identifies the shape tree to be associated with the managed resource
    private String managedResource;         // Identifies the resource that the shape tree location is managing
    private URL rootShapeTreeLocation;      // Identifies the root shape tree location
    private String focusNode;               // Identifies the focus node for shape validation in the managed resource
    private String shape;                   // Identifies the shape to which focusNode must conform
    private URL url;
/*
    public URL getBaseUri () throws MalformedURLException {

        if (this.url == null || this.url.getScheme() == null || this.url.getSchemeSpecificPart() == null) {
            return null;
        }
        // Get the base URL of the resource
        return new URL(this.url.getScheme(), this.url.getSchemeSpecificPart(), null);

    }
*/
    public void setUrl(final URL url) {
        this.url = url;
    }

    public static ShapeTreeLocation getShapeTreeLocationFromGraph(URL uri, Graph shapeTreeMetadataGraph) throws MalformedURLException {

        URL shapeTree = null;
        String managedResource = null;
        URL rootShapeTreeLocation = null;
        String focusNode = null;
        String shape = null;

        // Look up the ShapeTreeLocation in the Metadata Graph via its URL
        List<Triple> locationTriples = shapeTreeMetadataGraph.find(NodeFactory.createURI(uri.toString()), Node.ANY, Node.ANY).toList();

        // A valid location must have at least a shape tree, managed resource, and root location url
        if (locationTriples.size() < 3) {
            throw new IllegalStateException("Incomplete shape tree location, Only " + locationTriples.size() + " attributes found");
        }

        // Lookup and assign each triple in the nested ShapeTreeLocation
        for (Triple locationTriple : locationTriples) {

            switch (locationTriple.getPredicate().getURI()) {
                case ShapeTreeVocabulary.HAS_SHAPE_TREE:
                    shapeTree = new URL(locationTriple.getObject().getURI());
                    break;
                case ShapeTreeVocabulary.HAS_ROOT_SHAPE_TREE_LOCATION:
                    rootShapeTreeLocation = new URL(locationTriple.getObject().getURI());
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
        return this.getUrl().equals(this.getRootShapeTreeLocation());
    }

}
