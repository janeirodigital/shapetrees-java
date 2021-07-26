package com.janeirodigital.shapetrees.core.models;

import com.janeirodigital.shapetrees.core.ShapeTreeFactory;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

import java.net.URI;
import java.net.URISyntaxException;

/**
 * ShapeTreeLocation
 *
 * Shape Trees, §3:  Each shape tree location identifies a shape tree associated with the managed resource,
 * the focus node for shape validation, and the information needed to navigate the physical hierarchy in
 * which that managed resource resides.
 * https://shapetrees.org/TR/specification/#locator
*/
@Getter @Setter
@AllArgsConstructor @EqualsAndHashCode
public class ShapeTreeLocation {

    private String shapeTree;               // Identifies the shape tree to be associated with the managed resource
    private String rootShapeTree;           // Marks the primary, or parent shape tree in a physical hierarchy
    private String rootShapeTreeInstance;   // Marks the primary, or parent shape tree instance in a physical hierarchy
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
        this.rootShapeTree = rootShapeTree;
        this.rootShapeTreeInstance = rootShapeTreeInstance;
        this.focusNode = focusNode;

    }

    public URI getBaseUri () {

        if (this.uri == null || this.uri.getScheme() == null || this.uri.getSchemeSpecificPart() == null) {
            return null;
        }
        // Get the base URI of the resource
        URI base = new URI(this.uri.getScheme(), this.uri.getSchemeSpecificPart(), null);

        return base;

    }

}
