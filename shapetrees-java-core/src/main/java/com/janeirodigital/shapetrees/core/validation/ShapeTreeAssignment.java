package com.janeirodigital.shapetrees.core.validation;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.vocabularies.RdfVocabulary;
import com.janeirodigital.shapetrees.core.vocabularies.ShapeTreeVocabulary;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import org.apache.jena.graph.Graph;
import org.apache.jena.graph.Node;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Triple;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.List;
import java.util.Objects;

/**
 * ShapeTreeAssignment
 *
 * Shape Trees, §3:  Each shape tree assignment identifies a shape tree associated with the managed resource,
 * the focus node for shape validation, and the information needed to navigate the physical hierarchy in
 * which that managed resource resides.
 * https://shapetrees.org/TR/specification/#manager
*/
@Getter
@EqualsAndHashCode
public class ShapeTreeAssignment {

    private final URL shapeTree;               // Identifies the shape tree to be associated with the managed resource
    private final URL managedResource;         // Identifies the resource managed by the shape tree assignment
    private final URL rootAssignment;          // Identifies the root shape tree assignment
    private final URL focusNode;               // Identifies the focus node for shape validation in the managed resource
    private final URL shape;                   // Identifies the shape to which focusNode must conform
    private final URL url;

    public ShapeTreeAssignment(URL shapeTree, URL managedResource, URL rootAssignment, URL focusNode, URL shape, URL url) throws ShapeTreeException {
        try {
            this.shapeTree = Objects.requireNonNull(shapeTree, "Must provide an assigned shape tree");
            this.managedResource = Objects.requireNonNull(managedResource, "Must provide a shape tree context");
            this.rootAssignment = Objects.requireNonNull(rootAssignment, "Must provide a root shape tree assignment");
            this.url = Objects.requireNonNull(url, "Must provide a url for shape tree assignment");
            if (shape != null) {
                this.shape = shape;
                this.focusNode = Objects.requireNonNull(focusNode, "Must provide a focus node for shape validation");
            } else {
                this.shape = null;
                if (focusNode != null) {
                    throw new IllegalStateException("Cannot provide a focus node when no shape has been provided");
                }
                this.focusNode = null;
            }
        } catch (NullPointerException|IllegalStateException ex) {
            throw new ShapeTreeException(500, "Failed to initialize shape tree assignment: " + ex.getMessage());
        }
    }

    public static ShapeTreeAssignment getFromGraph(URL url, Graph managerGraph) throws MalformedURLException, ShapeTreeException {

        URL shapeTree = null;
        URL managedResource = null;
        URL rootAssignment = null;
        URL focusNode = null;
        URL shape = null;

        // Look up the ShapeTreeAssignment in the ManagerResource Graph via its URL
        List<Triple> assignmentTriples = managerGraph.find(NodeFactory.createURI(url.toString()), Node.ANY, Node.ANY).toList();

        // A valid assignment must have at least a shape tree, managed resource, and root assignment urls
        if (assignmentTriples.size() < 3) {
            throw new IllegalStateException("Incomplete shape tree assignment, Only " + assignmentTriples.size() + " attributes found");
        }

        // Lookup and assign each triple in the nested ShapeTreeAssignment
        for (Triple assignmentTriple : assignmentTriples) {

            switch (assignmentTriple.getPredicate().getURI()) {
                case RdfVocabulary.TYPE:
                    if (!assignmentTriple.getObject().isURI() ||
                        !assignmentTriple.getObject().getURI().equals(ShapeTreeVocabulary.SHAPETREE_ASSIGNMENT)) {
                        throw new IllegalStateException("Unexpected value: " + assignmentTriple.getPredicate().getURI());
                    }
                    break;
                case ShapeTreeVocabulary.ASSIGNS_SHAPE_TREE:
                    shapeTree = new URL(assignmentTriple.getObject().getURI());
                    break;
                case ShapeTreeVocabulary.HAS_ROOT_ASSIGNMENT:
                    rootAssignment = new URL(assignmentTriple.getObject().getURI());
                    break;
                case ShapeTreeVocabulary.MANAGES_RESOURCE:
                    managedResource = new URL(assignmentTriple.getObject().getURI());
                    break;
                case ShapeTreeVocabulary.SHAPE:
                    shape = new URL(assignmentTriple.getObject().getURI());
                    break;
                case ShapeTreeVocabulary.FOCUS_NODE:
                    focusNode = new URL(assignmentTriple.getObject().getURI());
                    break;
                default:
                    throw new IllegalStateException("Unexpected value: " + assignmentTriple.getPredicate().getURI());
            }
        }
        return new ShapeTreeAssignment(shapeTree,managedResource,rootAssignment,focusNode,shape, url);
    }

    public boolean isRootAssignment() {
        return this.getUrl().equals(this.getRootAssignment());
    }

}
