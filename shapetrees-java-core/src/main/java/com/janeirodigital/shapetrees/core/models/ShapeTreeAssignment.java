package com.janeirodigital.shapetrees.core.models;

import com.janeirodigital.shapetrees.core.vocabularies.RdfVocabulary;
import com.janeirodigital.shapetrees.core.vocabularies.ShapeTreeVocabulary;
import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import org.apache.jena.graph.Graph;
import org.apache.jena.graph.Node;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Triple;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.List;

/**
 * ShapeTreeAssignment
 *
 * Shape Trees, §3:  Each shape tree assignment identifies a shape tree associated with the managed resource,
 * the focus node for shape validation, and the information needed to navigate the physical hierarchy in
 * which that managed resource resides.
 * https://shapetrees.org/TR/specification/#manager
*/
@Getter
@AllArgsConstructor @EqualsAndHashCode
public class ShapeTreeAssignment {

    private URL shapeTree;               // Identifies the shape tree to be associated with the managed resource
    private URL managedResource;         // Identifies the resource managed by the shape tree assignment
    private URL rootAssignment;          // Identifies the root shape tree assignment
    private URL focusNode;               // Identifies the focus node for shape validation in the managed resource
    private URL shape;                   // Identifies the shape to which focusNode must conform
    private URL url;

    public void setUrl(final URL url) {
        this.url = url;
    }

    public static ShapeTreeAssignment getFromGraph(URL url, Graph managerGraph) throws MalformedURLException {

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
