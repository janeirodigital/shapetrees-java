package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.helpers.GraphHelper;
import com.janeirodigital.shapetrees.core.vocabularies.RdfVocabulary;
import com.janeirodigital.shapetrees.core.vocabularies.ShapeTreeVocabulary;
import lombok.Getter;
import org.apache.commons.lang3.RandomStringUtils;
import org.apache.jena.graph.Graph;
import org.apache.jena.graph.Node;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Triple;
import org.apache.jena.vocabulary.RDF;

import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import static com.janeirodigital.shapetrees.core.helpers.GraphHelper.urlToUri;


/**
 * ShapeTreeManager
 *
 * Shape Trees, §3:  A shape tree manager associates a managed resource with one or more shape trees. No more
 * than one shape tree manager may be associated with a managed resource. A shape tree manager includes
 * one or more shape tree assignments via st:hasAssignment.
 * https://shapetrees.org/TR/specification/#manager
*/
@Getter
public class ShapeTreeManager {

    private final URL id;
    // Each ShapeTreeManager has one or more ShapeTreeAssignments
    private final List<ShapeTreeAssignment> assignments = new ArrayList<>(); // TODO: try Map<URL, ShapeTreeAssignment>, makes getContainingAssignments() redundant against getAssignments()

    /**
     * Constructor for a new ShapeTreeManager
     * @param id URL of the ShapeTreeManager resource
     */
    public
    ShapeTreeManager(URL id) {
        this.id = id;
    }

    /**
     * Get the URL (identifier) of the ShapeTreeManager
     * @return URL identifier of the ShapeTreeManager
     */
    protected URL
    getUrl() {
        return this.id;
    }

    /**
     * Get the ShapeTreeManager as an RDF Graph
     * @return Graph of the ShapeTreeManager
     * @throws ShapeTreeException
     */
    public Graph
    getGraph() throws ShapeTreeException {

        Graph managerGraph = GraphHelper.getEmptyGraph();
        String managerSubject = this.getUrl().toString();

        // <> a st:Manager
        managerGraph.add(GraphHelper.newTriple(managerSubject, RDF.type.toString(), GraphHelper.knownUrl(ShapeTreeVocabulary.SHAPETREE_MANAGER)));

        // For each assignment create a blank node and populate
        for (ShapeTreeAssignment assignment : this.assignments) {

            // <> st:hasAssignment <assignment1>, <assignment2>
            managerGraph.add(GraphHelper.newTriple(managerSubject, ShapeTreeVocabulary.HAS_ASSIGNMENT, assignment.getUrl()));

            final URI subject = urlToUri(assignment.getUrl());
            managerGraph.add(GraphHelper.newTriple(subject, URI.create(ShapeTreeVocabulary.ASSIGNS_SHAPE_TREE), assignment.getShapeTree()));
            managerGraph.add(GraphHelper.newTriple(subject, URI.create(ShapeTreeVocabulary.MANAGES_RESOURCE), assignment.getManagedResource()));
            managerGraph.add(GraphHelper.newTriple(subject, URI.create(ShapeTreeVocabulary.HAS_ROOT_ASSIGNMENT), assignment.getRootAssignment()));

            if (assignment.getShape() != null) {
                managerGraph.add(GraphHelper.newTriple(subject, URI.create(ShapeTreeVocabulary.SHAPE), assignment.getShape()));
            }

            if (assignment.getFocusNode() != null) {
                managerGraph.add(GraphHelper.newTriple(subject, URI.create(ShapeTreeVocabulary.FOCUS_NODE), assignment.getFocusNode()));
            }

        }

        return managerGraph;
    }

    /**
     * Add a {@link com.janeirodigital.shapetrees.core.ShapeTreeAssignment} to the ShapeTreeManager.
     * @param assignment Shape tree assignment to add
     * @throws ShapeTreeException
     */
    public void
    addAssignment(ShapeTreeAssignment assignment) throws ShapeTreeException {

        if (assignment == null) {
            throw new ShapeTreeException(500, "Must provide a non-null assignment to an initialized List of assignments");
        }

        if (!this.assignments.isEmpty()) {
            for (ShapeTreeAssignment existingAssignment : this.assignments) {
                if (existingAssignment.equals(assignment)) {
                    throw new ShapeTreeException(422, "Identical shape tree assignment cannot be added to Shape Tree Manager: " + this.id);
                }
            }
        }

        this.assignments.add(assignment);

    }

    /**
     * Generates or "mints" a URL for a new ShapeTreeAssignment
     * @return URL minted for a new shape tree assignment
     */
    public URL
    mintAssignmentUrl() {

        String fragment = RandomStringUtils.random(8, true, true);
        String assignmentString = this.getUrl().toString() + "#" + fragment;

        final URL assignmentUrl;
        try {
            assignmentUrl = new URL(assignmentString);
        } catch (MalformedURLException ex) {
            throw new IllegalStateException("Minted illegal URL <" + assignmentString + "> - " + ex.getMessage());
        }

        return assignmentUrl;
    }

    /**
     * Ensure a proposed URL for a new ShapeTreeAssigment doesn't conflict with
     * other assignment URLs already allocated for the ShapeTreeManager
     * @param proposedAssignmentUrl URL of the proposed shape tree assignment
     * @return Minted URL for a new shape tree assignment
     */
    public URL
    mintAssignmentUrl(URL proposedAssignmentUrl) {

        for (ShapeTreeAssignment assignment : this.assignments) {
            if (assignment.getUrl().equals(proposedAssignmentUrl)) {
                // If we somehow managed to randomly generate a location URL that already exists, generate another
                return mintAssignmentUrl();
            }
        }
        return proposedAssignmentUrl;
    }

    public List<ShapeTreeAssignment>
    getContainingAssignments() throws ShapeTreeException {

        ArrayList<ShapeTreeAssignment> containingAssignments = new ArrayList<>();

        for (ShapeTreeAssignment assignment : this.assignments) {
            ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(assignment.getShapeTree());
            if (!shapeTree.getContains().isEmpty()) {
                containingAssignments.add(assignment);
            }
        }

        return containingAssignments;
    }

    public static ShapeTreeManager getFromGraph(URL id, Graph managerGraph) throws ShapeTreeException {

         ShapeTreeManager manager = new ShapeTreeManager(id);

        // Look up the ShapeTreeManager in the ManagerResource Graph via (any subject node, rdf:type, st:ShapeTreeManager)
        List<Triple> managerTriples = managerGraph.find(Node.ANY,
                                                        NodeFactory.createURI(RdfVocabulary.TYPE),
                                                        NodeFactory.createURI(ShapeTreeVocabulary.SHAPETREE_MANAGER)).toList();

        // Shape Trees, §3: No more than one shape tree manager may be associated with a managed resource.
        // https://shapetrees.org/TR/specification/#manager
        if (managerTriples.size() > 1) {
            throw new IllegalStateException("Multiple ShapeTreeManager instances found: " + managerTriples.size());
        } else if (managerTriples.isEmpty()) {
            // Given the fact that a manager resource exists, there should never be a case where the manager resource
            // exists but no manager is found inside of it.
            throw new IllegalStateException("No ShapeTreeManager instances found: " + managerTriples.size()); // TODO: isn't that always 0?
        }

        // Get the URL of the ShapeTreeManager subject node
        String managerUrl = managerTriples.get(0).getSubject().getURI();

        // Look up ShapeTreeAssignment nodes (manager subject node, st:hasAssignment, any st:hasAssignment nodes).
        // There should be one result per nested ShapeTreeAssignment, each identified by a unique url.
        // Shape Trees, §3: A shape tree manager includes one or more shape tree assignments via st:hasAssignment
        // https://shapetrees.org/TR/specification/#manager
        final Node s = NodeFactory.createURI(managerUrl);
        final Node stAssignment = NodeFactory.createURI(ShapeTreeVocabulary.HAS_ASSIGNMENT);
        List<Triple> assignmentNodes = managerGraph.find(s, stAssignment, Node.ANY).toList();

        // For each st:hasAssignment node, extract a new ShapeTreeAssignment
        for (Triple assignmentNode : assignmentNodes) {
            ShapeTreeAssignment assignment = null;
            try {
                assignment = ShapeTreeAssignment.getFromGraph(new URL(assignmentNode.getObject().getURI()), managerGraph);
            } catch (MalformedURLException e) {
                throw new ShapeTreeException(500, "Object of { "+s+" "+ stAssignment +" "+assignmentNode.getObject()+" } must be a URL.");
            }
            manager.assignments.add(assignment);
        }

        return manager;

    }

    public ShapeTreeAssignment getAssignmentForShapeTree(URL shapeTreeUrl) { // TODO: return list of assignments with same ST but different roots

        if (this.assignments.isEmpty()) { return null; }

        for (ShapeTreeAssignment assignment : this.assignments) {
            if (assignment.getShapeTree().equals(shapeTreeUrl)) { return assignment; }
        }
        return null;
    }

    // Given a root assignment, lookup the corresponding assignment in a shape tree manager that has the same root assignment
    public ShapeTreeAssignment getAssignmentForRoot(ShapeTreeAssignment rootAssignment) {
        if (this.getAssignments() == null || this.getAssignments().isEmpty()) { return null; }

        for (ShapeTreeAssignment assignment : this.getAssignments()) {
            if (rootAssignment.getUrl().equals(assignment.getRootAssignment())) {
                return assignment;
            }
        }
        return null;
    }

    public void removeAssignment(ShapeTreeAssignment assignment) {

        if (assignment == null) {
            throw new IllegalStateException("Cannot remove a null assignment");
        }

        if (this.assignments.isEmpty()) {
            throw new IllegalStateException("Cannot remove assignments from empty set");
        }

        if (!this.assignments.remove(assignment)) {
            throw new IllegalStateException("Cannot remove assignment that does not exist in set");
        }

    }

    public void removeAssignmentForShapeTree(URL shapeTreeUrl) {
        removeAssignment(getAssignmentForShapeTree(shapeTreeUrl));
    }
}
