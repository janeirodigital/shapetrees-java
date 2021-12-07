// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.core
import { ShapeTreeException } from './exceptions/ShapeTreeException';
import { GraphHelper } from './helpers/GraphHelper';
import { RdfVocabulary } from './vocabularies/RdfVocabulary';
import { ShapeTreeVocabulary } from './vocabularies/ShapeTreeVocabulary';
import * as RandomStringUtils from 'org/apache/commons/lang3';
import * as Graph from 'org/apache/jena/graph';
import * as Node from 'org/apache/jena/graph';
import * as NodeFactory from 'org/apache/jena/graph';
import * as Triple from 'org/apache/jena/graph';
import * as RDF from 'org/apache/jena/vocabulary';
import * as MalformedURLException from 'java/net';
import * as URI from 'java/net';
import { urlToUri } from './helpers/GraphHelper/urlToUri';
import { ShapeTreeAssignment } from './ShapeTreeAssignment';
import { ShapeTree } from './ShapeTree';

/**
 * ShapeTreeManager
 *
 * Shape Trees, §3:  A shape tree manager associates a managed resource with one or more shape trees. No more
 * than one shape tree manager may be associated with a managed resource. A shape tree manager includes
 * one or more shape tree assignments via st:hasAssignment.
 * https://shapetrees.org/TR/specification/#manager
 */
export class ShapeTreeManager {

   private readonly id: URL;

  // Each ShapeTreeManager has one or more ShapeTreeAssignments
   private readonly assignments: Array<ShapeTreeAssignment> = new Array<>();

  /**
   * Constructor for a new ShapeTreeManager
   * @param id URL of the ShapeTreeManager resource
   */
  public constructor(id: URL) {
    this.id = id;
  }

  /**
   * Get the URL (identifier) of the ShapeTreeManager
   * @return URL identifier of the ShapeTreeManager
   */
  protected getUrl(): URL {
    return this.id;
  }

  /**
   * Get the ShapeTreeManager as an RDF Graph
   * @return Graph of the ShapeTreeManager
   * @throws ShapeTreeException
   */
  public getGraph(): Graph /* throws ShapeTreeException */ {
    let managerGraph: Graph = GraphHelper.getEmptyGraph();
    let managerSubject: string = this.getUrl().toString();
    // <> a st:Manager
    managerGraph.add(GraphHelper.newTriple(managerSubject, RDF.type.toString(), GraphHelper.knownUrl(ShapeTreeVocabulary.SHAPETREE_MANAGER)));
    // For each assignment create a blank node and populate
    for (const assignment of this.assignments) {
      // <> st:hasAssignment <assignment1>, <assignment2>
      managerGraph.add(GraphHelper.newTriple(managerSubject, ShapeTreeVocabulary.HAS_ASSIGNMENT, assignment.getUrl()));
      const subject: URI = urlToUri(assignment.getUrl());
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
  public addAssignment(assignment: ShapeTreeAssignment): void /* throws ShapeTreeException */ {
    if (assignment === null) {
      throw new ShapeTreeException(500, "Must provide a non-null assignment to an initialized List of assignments");
    }
    if (!this.assignments.isEmpty()) {
      for (const existingAssignment of this.assignments) {
        if (existingAssignment === assignment) {
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
  public mintAssignmentUrl(): URL {
    let fragment: string = RandomStringUtils.random(8, true, true);
    let assignmentString: string = this.getUrl().toString() + "#" + fragment;
    const assignmentUrl: URL;
    try {
      assignmentUrl = new URL(assignmentString);
    } catch (ex) {
 if (ex instanceof MalformedURLException) {
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
  public mintAssignmentUrl(proposedAssignmentUrl: URL): URL {
    for (const assignment of this.assignments) {
      if (assignment.getUrl() === proposedAssignmentUrl) {
        // If we somehow managed to randomly generate a location URL that already exists, generate another
        return mintAssignmentUrl();
      }
    }
    return proposedAssignmentUrl;
  }

  public getContainingAssignments(): Array<ShapeTreeAssignment> /* throws ShapeTreeException */ {
    let containingAssignments: Array<ShapeTreeAssignment> = new Array<>();
    for (const assignment of this.assignments) {
      let shapeTree: ShapeTree = ShapeTreeFactory.getShapeTree(assignment.getShapeTree());
      if (!shapeTree.getContains().isEmpty()) {
        containingAssignments.add(assignment);
      }
    }
    return containingAssignments;
  }

  public static getFromGraph(id: URL, managerGraph: Graph): ShapeTreeManager /* throws ShapeTreeException */ {
    let manager: ShapeTreeManager = new ShapeTreeManager(id);
    // Look up the ShapeTreeManager in the ManagerResource Graph via (any subject node, rdf:type, st:ShapeTreeManager)
    let managerTriples: Array<Triple> = managerGraph.find(Node.ANY, NodeFactory.createURI(RdfVocabulary.TYPE), NodeFactory.createURI(ShapeTreeVocabulary.SHAPETREE_MANAGER)).toList();
    // Shape Trees, §3: No more than one shape tree manager may be associated with a managed resource.
    // https://shapetrees.org/TR/specification/#manager
    if (managerTriples.size() > 1) {
      throw new IllegalStateException("Multiple ShapeTreeManager instances found: " + managerTriples.size());
    } else if (managerTriples.isEmpty()) {
      // Given the fact that a manager resource exists, there should never be a case where the manager resource
      // exists but no manager is found inside of it.
      throw new IllegalStateException("No ShapeTreeManager instances found: " + managerTriples.size());
    }
    // Get the URL of the ShapeTreeManager subject node
    let managerUrl: string = managerTriples.get(0).getSubject().getURI();
    // Look up ShapeTreeAssignment nodes (manager subject node, st:hasAssignment, any st:hasAssignment nodes).
    // There should be one result per nested ShapeTreeAssignment, each identified by a unique url.
    // Shape Trees, §3: A shape tree manager includes one or more shape tree assignments via st:hasAssignment
    // https://shapetrees.org/TR/specification/#manager
    const s: Node = NodeFactory.createURI(managerUrl);
    const stAssignment: Node = NodeFactory.createURI(ShapeTreeVocabulary.HAS_ASSIGNMENT);
    let assignmentNodes: Array<Triple> = managerGraph.find(s, stAssignment, Node.ANY).toList();
    // For each st:hasAssignment node, extract a new ShapeTreeAssignment
    for (const assignmentNode of assignmentNodes) {
      let assignment: ShapeTreeAssignment = null;
      try {
        assignment = ShapeTreeAssignment.getFromGraph(new URL(assignmentNode.getObject().getURI()), managerGraph);
      } catch (ex) {
 if (ex instanceof MalformedURLException) {
         throw new ShapeTreeException(500, "Object of { " + s + " " + stAssignment + " " + assignmentNode.getObject() + " } must be a URL.");
       }
      manager.assignments.add(assignment);
    }
    return manager;
  }

  public getAssignmentForShapeTree(shapeTreeUrl: URL): ShapeTreeAssignment {
    if (this.assignments.isEmpty()) {
      return null;
    }
    for (const assignment of this.assignments) {
      if (assignment.getShapeTree() === shapeTreeUrl) {
        return assignment;
      }
    }
    return null;
  }

  // Given a root assignment, lookup the corresponding assignment in a shape tree manager that has the same root assignment
  public getAssignmentForRoot(rootAssignment: ShapeTreeAssignment): ShapeTreeAssignment {
    if (this.getAssignments() === null || this.getAssignments().isEmpty()) {
      return null;
    }
    for (const assignment of this.getAssignments()) {
      if (rootAssignment.getUrl() === assignment.getRootAssignment()) {
        return assignment;
      }
    }
    return null;
  }

  public removeAssignment(assignment: ShapeTreeAssignment): void {
    if (assignment === null) {
      throw new IllegalStateException("Cannot remove a null assignment");
    }
    if (this.assignments.isEmpty()) {
      throw new IllegalStateException("Cannot remove assignments from empty set");
    }
    if (!this.assignments.remove(assignment)) {
      throw new IllegalStateException("Cannot remove assignment that does not exist in set");
    }
  }

  public removeAssignmentForShapeTree(shapeTreeUrl: URL): void {
    removeAssignment(getAssignmentForShapeTree(shapeTreeUrl));
  }

  public getId(): URL {
    return this.id;
  }

  public getAssignments(): Array<ShapeTreeAssignment> {
    return this.assignments;
  }
}
