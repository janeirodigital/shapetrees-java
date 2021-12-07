// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.core
import { ShapeTreeException } from './exceptions/ShapeTreeException';
import { RdfVocabulary } from './vocabularies/RdfVocabulary';
import { ShapeTreeVocabulary } from './vocabularies/ShapeTreeVocabulary';
import * as Graph from 'org/apache/jena/graph';
import * as Node from 'org/apache/jena/graph';
import * as NodeFactory from 'org/apache/jena/graph';
import * as Triple from 'org/apache/jena/graph';
import * as MalformedURLException from 'java/net';
import * as Objects from 'java/util';

/**
 * ShapeTreeAssignment
 *
 * Shape Trees, §3:  Each shape tree assignment identifies a shape tree associated with the managed resource,
 * the focus node for shape validation, and the information needed to navigate the physical hierarchy in
 * which that managed resource resides.
 * https://shapetrees.org/TR/specification/#manager
 */
@EqualsAndHashCode
export class ShapeTreeAssignment {

  // Identifies the shape tree to be associated with the managed resource
   private readonly shapeTree: URL;

  // Identifies the resource managed by the shape tree assignment
   private readonly managedResource: URL;

  // Identifies the root shape tree assignment
   private readonly rootAssignment: URL;

  // Identifies the focus node for shape validation in the managed resource
   private readonly focusNode: URL;

  // Identifies the shape to which focusNode must conform
   private readonly shape: URL;

   private readonly url: URL;

  public constructor(shapeTree: URL, managedResource: URL, rootAssignment: URL, focusNode: URL, shape: URL, url: URL) /* throws ShapeTreeException */ {
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
    } catch (ex) {
 if (ex instanceof NullPointerException || ex instanceof IllegalStateException) {
       throw new ShapeTreeException(500, "Failed to initialize shape tree assignment: " + ex.getMessage());
     }
  }

  public static getFromGraph(url: URL, managerGraph: Graph): ShapeTreeAssignment /* throws MalformedURLException, ShapeTreeException */ {
    let shapeTree: URL = null;
    let managedResource: URL = null;
    let rootAssignment: URL = null;
    let focusNode: URL = null;
    let shape: URL = null;
    // Look up the ShapeTreeAssignment in the ManagerResource Graph via its URL
    let assignmentTriples: Array<Triple> = managerGraph.find(NodeFactory.createURI(url.toString()), Node.ANY, Node.ANY).toList();
    // A valid assignment must have at least a shape tree, managed resource, and root assignment urls
    if (assignmentTriples.size() < 3) {
      throw new IllegalStateException("Incomplete shape tree assignment, Only " + assignmentTriples.size() + " attributes found");
    }
    // Lookup and assign each triple in the nested ShapeTreeAssignment
    for (const assignmentTriple of assignmentTriples) {
      switch(assignmentTriple.getPredicate().getURI()) {
        case RdfVocabulary.TYPE:
          if (!assignmentTriple.getObject().isURI() || !assignmentTriple.getObject().getURI() === ShapeTreeVocabulary.SHAPETREE_ASSIGNMENT) {
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
    return new ShapeTreeAssignment(shapeTree, managedResource, rootAssignment, focusNode, shape, url);
  }

  public isRootAssignment(): boolean {
    return this.getUrl() === this.getRootAssignment();
  }

  public getShapeTree(): URL {
    return this.shapeTree;
  }

  public getManagedResource(): URL {
    return this.managedResource;
  }

  public getRootAssignment(): URL {
    return this.rootAssignment;
  }

  public getFocusNode(): URL {
    return this.focusNode;
  }

  public getShape(): URL {
    return this.shape;
  }

  public getUrl(): URL {
    return this.url;
  }
}
