// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.core
import { ShapeTreeContainsPriority } from './comparators/ShapeTreeContainsPriority';
import { DocumentLoaderManager } from './contentloaders/DocumentLoaderManager';
import { HttpHeaders } from './enums/HttpHeaders';
import { RecursionMethods } from './enums/RecursionMethods';
import { ShapeTreeResourceType } from './enums/ShapeTreeResourceType';
import { ShapeTreeException } from './exceptions/ShapeTreeException';
import { GraphHelper } from './helpers/GraphHelper';
import * as GlobalFactory from 'fr/inria/lille/shexjava';
import * as Label from 'fr/inria/lille/shexjava/schema';
import * as ShexSchema from 'fr/inria/lille/shexjava/schema';
import * as ShExCParser from 'fr/inria/lille/shexjava/schema/parsing';
import * as RecursiveValidation from 'fr/inria/lille/shexjava/validation';
import * as ValidationAlgorithm from 'fr/inria/lille/shexjava/validation';
import * as IRI from 'org/apache/commons/rdf/api';
import * as JenaRDF from 'org/apache/commons/rdf/jena';
import * as Graph from 'org/apache/jena/graph';
import * as GraphUtil from 'org/apache/jena/graph';
import * as Node from 'org/apache/jena/graph';
import * as NotNull from 'org/jetbrains/annotations';
import * as MalformedURLException from 'java/net';
import * as StandardCharsets from 'java/nio/charset';
import * as Iterator from 'java/util';
import * as Collections from 'java/util';
import * as Queue from 'java/util';
import * as LinkedList from 'java/util';
import { urlToUri } from './helpers/GraphHelper/urlToUri';
import { ShapeTreeReference } from './ShapeTreeReference';
import { DocumentResponse } from './DocumentResponse';
import { ManageableResource } from './ManageableResource';
import { ValidationResult } from './ValidationResult';

export class ShapeTree {

  @NotNull
   private readonly id: URL;

  @NotNull
   private readonly expectedResourceType: URL;

   private readonly shape: URL;

   private readonly label: string;

  @NotNull
   private readonly contains: Array<URL>;

  @NotNull
   private readonly references: Array<ShapeTreeReference>;

  public constructor(@NotNull id: URL, @NotNull expectedResourceType: URL, label: string, shape: URL, @NotNull references: Array<ShapeTreeReference>, @NotNull contains: Array<URL>) {
    this.id = id;
    this.expectedResourceType = expectedResourceType;
    this.label = label;
    this.shape = shape;
    this.references = references;
    this.contains = contains;
  }

  public validateResource(targetResource: ManageableResource): ValidationResult /* throws ShapeTreeException */ {
    return validateResource(targetResource, null);
  }

  public validateResource(targetResource: ManageableResource, focusNodeUrls: Array<URL>): ValidationResult /* throws ShapeTreeException */ {
    let bodyGraph: Graph = null;
    if (targetResource.getResourceType() != ShapeTreeResourceType.NON_RDF) {
      bodyGraph = GraphHelper.readStringIntoGraph(urlToUri(targetResource.getUrl()), targetResource.getBody(), targetResource.getAttributes().firstValue(HttpHeaders.CONTENT_TYPE.getValue()).orElse(null));
    }
    return validateResource(targetResource.getName(), targetResource.getResourceType(), bodyGraph, focusNodeUrls);
  }

  public validateResource(requestedName: string, resourceType: ShapeTreeResourceType, bodyGraph: Graph, focusNodeUrls: Array<URL>): ValidationResult /* throws ShapeTreeException */ {
    // Check whether the proposed resource is the same type as what is expected by the shape tree
    if (!this.expectedResourceType.toString() === resourceType.getValue()) {
      return new ValidationResult(false, this, "Resource type " + resourceType + " is invalid. Expected " + this.expectedResourceType);
    }
    // If a label is specified, check if the proposed name is the same
    if (this.label != null && !this.label === requestedName) {
      return new ValidationResult(false, this, "Proposed resource name " + requestedName + " is invalid. Expected " + this.label);
    }
    // If the shape tree specifies a shape to validate, perform shape validation
    if (this.shape != null) {
      if (focusNodeUrls === null) {
        focusNodeUrls = Collections.emptyList();
      }
      return this.validateGraph(bodyGraph, focusNodeUrls);
    }
    // Allow if we fall through to here. Focus node is set to null because we only get here if no shape validation was performed
    return new ValidationResult(true, this, this, null);
  }

  public validateGraph(graph: Graph, focusNodeUrls: Array<URL>): ValidationResult /* throws ShapeTreeException */ {
    // if (true) return new ValidationResult(true, this, this, focusNodeUrl); // [debug] ShExC parser brings debugger to its knees
    if (this.shape === null) {
      throw new ShapeTreeException(400, "Attempting to validate a shape for ShapeTree " + this.id + "but it doesn't specify one");
    }
    let schema: ShexSchema;
    if (SchemaCache.isInitialized() && SchemaCache.containsSchema(this.shape)) {
      log.debug("Found cached schema {}", this.shape);
      schema = SchemaCache.getSchema(this.shape);
    } else {
      log.debug("Did not find schema in cache {} will retrieve and parse", this.shape);
      let shexShapeContents: DocumentResponse = DocumentLoaderManager.getLoader().loadExternalDocument(this.shape);
      if (shexShapeContents === null || shexShapeContents.getBody() === null || shexShapeContents.getBody().isEmpty()) {
        throw new ShapeTreeException(400, "Attempting to validate a ShapeTree (" + this.id + ") - Shape at (" + this.shape + ") is not found or is empty");
      }
      let shapeBody: string = shexShapeContents.getBody();
      let stream: InputStream = new ByteArrayInputStream(shapeBody.getBytes(StandardCharsets.UTF_8));
      let shexCParser: ShExCParser = new ShExCParser();
      try {
        schema = new ShexSchema(GlobalFactory.RDFFactory, shexCParser.getRules(stream), shexCParser.getStart());
        if (SchemaCache.isInitialized()) {
          SchemaCache.putSchema(this.shape, schema);
        }
      } catch (ex) {
 if (ex instanceof Exception) {
         throw new ShapeTreeException(500, "Error parsing ShEx schema - " + ex.getMessage());
       }
    }
    // Tell ShExJava we want to use Jena as our graph library
    let jenaRDF: JenaRDF = new org.apache.commons.rdf.jena.JenaRDF();
    GlobalFactory.RDFFactory = jenaRDF;
    let validation: ValidationAlgorithm = new RecursiveValidation(schema, jenaRDF.asGraph(graph));
    let shapeLabel: Label = new Label(GlobalFactory.RDFFactory.createIRI(this.shape.toString()));
    if (!focusNodeUrls.isEmpty()) {
      // One or more focus nodes were provided for validation
      for (const focusNodeUrl of focusNodeUrls) {
        // Evaluate each provided focus node
        let focusNode: IRI = GlobalFactory.RDFFactory.createIRI(focusNodeUrl.toString());
        log.debug("Validating Shape Label = {}, Focus Node = {}", shapeLabel.toPrettyString(), focusNode.getIRIString());
        validation.validate(focusNode, shapeLabel);
        let valid: boolean = validation.getTyping().isConformant(focusNode, shapeLabel);
        if (valid) {
          return new ValidationResult(valid, this, this, focusNodeUrl);
        }
      }
      // None of the provided focus nodes were valid - this will return the last failure
      return new ValidationResult(false, this, "Failed to validate: " + shapeLabel.toPrettyString());
    } else {
      // No focus nodes were provided for validation, so all subject nodes will be evaluated
      let evaluateNodes: Array<Node> = GraphUtil.listSubjects(graph, Node.ANY, Node.ANY).toList();
      for (const evaluateNode of evaluateNodes) {
        const focusUriString: string = evaluateNode.getURI();
        let node: IRI = GlobalFactory.RDFFactory.createIRI(focusUriString);
        validation.validate(node, shapeLabel);
        let valid: boolean = validation.getTyping().isConformant(node, shapeLabel);
        if (valid) {
          const matchingFocusNode: URL;
          try {
            matchingFocusNode = new URL(focusUriString);
          } catch (ex) {
 if (ex instanceof MalformedURLException) {
             throw new ShapeTreeException(500, "Error reporting validation success on malformed URL <" + focusUriString + ">: " + ex.getMessage());
           }
          return new ValidationResult(valid, this, this, matchingFocusNode);
        }
      }
      return new ValidationResult(false, this, "Failed to validate: " + shapeLabel.toPrettyString());
    }
  }

  public validateContainedResource(containedResource: ManageableResource): ValidationResult /* throws ShapeTreeException */ {
    if (this.contains === null || this.contains.isEmpty()) {
      // TODO: say it can't be null?
      // The contained resource is permitted because this shape tree has no restrictions on what it contains
      return new ValidationResult(true, this, this, null);
    }
    return validateContainedResource(containedResource, Collections.emptyList(), Collections.emptyList());
  }

  public validateContainedResource(containedResource: ManageableResource, targetShapeTreeUrls: Array<URL>, focusNodeUrls: Array<URL>): ValidationResult /* throws ShapeTreeException */ {
    let containedResourceGraph: Graph = null;
    if (containedResource.getResourceType() != ShapeTreeResourceType.NON_RDF) {
      containedResourceGraph = GraphHelper.readStringIntoGraph(urlToUri(containedResource.getUrl()), containedResource.getBody(), containedResource.getAttributes().firstValue(HttpHeaders.CONTENT_TYPE.getValue()).orElse(null));
    }
    return validateContainedResource(containedResource.getName(), containedResource.getResourceType(), targetShapeTreeUrls, containedResourceGraph, focusNodeUrls);
  }

  public validateContainedResource(requestedName: string, resourceType: ShapeTreeResourceType, targetShapeTreeUrls: Array<URL>, bodyGraph: Graph, focusNodeUrls: Array<URL>): ValidationResult /* throws ShapeTreeException */ {
    if (this.contains === null || this.contains.isEmpty()) {
      // The contained resource is permitted because this shape tree has no restrictions on what it contains
      return new ValidationResult(true, this, this, null);
    }
    // If one or more target shape trees have been supplied
    if (!targetShapeTreeUrls.isEmpty()) {
      // Test each supplied target shape tree
      for (const targetShapeTreeUrl of targetShapeTreeUrls) {
        // Check if it exists in st:contains
        if (this.contains.contains(targetShapeTreeUrl)) {
          let targetShapeTree: ShapeTree = ShapeTreeFactory.getShapeTree(targetShapeTreeUrl);
          // Evaluate the shape tree against the attributes of the proposed resources
          let result: ValidationResult = targetShapeTree.validateResource(requestedName, resourceType, bodyGraph, focusNodeUrls);
          if (Boolean.TRUE === result.getValid()) {
            // Return a successful validation result, including the matching shape tree
            return new ValidationResult(true, this, targetShapeTree, result.getMatchingFocusNode());
          }
        }
      }
      // None of the provided target shape trees matched
      return new ValidationResult(false, null, "Failed to validate " + targetShapeTreeUrls);
    } else {
      // For each shape tree in st:contains
      for (const containsShapeTreeUrl of getPrioritizedContains()) {
        let containsShapeTree: ShapeTree = ShapeTreeFactory.getShapeTree(containsShapeTreeUrl);
        // Continue if the shape tree isn't gettable
        if (containsShapeTree === null) {
          continue;
        }
        // Evaluate the shape tree against the attributes of the proposed resources
        let result: ValidationResult = containsShapeTree.validateResource(requestedName, resourceType, bodyGraph, focusNodeUrls);
        // Continue if the proposed attributes were not a match
        if (Boolean.FALSE === result.getValid()) {
          continue;
        }
        // Return the successful validation result
        return new ValidationResult(true, this, containsShapeTree, result.getMatchingFocusNode());
      }
    }
    return new ValidationResult(false, null, "Failed to validate shape tree: " + this.id);
  }

  public getReferencedShapeTrees(): Iterator<ShapeTreeReference> /* throws ShapeTreeException */ {
    return getReferencedShapeTrees(RecursionMethods.DEPTH_FIRST);
  }

  public getReferencedShapeTrees(recursionMethods: RecursionMethods): Iterator<ShapeTreeReference> /* throws ShapeTreeException */ {
    return getReferencedShapeTreesList(recursionMethods).iterator();
  }

  // Return the list of shape tree contains by priority from most to least strict
  public getPrioritizedContains(): Array<URL> {
    let prioritized: Array<URL> = new Array<>(this.contains);
    Collections.sort(prioritized, new ShapeTreeContainsPriority());
    return prioritized;
  }

  private getReferencedShapeTreesList(recursionMethods: RecursionMethods): Array<ShapeTreeReference> /* throws ShapeTreeException */ {
    if (recursionMethods === RecursionMethods.BREADTH_FIRST) {
      return getReferencedShapeTreesListBreadthFirst();
    } else {
      let referencedShapeTrees: Array<ShapeTreeReference> = new Array<>();
      return getReferencedShapeTreesListDepthFirst(this.getReferences(), referencedShapeTrees);
    }
  }

  private getReferencedShapeTreesListBreadthFirst(): Array<ShapeTreeReference> /* throws ShapeTreeException */ {
    let referencedShapeTrees: Array<ShapeTreeReference> = new Array<>();
    let queue: Queue<ShapeTreeReference> = new LinkedList<>(this.getReferences());
    while (!queue.isEmpty()) {
      let currentShapeTree: ShapeTreeReference = queue.poll();
      referencedShapeTrees.add(currentShapeTree);
      let shapeTree: ShapeTree = ShapeTreeFactory.getShapeTree(currentShapeTree.getReferenceUrl());
      if (shapeTree != null) {
        let currentReferencedShapeTrees: Array<ShapeTreeReference> = shapeTree.getReferences();
        if (currentReferencedShapeTrees != null) {
          queue.addAll(currentReferencedShapeTrees);
        }
      }
    }
    return referencedShapeTrees;
  }

  private getReferencedShapeTreesListDepthFirst(currentReferencedShapeTrees: Array<ShapeTreeReference>, referencedShapeTrees: Array<ShapeTreeReference>): Array<ShapeTreeReference> /* throws ShapeTreeException */ {
    for (const currentShapeTreeReference of currentReferencedShapeTrees) {
      referencedShapeTrees.add(currentShapeTreeReference);
      let currentReferencedShapeTree: ShapeTree = ShapeTreeFactory.getShapeTree(currentShapeTreeReference.getReferenceUrl());
      if (currentReferencedShapeTree != null) {
        referencedShapeTrees = getReferencedShapeTreesListDepthFirst(currentReferencedShapeTree.getReferences(), referencedShapeTrees);
      }
    }
    return referencedShapeTrees;
  }

  public getId(): URL {
    return this.id;
  }

  public getExpectedResourceType(): URL {
    return this.expectedResourceType;
  }

  public getShape(): URL {
    return this.shape;
  }

  public getLabel(): string {
    return this.label;
  }

  public getContains(): Array<URL> {
    return this.contains;
  }

  public getReferences(): Array<ShapeTreeReference> {
    return this.references;
  }
}
