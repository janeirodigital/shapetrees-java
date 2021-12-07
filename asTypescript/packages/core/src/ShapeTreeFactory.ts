// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.core
import { ShapeTreeException } from './exceptions/ShapeTreeException';
import { ShapeTreeVocabulary } from './vocabularies/ShapeTreeVocabulary';
import * as Node from 'org/apache/jena/graph';
import * as Node_URI from 'org/apache/jena/graph';
import * as Model from 'org/apache/jena/rdf/model';
import * as Resource from 'org/apache/jena/rdf/model';
import * as Statement from 'org/apache/jena/rdf/model';
import * as Property from 'org/apache/jena/rdf/model';
import * as RDFNode from 'org/apache/jena/rdf/model';
import * as MalformedURLException from 'java/net';
import * as URI from 'java/net';
import { urlToUri } from './helpers/GraphHelper/urlToUri';
import { ShapeTreeReference } from './ShapeTreeReference';
import { ShapeTree } from './ShapeTree';
import { ShapeTreeResource } from './ShapeTreeResource';

/**
 * Provides a factory to look up and initialize ShapeTrees.
 * Includes a simple in-memory local cache to avoid repeated fetching of
 * remote shape tree resources.
 */
export class ShapeTreeFactory {

  private constructor() {
  }

   private static readonly RDFS_LABEL: string = "http://www.w3.org/2000/01/rdf-schema#label";

  @Getter
   private static readonly localShapeTreeCache: Map<URI, ShapeTree> = new Map<>();

  /**
   * Looks up and parses the shape tree at <code>shapeTreeUrl</code>.
   * Shape trees linked via st:contains and st:references are parsed
   * recursively. Maintains a cache to avoid parsing the same shape tree
   * more than once.
   * @param shapeTreeUrl URL of the shape tree to get
   * @return Parsed and initialized shape tree
   * @throws ShapeTreeException
   */
  public static getShapeTree(shapeTreeUrl: URL): ShapeTree /* throws ShapeTreeException */ {
    log.debug("Parsing shape tree: {}", shapeTreeUrl);
    if (localShapeTreeCache.containsKey(urlToUri(shapeTreeUrl))) {
      log.debug("[{}] previously cached -- returning", shapeTreeUrl.toString());
      return localShapeTreeCache.get(urlToUri(shapeTreeUrl));
    }
    // Load the entire shape tree resource (which may contain multiple shape trees)
    let shapeTreeResource: ShapeTreeResource = ShapeTreeResource.getShapeTreeResource(shapeTreeUrl);
    let resourceModel: Model = shapeTreeResource.getModel();
    let shapeTreeNode: Resource = resourceModel.getResource(shapeTreeUrl.toString());
    // Load and set the expected resource type
    const expectsType: URL = getUrlValue(resourceModel, shapeTreeNode, ShapeTreeVocabulary.EXPECTS_TYPE, shapeTreeUrl);
    if (expectsType === null)
      throw new ShapeTreeException(500, "Shape Tree :expectsType not found");
    // Load and set the Shape URL
    const shape: URL = getUrlValue(resourceModel, shapeTreeNode, ShapeTreeVocabulary.SHAPE, shapeTreeUrl);
    // Load and set Label
    const label: string = getStringValue(resourceModel, shapeTreeNode, RDFS_LABEL);
    // Load and set contains list
    const contains: Array<URL> = getContains(resourceModel, shapeTreeNode, shapeTreeUrl);
    // Load and set references list
    const references: Array<ShapeTreeReference> = getReferences(resourceModel, shapeTreeNode, shapeTreeUrl);
    if (!contains.isEmpty() && !expectsType.toString() === ShapeTreeVocabulary.CONTAINER) {
      throw new ShapeTreeException(400, "Only a container can be expected to have st:contains");
    }
    let shapeTree: ShapeTree = new ShapeTree(shapeTreeUrl, expectsType, label, shape, references, contains);
    localShapeTreeCache.put(urlToUri(shapeTreeUrl), shapeTree);
    // Recursively parse contained shape trees
    for (const containedUrl of contains) {
      getShapeTree(containedUrl);
    }
    // Recursively parse referenced shape trees
    for (const reference of references) {
      getShapeTree(reference.getReferenceUrl());
    }
    return shapeTree;
  }

  /**
   * Get the list of URLs linked via st:contains by the shape tree being parsed.
   * @param resourceModel RDF Model representing the shape tree resource
   * @param shapeTreeNode RDF Node of the shape tree
   * @param shapeTreeUrl URL of the shape tree
   * @return List of URLs linked via st:contains
   * @throws ShapeTreeException
   */
  private static getContains(resourceModel: Model, shapeTreeNode: Resource, shapeTreeUrl: URL): Array<URL> /* throws ShapeTreeException */ {
    try {
      return getURLListValue(resourceModel, shapeTreeNode, ShapeTreeVocabulary.CONTAINS);
    } catch (ex) {
 if (ex instanceof MalformedURLException || ex instanceof ShapeTreeException) {
       throw new ShapeTreeException(500, "List <" + shapeTreeUrl + "> contains malformed URL: " + ex.getMessage());
     }
  }

  /**
   * Get the list of ShapeTreeReferences linked via st:references by the shape tree being parsed.
   * @param resourceModel RDF Model representing the shape tree resource
   * @param shapeTreeNode RDF Node of the shape tree
   * @param shapeTreeUrl URL of the shape tree
   * @return List of ShapeTreeReferences linked via st:references
   * @throws ShapeTreeException
   */
  private static getReferences(resourceModel: Model, shapeTreeNode: Resource, shapeTreeUrl: URL): Array<ShapeTreeReference> /* throws ShapeTreeException */ {
    let references: Array<ShapeTreeReference> = new Array<>();
    let referencesProperty: Property = resourceModel.createProperty(ShapeTreeVocabulary.REFERENCES);
    if (shapeTreeNode.hasProperty(referencesProperty)) {
      let referenceStatements: Array<Statement> = shapeTreeNode.listProperties(referencesProperty).toList();
      for (const referenceStatement of referenceStatements) {
        let referenceResource: Resource = referenceStatement.getObject().asResource();
        const referencedShapeTreeUrlString: string = getStringValue(resourceModel, referenceResource, ShapeTreeVocabulary.REFERENCES_SHAPE_TREE);
        const referencedShapeTreeUrl: URL;
        let referencedShapeTree: ShapeTreeReference;
        try {
          referencedShapeTreeUrl = new URL(referencedShapeTreeUrlString);
        } catch (ex) {
 if (ex instanceof MalformedURLException) {
           throw new ShapeTreeException(500, "ShapeTree <" + shapeTreeUrl + "> references malformed URL <" + referencedShapeTreeUrlString + ">: " + ex.getMessage());
         }
        let viaShapePath: string = getStringValue(resourceModel, referenceResource, ShapeTreeVocabulary.VIA_SHAPE_PATH);
        let viaPredicate: URL = getUrlValue(resourceModel, referenceResource, ShapeTreeVocabulary.VIA_PREDICATE, shapeTreeUrl);
        referencedShapeTree = new ShapeTreeReference(referencedShapeTreeUrl, viaShapePath, viaPredicate);
        references.add(referencedShapeTree);
      }
    }
    return references;
  }

  /**
   * Validate and get a single URL value linked to a shape tree by the supplied <code>predicate</code>.
   * @param model RDF Model representing the shape tree resource
   * @param resource RDF Node of the shape tree
   * @param predicate Predicate to match
   * @param shapeTreeUrl URL of the shape tree
   * @return URL value linked via <code>predicate</code>
   * @throws ShapeTreeException
   */
  private static getUrlValue(model: Model, resource: Resource, predicate: string, shapeTreeUrl: URL): URL /* throws ShapeTreeException */ {
    let property: Property = model.createProperty(predicate);
    if (resource.hasProperty(property)) {
      let statement: Statement = resource.getProperty(property);
      const object: RDFNode = statement.getObject();
      if (object.isURIResource()) {
        try {
          return new URL(object.asResource().getURI());
        } catch (ex) {
 if (ex instanceof MalformedURLException) {
           throw new IllegalStateException("Malformed ShapeTree <" + shapeTreeUrl + ">: Jena URIResource <" + object + "> didn't parse as URL - " + ex.getMessage());
         }
      } else {
        throw new ShapeTreeException(500, "Malformed ShapeTree <" + shapeTreeUrl + ">: expected " + object + " to be a URL");
      }
    }
    return null;
  }

  /**
   * Validate and get a single String value linked to a shape tree by the supplied <code>predicate</code>.
   * @param model RDF Model representing the shape tree resource
   * @param resource RDF Node of the shape tree
   * @param predicate Predicate to match
   * @return String value linked via <code>predicate</code>
   * @throws ShapeTreeException
   */
  private static getStringValue(model: Model, resource: Resource, predicate: string): string /* throws ShapeTreeException */ {
    let property: Property = model.createProperty(predicate);
    if (resource.hasProperty(property)) {
      let statement: Statement = resource.getProperty(property);
      if (statement.getObject().isLiteral()) {
        return statement.getObject().asLiteral().getString();
      } else if (statement.getObject().isURIResource()) {
        return statement.getObject().asResource().getURI();
      } else {
        throw new ShapeTreeException(500, "Cannot determine object type when converting from string for: " + predicate);
      }
    }
    return null;
  }

  /**
   * Validate and get list of URLs linked to a shape tree by the supplied <code>predicate</code>.
   * @param model RDF Model representing the shape tree resource
   * @param resource RDF Node of the shape tree
   * @param predicate Predicate to match
   * @return List of URLs linked via <code>predicate</code>
   * @throws MalformedURLException
   * @throws ShapeTreeException
   */
  private static getURLListValue(model: Model, resource: Resource, predicate: string): Array<URL> /* throws MalformedURLException, ShapeTreeException */ {
    let urls: Array<URL> = new Array<>();
    let property: Property = model.createProperty(predicate);
    if (resource.hasProperty(property)) {
      let propertyStatements: Array<Statement> = resource.listProperties(property).toList();
      for (const propertyStatement of propertyStatements) {
        let propertyNode: Node = propertyStatement.getObject().asNode();
        if (propertyNode instanceof Node_URI) {
          let contentUrl: URL = new URL(propertyNode.getURI());
          urls.add(contentUrl);
        } else {
          throw new ShapeTreeException(500, "Must provide a valid URI in URI listing");
        }
      }
    }
    return urls;
  }

  /**
   * Clears the local shape tree cache
   */
  public static clearCache(): void {
    localShapeTreeCache.clear();
  }
}
