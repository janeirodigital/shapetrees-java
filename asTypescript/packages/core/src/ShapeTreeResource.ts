// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.core
import { DocumentLoaderManager } from './contentloaders/DocumentLoaderManager';
import { ShapeTreeException } from './exceptions/ShapeTreeException';
import { GraphHelper } from './helpers/GraphHelper';
import * as Model from 'org/apache/jena/rdf/model';
import * as URI from 'java/net';
import { removeUrlFragment } from './helpers/GraphHelper/removeUrlFragment';
import { urlToUri } from './helpers/GraphHelper/urlToUri';
import { DocumentResponse } from './DocumentResponse';

/**
 * Represents a resource that contains one or more shape tree definitions. Provides
 * a factory to lookup, initialize, and cache them.
 */
export class ShapeTreeResource {

   readonly url: URL;

   readonly body: string;

   readonly contentType: string;

   readonly model: Model;

  @Getter
   private static readonly localResourceCache: Map<URI, ShapeTreeResource> = new Map<>();

  /**
   * Looks up and caches the shape tree resource at <code>resourceUrl</code>. Will used cached
   * verion if it exists. Throws exceptions if the resource doesn't exist, or isn't a valid
   * RDF document.
   * @param resourceUrl URL of shape tree resource
   * @return Shape tree resource at provided <code>resourceUrl</code>
   * @throws ShapeTreeException
   */
  public static getShapeTreeResource(resourceUrl: URL): ShapeTreeResource /* throws ShapeTreeException */ {
    resourceUrl = removeUrlFragment(resourceUrl);
    if (localResourceCache.containsKey(urlToUri(resourceUrl))) {
      log.debug("[{}] previously cached -- returning", resourceUrl);
      return localResourceCache.get(urlToUri(resourceUrl));
    }
    let externalDocument: DocumentResponse = DocumentLoaderManager.getLoader().loadExternalDocument(resourceUrl);
    if (!externalDocument.isExists()) {
      throw new ShapeTreeException(500, "Cannot load shape shape tree resource at " + resourceUrl);
    }
    let model: Model = GraphHelper.readStringIntoModel(urlToUri(resourceUrl), externalDocument.getBody(), externalDocument.getContentType().orElse("text/turtle"));
    let resource: ShapeTreeResource = new ShapeTreeResource(resourceUrl, externalDocument.getBody(), externalDocument.getContentType().orElse("text/turtle"), model);
    localResourceCache.put(urlToUri(resourceUrl), resource);
    return resource;
  }

  /**
   * Clears the local shape tree resource cache
   */
  public static clearCache(): void {
    localResourceCache.clear();
  }

  public constructor(url: URL, body: string, contentType: string, model: Model, localResourceCache: Map<URI, ShapeTreeResource>) {
    this.url = url;
    this.body = body;
    this.contentType = contentType;
    this.model = model;
    this.localResourceCache = localResourceCache;
  }

  public getUrl(): URL {
    return this.url;
  }

  public getBody(): string {
    return this.body;
  }

  public getContentType(): string {
    return this.contentType;
  }

  public getModel(): Model {
    return this.model;
  }

  public getLocalResourceCache(): Map<URI, ShapeTreeResource> {
    return this.localResourceCache;
  }
}
