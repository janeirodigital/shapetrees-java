// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.core
import { HttpHeaders } from './enums/HttpHeaders';
import { ShapeTreeResourceType } from './enums/ShapeTreeResourceType';
import { ShapeTreeException } from './exceptions/ShapeTreeException';
import { GraphHelper } from './helpers/GraphHelper';
import * as Graph from 'org/apache/jena/graph';
import * as URI from 'java/net';
import { urlToUri } from './helpers/GraphHelper/urlToUri';
import { ResourceAttributes } from './ResourceAttributes';

/**
 * InstanceResource is a base class which may represent either a ManageableResource
 * (a resource that can be managed by a shape tree), or a ManagerResource (a resource
 * which assigns shape trees to a ManageableResource). This class is only meant to be
 * extended by ManageableResource and ManagerResource, or used to indicate either
 * of the two.
 */
export class InstanceResource {

   private readonly url: URL;

   private readonly resourceType: ShapeTreeResourceType;

   private readonly attributes: ResourceAttributes;

   private readonly body: string;

   private readonly name: string;

   private readonly exists: boolean;

  /**
   * Construct an InstanceResource by providing essential attributes. This constructor is meant
   * to be called by sub-class constructors.
   * @param url URL of the instance resource
   * @param resourceType Identified shape tree resource type
   * @param attributes Associated resource attributes
   * @param body Body of the resource
   * @param name Name of the resource
   * @param exists Whether the resource exists
   */
  constructor(url: URL, resourceType: ShapeTreeResourceType, attributes: ResourceAttributes, body: string, name: string, exists: boolean) {
    this.url = url;
    this.resourceType = resourceType;
    this.attributes = attributes;
    this.body = body;
    this.name = name;
    this.exists = exists;
  }

  /**
   * Get an RDF graph of the body of the InstanceResource. If <code>baseUrl</code> is not
   * provided, the URL of the InstanceResource will be used. An exception is thrown if
   * the body cannot be processed (e.g. if it isn't a valid RDF resource).
   * @param baseUrl Base URL to use for the graph
   * @return RDF graph of the InstanceResource body
   * @throws ShapeTreeException
   */
  public getGraph(baseUrl: URL): Graph /* throws ShapeTreeException */ {
    if (!this.isExists()) {
      return null;
    }
    if (baseUrl === null) {
      baseUrl = this.url;
    }
    const baseUri: URI = urlToUri(baseUrl);
    return GraphHelper.readStringIntoGraph(baseUri, this.getBody(), this.getAttributes().firstValue(HttpHeaders.CONTENT_TYPE.getValue()).orElse(null));
  }

  public getUrl(): URL {
    return this.url;
  }

  public getResourceType(): ShapeTreeResourceType {
    return this.resourceType;
  }

  public getAttributes(): ResourceAttributes {
    return this.attributes;
  }

  public getBody(): string {
    return this.body;
  }

  public getName(): string {
    return this.name;
  }

  public getExists(): boolean {
    return this.exists;
  }
}
