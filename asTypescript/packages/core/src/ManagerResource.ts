// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.core
import { ShapeTreeResourceType } from './enums/ShapeTreeResourceType';
import { ShapeTreeException } from './exceptions/ShapeTreeException';
import * as Graph from 'org/apache/jena/graph';
import { InstanceResource } from './InstanceResource';
import { ShapeTreeManager } from './ShapeTreeManager';
import { ResourceAttributes } from './ResourceAttributes';

/**
 * A ManagerResource represents a resource that is associated with
 * a regular MangeableResource, and contains metadata in the form
 * of a ShapeTreeManager that assigns one or more shape trees to the
 * associated ManageableResource. When it exists, the associated
 * resource is considered to be managed. When it doesn't, the associated
 * resource is considered to be unmanaged.
 */
export class ManagerResource extends InstanceResource {

   private readonly managedResourceUrl: URL;

  /**
   * Construct a manager resource
   * @param url URL of the resource
   * @param resourceType Identified shape tree resource type
   * @param attributes Associated resource attributes
   * @param body Body of the resource
   * @param name Name of the resource
   * @param exists Whether the resource exists
   * @param managedResourceUrl URL of the associated managed resource
   */
  public constructor(url: URL, resourceType: ShapeTreeResourceType, attributes: ResourceAttributes, body: string, name: string, exists: boolean, managedResourceUrl: URL) {
    super(url, resourceType, attributes, body, name, exists);
    this.managedResourceUrl = managedResourceUrl;
  }

  /**
   * Get a ShapeTreeManager from the body of the ManagerResource
   * @return Shape tree manager
   * @throws ShapeTreeException
   */
  public getManager(): ShapeTreeManager /* throws ShapeTreeException */ {
    if (!this.isExists()) {
      return null;
    }
    let managerGraph: Graph = this.getGraph(this.getUrl());
    if (managerGraph === null) {
      return null;
    }
    return ShapeTreeManager.getFromGraph(this.getUrl(), managerGraph);
  }

  public getManagedResourceUrl(): URL {
    return this.managedResourceUrl;
  }
}
