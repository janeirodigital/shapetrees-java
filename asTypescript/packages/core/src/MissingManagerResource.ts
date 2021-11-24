// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.core
import { ShapeTreeResourceType } from './enums/ShapeTreeResourceType';
import { MissingManageableResource } from './MissingManageableResource';
import { ManagerResource } from './ManagerResource';
import { ResourceAttributes } from './ResourceAttributes';

/**
 * A MissingManagerResource represents a state where a given
 * ManagerResource at a URL does not exist.
 */
export class MissingManagerResource extends ManagerResource {

  /**
   * Construct a missing manager resource based on a MissingManageableResource
   * @param manageable Missing manageable resource
   * @param managedUrl Corresponding URL of the resource that would be managed
   */
  public constructor(manageable: MissingManageableResource, managedUrl: URL) {
    super(manageable.getUrl(), manageable.getResourceType(), manageable.getAttributes(), manageable.getBody(), manageable.getName(), manageable.isExists(), managedUrl);
  }

  /**
   * Construct a missing manager resource.
   * @param url URL of the resource
   * @param resourceType Identified shape tree resource type
   * @param attributes Associated resource attributes
   * @param body Body of the resource
   * @param name Name of the resource
   * @param managedResourceUrl URL of the resource that would be managed
   */
  public constructor(url: URL, resourceType: ShapeTreeResourceType, attributes: ResourceAttributes, body: string, name: string, managedResourceUrl: URL) {
    super(url, resourceType, attributes, body, name, false, managedResourceUrl);
  }
}
