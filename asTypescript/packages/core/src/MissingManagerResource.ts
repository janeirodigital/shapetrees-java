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
   * @param managedResourceUrl Corresponding URL of the resource that would be managed
   * @param manageable Missing manageable resource
   */
  public constructor(managedResourceUrl: URL, manageable: MissingManageableResource) {
    super(manageable.getUrl(), manageable.getResourceType(), manageable.getAttributes(), manageable.getBody(), manageable.getName(), manageable.isExists(), managedResourceUrl);
  }

  /**
   * Construct a missing manager resource.
   * @param managedResourceUrl URL of the resource that would be managed
   * @param url URL of the resource
   * @param resourceType Identified shape tree resource type
   * @param attributes Associated resource attributes
   * @param body Body of the resource
   * @param name Name of the resource
   */
  public constructor(managedResourceUrl: URL, url: URL, resourceType: ShapeTreeResourceType, attributes: ResourceAttributes, body: string, name: string) {
    super(url, resourceType, attributes, body, name, false, managedResourceUrl);
  }
}
