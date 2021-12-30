// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.core
import { ShapeTreeResourceType } from './enums/ShapeTreeResourceType';
import { ManageableResource } from './ManageableResource';
import { ResourceAttributes } from './ResourceAttributes';

/**
 * A MissingManageableResource represents a state where a given
 * ManageableResource at a URL does not exist.
 */
export class MissingManageableResource extends ManageableResource {

  /**
   * Construct a missing manageable resource.
   * @param url URL of the resource
   * @param resourceType Identified shape tree resource type
   * @param attributes Associated resource attributes
   * @param body Body of the resource
   * @param name Name of the resource
   * @param managerResourceUrl URL of the shape tree manager resource
   * @param isContainer Whether the resource is a container
   */
  public constructor(url: URL, resourceType: ShapeTreeResourceType, attributes: ResourceAttributes, body: string, name: string, managerResourceUrl: URL | null, isContainer: boolean) {
    super(url, resourceType, attributes, body, name, false, managerResourceUrl, isContainer);
  }
}
