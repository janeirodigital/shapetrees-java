// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.core
import { ManageableResource } from './ManageableResource';

/**
 * A ManagedResource indicates that a given ManageableResource
 * is managed by a shape tree. This means that is has an associated
 * ManagerResource that exists and contains a valid ShapeTreeManager.
 */
export class ManagedResource extends ManageableResource {

  /**
   * Construct a ManagedResource based on a provided ManageableResource
   * <code>manageable</code> and <code>managerUrl</code>
   * @param manageable ManageableResource to construct the ManagedResource from
   * @param managerUrl URL of the associated shape tree manager resource
   */
  public constructor(manageable: ManageableResource, managerUrl: URL | null) {
    super(manageable.getUrl(), manageable.getResourceType(), manageable.getAttributes(), manageable.getBody(), manageable.getName(), manageable.isExists(), managerUrl, manageable.isContainer());
  }
}
