// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.core
import { ManageableResource } from './ManageableResource';

/**
 * An UnmanagedResource indicates that a given ManageableResource
 * is not managed by a shape tree. This means that there is not an
 * associated ManagerResource that exists.
 */
export class UnmanagedResource extends ManageableResource {

  /**
   * Construct an UnmanagedResource based on a provided ManageableResource
   * <code>manageable</code> and <code>managerUrl</code>
   * @param manageable ManageableResource to construct the UnmanagedResource from
   * @param managerUrl URL of the associated shape tree manager resource
   */
  public constructor(manageable: ManageableResource, managerUrl: URL | null) {
    super(manageable.getUrl(), manageable.getResourceType(), manageable.getAttributes(), manageable.getBody(), manageable.getName(), manageable.isExists(), managerUrl, manageable.isContainer());
  }
}
