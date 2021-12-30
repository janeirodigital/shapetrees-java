// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.core.comparators
import { ManageableInstance } from '../ManageableInstance';
import * as Comparator from 'java/util';

export class ResourceTypeAssignmentPriority implements Comparator<ManageableInstance>, Serializable {

  // Used for sorting by shape tree resource type with the following order
  // 1. Containers
  // 2. Resources
  // 3. Non-RDF Resources
  // @SneakyThrows
  public compare(a: ManageableInstance, b: ManageableInstance): number {
    return a.getManageableResource().getResourceType().compareTo(b.getManageableResource().getResourceType());
  }
}
