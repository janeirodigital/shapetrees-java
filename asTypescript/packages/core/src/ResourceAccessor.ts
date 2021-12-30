// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.core
import { ShapeTreeException } from './exceptions/ShapeTreeException';
import { ManageableInstance } from './ManageableInstance';
import { InstanceResource } from './InstanceResource';
import { ShapeTreeContext } from './ShapeTreeContext';
import { DocumentResponse } from './DocumentResponse';
import { ManagerResource } from './ManagerResource';
import { ResourceAttributes } from './ResourceAttributes';

/**
 * Interface used by the shape trees core for accessing {@link ManageableInstance}s
 * and individual {@link InstanceResource}s.
 *
 * <p>Depending upon the context, this could be implemented by a <code>ResourceAccessor</code> implementation
 * accessing a database or filesystem (typical of server-side processing), or by a <code>ResourceAccessor</code>
 * implementation that is working with remote resources over http (typical of client-side processing).</p>
 *
 * <p>Note that create and update methods make the assumption that requests to do so are
 * originating from HTTP requests regardless of context (hence the inclusion of method,
 * headers, and contentType).</p>
 *
 * <p>Deletion and Update of {@link ManageableInstance}s aren't supported, as both should be targeted
 * specifically to either a {@link ManageableResource} or {@link ManagerResource} with
 * {@link #deleteResource(ShapeTreeContext, ManagerResource) deleteResource} or
 * {@link #updateResource(ShapeTreeContext, String, InstanceResource, String) updateResource}.</p>
 */
export interface ResourceAccessor {

  /**
   * Return a {@link ManageableInstance} constructed starting with the resource identified by the provided
   * <code>resourceUrl</code>. The <code>resourceUrl</code> may target either a {@link ManageableResource},
   * or a {@link ManagerResource}.
   *
   * <p>Both the {@link ManageableResource} and {@link ManagerResource} are retrieved and loaded as specifically
   * typed sub-classes that indicate whether they exist, or (in the case of {@link ManageableResource})
   * whether they are managed.</p>
   * @param context {@link ShapeTreeContext}
   * @param resourceUrl URL of the resource to get
   * @return {@link ManageableInstance} including {@link ManageableResource} and {@link ManagerResource}
   * @throws ShapeTreeException
   */
  getInstance(context: ShapeTreeContext, resourceUrl: URL): ManageableInstance /* throws ShapeTreeException */;

  /**
   * Gets a {@link ManageableInstance} by first creating the resource identified by the provided
   * <code>resourceUrl</code>, which could mean creating either a {@link ManageableResource} or a {@link ManagerResource}.
   * The newly created resource is loaded into the instance, and the corresponding {@link ManageableResource} or
   * {@link ManagerResource} is looked up and loaded into the instance alongside it. They are loaded as specifically
   * typed sub-classes that indicate whether they exist, or (in the case of {@link ManageableResource}),
   * whether they are managed.
   * @param context {@link ShapeTreeContext}
   * @param method Incoming HTTP method triggering resource creation
   * @param resourceUrl URL of the resource to create
   * @param headers Incoming HTTP headers
   * @param body Body of the resource to create
   * @param contentType Content-type of the resource to create
   * @return {@link ManageableInstance} including {@link ManageableResource} and {@link ManagerResource}
   * @throws ShapeTreeException
   */
  createInstance(context: ShapeTreeContext, method: string, resourceUrl: URL, headers: ResourceAttributes, body: string, contentType: string): ManageableInstance /* throws ShapeTreeException */;

  /**
   * Gets a list of {@link ManageableInstance}s contained in the container at the <code>containerResourceUrl</code>.
   * @param context {@link ShapeTreeContext}
   * @param containerResourceUrl URL of the target container
   * @return List of contained {@link ManageableInstance}s
   * @throws ShapeTreeException
   */
  getContainedInstances(context: ShapeTreeContext, containerResourceUrl: URL): Array<ManageableInstance> /* throws ShapeTreeException */;

  /**
   * Gets a specific {@link InstanceResource} identified by the provided <code>resourceUrl</code>.
   * @param context {@link ShapeTreeContext}
   * @param resourceUrl URL of the target resource to get
   * @return {@link InstanceResource}
   * @throws ShapeTreeException
   */
  getResource(context: ShapeTreeContext, resourceUrl: URL): InstanceResource /* throws ShapeTreeException */;

  /**
   * Creates a specific {@link InstanceResource} identified by the provided <code>resourceUrl</code>.
   * @param context {@link ShapeTreeContext}
   * @param method Incoming HTTP method triggering resource creation
   * @param resourceUrl URL of the resource to create
   * @param headers Incoming HTTP headers
   * @param body Body of the resource to create
   * @param contentType Content-type of the resource to create
   * @return {@link InstanceResource}
   * @throws ShapeTreeException
   */
  createResource(context: ShapeTreeContext, method: string, resourceUrl: URL, headers: ResourceAttributes, body: string, contentType: string): InstanceResource /* throws ShapeTreeException */;

  /**
   * Updates a specific {@link InstanceResource} identified by the provided <code>updatedResource</code>
   * @param context {@link ShapeTreeContext}
   * @param method Incoming HTTP method triggering resource update
   * @param updatedResource {@link InstanceResource} to update
   * @param body Updated body of the {@link InstanceResource}
   * @return Updated {@link InstanceResource}
   * @throws ShapeTreeException
   */
  updateResource(context: ShapeTreeContext, method: string, updatedResource: InstanceResource, body: string): DocumentResponse /* throws ShapeTreeException */;

  /**
   * Deletes a specific {@link InstanceResource} identified by the provided <code>updatedResource</code>
   * @param context {@link ShapeTreeContext}
   * @param deleteResource {@link InstanceResource} to delete
   * @return Resultant {@link DocumentResponse}
   * @throws ShapeTreeException
   */
  deleteResource(context: ShapeTreeContext, deleteResource: ManagerResource): DocumentResponse /* throws ShapeTreeException */;
}
