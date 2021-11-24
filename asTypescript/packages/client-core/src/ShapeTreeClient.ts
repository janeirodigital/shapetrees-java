// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.client.core
import { DocumentResponse } from '@shapetrees/DocumentResponse';
import { ShapeTreeContext } from '@shapetrees/ShapeTreeContext';
import { ShapeTreeManager } from '@shapetrees/ShapeTreeManager';
import { ShapeTreeException } from '@shapetrees/exceptions/ShapeTreeException';

/**
 * This interface defines a proposed API to be used for any client-side implementations of
 * a shape tree client
 */
export interface ShapeTreeClient {

  /**
   * Shape Trees, §4.1: This operation is used by a client-side agent to discover any shape trees associated
   * with a given resource. If URL is a managed resource, the associated Shape Tree Manager will be returned.
   *
   * https://shapetrees.org/TR/specification/#discover
   *
   * @param context ShapeTreeContext that would be used for authentication purposes
   * @param targetResource The URL of the target resource for shape tree discovery
   * @return A ShapeTreeManager associated with targetResource
   * @throws ShapeTreeException ShapeTreeException
   */
  discoverShapeTree(context: ShapeTreeContext, targetResource: URL): ShapeTreeManager | null /* throws ShapeTreeException */;

  /**
   * Shape Trees, §4.2: This operation marks an existing resource as being managed by one or more shape trees,
   * by associating a shape tree manager with the resource, and turning it into a managed resource.
   *
   * If the resource is already managed, the associated shape tree manager will be updated with another
   * shape tree assignment for the planted shape tree.
   *
   * If the resource is a container that already contains existing resources, this operation will
   * perform a depth first traversal through the containment hierarchy, validating
   * and assigning as it works its way back up to the target resource of this operation.
   *
   * https://shapetrees.org/TR/specification/#plant-shapetree
   *
   * Plants one or more shape trees at a given container
   * @param context ShapeTreeContext that would be used for authentication purposes
   * @param targetResource The URL of the resource to plant on
   * @param targetShapeTree A URL representing the shape tree to plant for targetResource
   * @param focusNode An optional URL representing the target subject within targetResource used for shape validation
   * @return DocumentResponse containing status and response headers/attributes
   * @throws ShapeTreeException ShapeTreeException
   */
  plantShapeTree(context: ShapeTreeContext, targetResource: URL, targetShapeTree: URL, focusNode: URL): DocumentResponse /* throws ShapeTreeException */;

  /**
   * Shape Trees, §4.3: This operation unassigns a planted root shape tree from a root shape tree instance. If
   * the root shape tree instance is a managed container, it will also unassign contained resources.
   * If there are no remaining shape trees managing the resource, it would no longer be considered as managed.
   *
   * https://shapetrees.org/TR/specification/#unplant-shapetree
   *
   * @param context ShapeTreeContext that would be used for authentication purposes
   * @param targetResource URL of target resource to unplant shape tree from
   * @param targetShapeTree URL of shape tree being unplanted
   */
  unplantShapeTree(context: ShapeTreeContext, targetResource: URL, targetShapeTree: URL): DocumentResponse /* throws ShapeTreeException */;

  /**
   * Creates a resource via HTTP POST that has been validated against the provided shape tree
   * @param context ShapeTreeContext that would be used for authentication purposes
   * @param parentContainer The container the created resource should be created within
   * @param focusNodes One or more nodes/subjects to use as the focus for shape validation
   * @param targetShapeTrees One or more target shape trees the resource should be validated by
   * @param proposedName Proposed resource name (aka Slug) for the resulting resource
   * @param isContainer Specifies whether the newly created resource should be created as a container or not
   * @param bodyString String representation of body of the created resource
   * @param contentType Content type to parse the bodyString parameter as
   * @return DocumentResponse containing status and response headers/attributes
   * @throws ShapeTreeException ShapeTreeException
   */
  postManagedInstance(context: ShapeTreeContext, parentContainer: URL, focusNodes: Array<URL>, targetShapeTrees: Array<URL>, proposedName: string, isContainer: boolean, bodyString: string, contentType: string): DocumentResponse /* throws ShapeTreeException */;

  /**
   * Creates a resource via HTTP PUT that has been validated against the provided target shape tree
   * @param context ShapeTreeContext that would be used for authentication purposes
   * @param targetResource The target resource to be created or updated
   * @param focusNodes One or more nodes/subjects to use as the focus for shape validation
   * @param targetShapeTrees The shape trees that a proposed resource to be created should be validated against
   * @param isContainer Specifies whether a newly created resource should be created as a container or not
   * @param bodyString String representation of the body of the resource to create or update
   * @param contentType Content type to parse the bodyString parameter as
   * @return DocumentResponse containing status and response header / attributes
   * @throws ShapeTreeException
   */
  putManagedInstance(context: ShapeTreeContext, targetResource: URL, focusNodes: Array<URL>, targetShapeTrees: Array<URL>, isContainer: boolean, bodyString: string, contentType: string): DocumentResponse /* throws ShapeTreeException */;

  /**
   * Updates a resource via HTTP PUT that has been validated against an associated shape tree
   * @param context ShapeTreeContext that would be used for authentication purposes
   * @param targetResource The target resource to be created or updated
   * @param focusNodes One or more nodes/subjects to use as the focus for shape validation
   * @param bodyString String representation of the body of the resource to create or update
   * @param contentType Content type to parse the bodyString parameter as
   * @return DocumentResponse containing status and response header / attributes
   * @throws ShapeTreeException
   */
  putManagedInstance(context: ShapeTreeContext, targetResource: URL, focusNodes: Array<URL>, bodyString: string, contentType: string): DocumentResponse /* throws ShapeTreeException */;

  /**
   * Updates a resource via HTTP PATCH that has been validated against an associated shape tree
   * @param context ShapeTreeContext that would be used for authentication purposes
   * @param targetResource The target resource to be created or updated
   * @param focusNodes One or more nodes/subjects to use as the focus for shape validation
   * @param patchString SPARQL Update statement to use in patching the resource
   * @return DocumentResponse containing status and response header / attributes
   * @throws ShapeTreeException
   */
  patchManagedInstance(context: ShapeTreeContext, targetResource: URL, focusNodes: Array<URL>, patchString: string): DocumentResponse /* throws ShapeTreeException */;

  /**
   * Deletes an existing resource.  Provided as a convenience - no validation is performed
   * @param context ShapeTreeContext that would be used for authentication purposes
   * @param resourceUrl The URL of the resource being deleted
   * @return DocumentResponse containing status and response headers/attributes
   * @throws ShapeTreeException ShapeTreeException
   */
  deleteManagedInstance(context: ShapeTreeContext, resourceUrl: URL): DocumentResponse /* throws ShapeTreeException */;

  /**
   * Indicates whether validation is currently being applied on the client
   * @return boolean of whether client-side validation is being performed
   */
  isShapeTreeValidationSkipped(): boolean;

  /**
   * Determines whether validation should be performed on the client
   * @param skipValidation boolean indicating whether validation should be performed on the client
   */
  skipShapeTreeValidation(skipValidation: boolean): void;
}
