// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.client.http
import { ShapeTreeClient } from '@shapetrees/ShapeTreeClient';
import { ShapeTreeManager } from '@shapetrees/ShapeTreeManager';
import { ShapeTreeContext } from '@shapetrees/ShapeTreeContext';
import { ManageableInstance } from '@shapetrees/ManageableInstance';
import { ManageableResource } from '@shapetrees/ManageableResource';
import { DocumentResponse } from '@shapetrees/DocumentResponse';
import { ShapeTree } from '@shapetrees/ShapeTree';
import { ShapeTreeFactory } from '@shapetrees/ShapeTreeFactory';
import { ShapeTreeAssignment } from '@shapetrees/ShapeTreeAssignment';
import { ResourceAttributes } from '@shapetrees/ResourceAttributes';
import { HttpHeaders } from '@shapetrees/enums/HttpHeaders';
import { LinkRelations } from '@shapetrees/enums/LinkRelations';
import { ShapeTreeException } from '@shapetrees/exceptions/ShapeTreeException';
import * as Lang from 'org/apache/jena/riot';
import * as RDFDataMgr from 'org/apache/jena/riot';
import { Writable } from 'stream';
import { HttpRequest } from './HttpRequest';
import { HttpResourceAccessor } from './HttpResourceAccessor';
import { HttpClient } from './HttpClient';

export class HttpShapeTreeClient implements ShapeTreeClient {

   private useClientShapeTreeValidation: boolean = true;

  override public isShapeTreeValidationSkipped(): boolean {
    return !this.useClientShapeTreeValidation;
  }

  override public skipShapeTreeValidation(skipValidation: boolean): void {
    this.useClientShapeTreeValidation = !skipValidation;
  }

  /**
   * Discover the ShapeTreeManager associated with a given target resource.
   * Implements {@link ShapeTreeClient#discoverShapeTree}
   *
   * Shape Trees, §4.1: This operation is used by a client-side agent to discover any shape trees associated
   * with a given resource. If URL is a managed resource, the associated Shape Tree Manager will be returned.
   * https://shapetrees.org/TR/specification/#discover
   *
   * @param context ShapeTreeContext that would be used for authentication purposes
   * @param targetResource The URL of the target resource for shape tree discovery
   * @return
   * @throws ShapeTreeException
   */
  override public discoverShapeTree(context: ShapeTreeContext, targetResource: URL): ShapeTreeManager | null /* throws ShapeTreeException */ {
    if (targetResource === null) {
      throw new ShapeTreeException(500, "Must provide a value target resource for discovery");
    }
    log.debug("Discovering shape tree manager managing {}", targetResource);
    // Lookup the target resource for pointer to associated shape tree manager
    const resourceAccessor: HttpResourceAccessor = new HttpResourceAccessor();
    let instance: ManageableInstance = resourceAccessor.getInstance(context, targetResource);
    let manageableResource: ManageableResource = instance.getManageableResource();
    if (!manageableResource.isExists()) {
      log.debug("Target resource for discovery {} does not exist", targetResource);
      return Optional.empty();
    }
    if (instance.wasRequestForManager()) {
      throw new ShapeTreeException(500, "Discovery target must not be a shape tree manager resource");
    }
    if (instance.isUnmanaged()) {
      return Optional.empty();
    }
    return Optional.of(instance.getManagerResource().getManager());
  }

  /**
   * Shape Trees, §4.2: This operation marks an existing resource as being managed by one or more shape trees,
   * by associating a shape tree manager with the resource, and turning it into a managed resource.
   *
   * If the resource is already managed, the associated shape tree manager will be updated with another
   * shape tree assignment for the planted shape tree.
   *
   * If the resource is a container that already contains existing resources, and a recursive plant is requested,
   * this operation will perform a depth first traversal through the containment hierarchy, validating
   * and assigning as it works its way back up to the target root resource of this operation.
   *
   * https://shapetrees.org/TR/specification/#plant-shapetree
   *
   * @param context ShapeTreeContext that would be used for authentication purposes
   * @param targetResource The URL of the resource to plant on
   * @param targetShapeTree A URL representing the shape tree to plant for targetResource
   * @param focusNode An optional URL representing the target subject within targetResource used for shape validation
   * @return The URL of the Shape Tree Manager that was planted for targetResource
   * @throws ShapeTreeException
   */
  override public plantShapeTree(context: ShapeTreeContext, targetResource: URL, targetShapeTree: URL, focusNode: URL): DocumentResponse /* throws ShapeTreeException */ {
    if (context === null || targetResource === null || targetShapeTree === null) {
      throw new ShapeTreeException(500, "Must provide a valid context, target resource, and target shape tree to the plant shape tree");
    }
    log.debug("Planting shape tree {} on {}: ", targetShapeTree, targetResource);
    log.debug("Focus node: {}", focusNode === null ? "None provided" : focusNode);
    const resourceAccessor: HttpResourceAccessor = new HttpResourceAccessor();
    // Lookup the shape tree
    let shapeTree: ShapeTree = ShapeTreeFactory.getShapeTree(targetShapeTree);
    // Lookup the target resource
    let instance: ManageableInstance = resourceAccessor.getInstance(context, targetResource);
    let manageableResource: ManageableResource = instance.getManageableResource();
    if (!manageableResource.isExists()) {
      return new DocumentResponse(null, "Cannot find target resource to plant: " + targetResource, 404);
    }
    let manager: ShapeTreeManager;
    let managerResourceUrl: URL = instance.getManagerResource().getUrl();
    if (instance.isManaged()) {
      manager = instance.getManagerResource().getManager();
    } else {
      manager = new ShapeTreeManager(managerResourceUrl);
    }
    // Initialize a shape tree assignment based on the supplied parameters
    let assignmentUrl: URL = manager.mintAssignmentUrl();
    let assignment: ShapeTreeAssignment = new ShapeTreeAssignment(targetShapeTree, targetResource, assignmentUrl, focusNode, shapeTree.getShape(), assignmentUrl);
    // Add the assignment to the manager
    manager.addAssignment(assignment);
    // Get an RDF version of the manager stored in a turtle string
    let sw: Writable = new Writable();
    RDFDataMgr.write(sw, manager.getGraph(), Lang.TURTLE);
    // Build an HTTP PUT request with the manager graph in turtle as the content body + link header
    let fetcher: HttpClient = HttpClientFactoryManager.getFactory().get(this.useClientShapeTreeValidation);
    let headers: ResourceAttributes = new ResourceAttributes();
    headers.maybeSet(HttpHeaders.AUTHORIZATION.getValue(), context.getAuthorizationHeaderValue());
    return fetcher.fetchShapeTreeResponse(new HttpRequest("PUT", managerResourceUrl, headers, sw.toString(), "text/turtle"));
  }

  override public postManagedInstance(context: ShapeTreeContext, parentContainer: URL, focusNodes: Array<URL>, targetShapeTrees: Array<URL>, proposedResourceName: string, isContainer: boolean, bodyString: string, contentType: string): DocumentResponse /* throws ShapeTreeException */ {
    if (context === null || parentContainer === null) {
      throw new ShapeTreeException(500, "Must provide a valid context and parent container to post shape tree instance");
    }
    log.debug("POST-ing shape tree instance to {}", parentContainer);
    log.debug("Proposed name: {}", proposedResourceName === null ? "None provided" : proposedResourceName);
    log.debug("Target Shape Tree: {}", targetShapeTrees === null || targetShapeTrees.isEmpty() ? "None provided" : targetShapeTrees.toString());
    log.debug("Focus Node: {}", focusNodes === null || focusNodes.isEmpty() ? "None provided" : focusNodes.toString());
    let fetcher: HttpClient = HttpClientFactoryManager.getFactory().get(this.useClientShapeTreeValidation);
    let headers: ResourceAttributes = getCommonHeaders(context, focusNodes, targetShapeTrees, isContainer, proposedResourceName, contentType);
    return fetcher.fetchShapeTreeResponse(new HttpRequest("POST", parentContainer, headers, bodyString, contentType));
  }

  // Create via HTTP PUT
  override public putManagedInstance(context: ShapeTreeContext, resourceUrl: URL, focusNodes: Array<URL>, targetShapeTrees: Array<URL>, isContainer: boolean, bodyString: string, contentType: string): DocumentResponse /* throws ShapeTreeException */ {
    if (context === null || resourceUrl === null) {
      throw new ShapeTreeException(500, "Must provide a valid context and target resource to create shape tree instance via PUT");
    }
    log.debug("Creating shape tree instance via PUT at {}", resourceUrl);
    log.debug("Target Shape Tree: {}", targetShapeTrees === null || targetShapeTrees.isEmpty() ? "None provided" : targetShapeTrees.toString());
    log.debug("Focus Node: {}", focusNodes === null || focusNodes.isEmpty() ? "None provided" : focusNodes.toString());
    let fetcher: HttpClient = HttpClientFactoryManager.getFactory().get(this.useClientShapeTreeValidation);
    let headers: ResourceAttributes = getCommonHeaders(context, focusNodes, targetShapeTrees, isContainer, null, contentType);
    return fetcher.fetchShapeTreeResponse(new HttpRequest("PUT", resourceUrl, headers, bodyString, contentType));
  }

  // Update via HTTP PUT
  override public putManagedInstance(context: ShapeTreeContext, resourceUrl: URL, focusNodes: Array<URL>, bodyString: string, contentType: string): DocumentResponse /* throws ShapeTreeException */ {
    if (context === null || resourceUrl === null) {
      throw new ShapeTreeException(500, "Must provide a valid context and target resource to update shape tree instance via PUT");
    }
    log.debug("Updating shape tree instance via PUT at {}", resourceUrl);
    log.debug("Focus Node: {}", focusNodes === null || focusNodes.isEmpty() ? "None provided" : focusNodes.toString());
    let fetcher: HttpClient = HttpClientFactoryManager.getFactory().get(this.useClientShapeTreeValidation);
    let headers: ResourceAttributes = getCommonHeaders(context, focusNodes, null, null, null, contentType);
    return fetcher.fetchShapeTreeResponse(new HttpRequest("PUT", resourceUrl, headers, bodyString, contentType));
  }

  override public patchManagedInstance(context: ShapeTreeContext, resourceUrl: URL, focusNodes: Array<URL>, patchString: string): DocumentResponse /* throws ShapeTreeException */ {
    if (context === null || resourceUrl === null || patchString === null) {
      throw new ShapeTreeException(500, "Must provide a valid context, target resource, and PATCH expression to PATCH shape tree instance");
    }
    log.debug("PATCH-ing shape tree instance at {}", resourceUrl);
    log.debug("PATCH String: {}", patchString);
    log.debug("Focus Node: {}", focusNodes === null || focusNodes.isEmpty() ? "None provided" : focusNodes.toString());
    let contentType: string = "application/sparql-update";
    let fetcher: HttpClient = HttpClientFactoryManager.getFactory().get(this.useClientShapeTreeValidation);
    let headers: ResourceAttributes = getCommonHeaders(context, focusNodes, null, null, null, contentType);
    return fetcher.fetchShapeTreeResponse(new HttpRequest("PATCH", resourceUrl, headers, patchString, contentType));
  }

  override public deleteManagedInstance(context: ShapeTreeContext, resourceUrl: URL): DocumentResponse /* throws ShapeTreeException */ {
    if (context === null || resourceUrl === null) {
      throw new ShapeTreeException(500, "Must provide a valid context and target resource to DELETE shape tree instance");
    }
    log.debug("DELETE-ing shape tree instance at {}", resourceUrl);
    let fetcher: HttpClient = HttpClientFactoryManager.getFactory().get(this.useClientShapeTreeValidation);
    let headers: ResourceAttributes = getCommonHeaders(context, null, null, null, null, null);
    return fetcher.fetchShapeTreeResponse(new HttpRequest("DELETE", resourceUrl, headers, null, null));
  }

  override public unplantShapeTree(context: ShapeTreeContext, targetResource: URL, targetShapeTree: URL): DocumentResponse /* throws ShapeTreeException */ {
    if (context === null || targetResource === null || targetShapeTree === null) {
      throw new ShapeTreeException(500, "Must provide a valid context, target resource, and target shape tree to unplant");
    }
    log.debug("Unplanting shape tree {} managing {}: ", targetShapeTree, targetResource);
    // Lookup the target resource
    const resourceAccessor: HttpResourceAccessor = new HttpResourceAccessor();
    let instance: ManageableInstance = resourceAccessor.getInstance(context, targetResource);
    let manageableResource: ManageableResource = instance.getManageableResource();
    if (!manageableResource.isExists()) {
      return new DocumentResponse(null, "Cannot find target resource to unplant: " + targetResource, 404);
    }
    if (instance.isUnmanaged()) {
      return new DocumentResponse(null, "Cannot unplant target resource that is not managed by a shapetree: " + targetResource, 500);
    }
    // Remove assignment from manager that corresponds with the provided shape tree
    let manager: ShapeTreeManager = instance.getManagerResource().getManager();
    manager.removeAssignmentForShapeTree(targetShapeTree);
    let method: string;
    let body: string;
    let contentType: string;
    if (manager.getAssignments().isEmpty()) {
      method = "DELETE";
      body = null;
      contentType = null;
    } else {
      // Build an HTTP PUT request with the manager graph in turtle as the content body + link header
      method = "PUT";
      // Get a RDF version of the manager stored in a turtle string
      let sw: Writable = new Writable();
      RDFDataMgr.write(sw, manager.getGraph(), Lang.TURTLE);
      body = sw.toString();
      contentType = "text/turtle";
    }
    let fetcher: HttpClient = HttpClientFactoryManager.getFactory().get(this.useClientShapeTreeValidation);
    return fetcher.fetchShapeTreeResponse(new HttpRequest(method, manager.getId(), // why no getCommonHeaders(context, null, null, null, null, null)
    null, body, contentType));
  }

  private getCommonHeaders(context: ShapeTreeContext, focusNodes: Array<URL>, targetShapeTrees: Array<URL>, isContainer: boolean, proposedResourceName: string, contentType: string): ResourceAttributes {
    let ret: ResourceAttributes = new ResourceAttributes();
    if (context.getAuthorizationHeaderValue() != null) {
      ret.maybeSet(HttpHeaders.AUTHORIZATION.getValue(), context.getAuthorizationHeaderValue());
    }
    if (isContainer != null) {
      let resourceTypeUrl: string = Boolean.TRUE === isContainer ? "http://www.w3.org/ns/ldp#Container" : "http://www.w3.org/ns/ldp#Resource";
      ret.maybeSet(HttpHeaders.LINK.getValue(), "<" + resourceTypeUrl + ">; rel=\"type\"");
    }
    if (focusNodes != null && !focusNodes.isEmpty()) {
      for (const focusNode of focusNodes) {
        ret.maybeSet(HttpHeaders.LINK.getValue(), "<" + focusNode + ">; rel=\"" + LinkRelations.FOCUS_NODE.getValue() + "\"");
      }
    }
    if (targetShapeTrees != null && !targetShapeTrees.isEmpty()) {
      for (const targetShapeTree of targetShapeTrees) {
        ret.maybeSet(HttpHeaders.LINK.getValue(), "<" + targetShapeTree + ">; rel=\"" + LinkRelations.TARGET_SHAPETREE.getValue() + "\"");
      }
    }
    if (proposedResourceName != null) {
      ret.maybeSet(HttpHeaders.SLUG.getValue(), proposedResourceName);
    }
    if (contentType != null) {
      ret.maybeSet(HttpHeaders.CONTENT_TYPE.getValue(), contentType);
    }
    return ret;
  }
}
