// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.client.http
import { ShapeTreeManager } from '@shapetrees/ShapeTreeManager';
import { ShapeTreeContext } from '@shapetrees/ShapeTreeContext';
import { ManageableInstance } from '@shapetrees/ManageableInstance';
import { ManageableResource } from '@shapetrees/ManageableResource';
import { DocumentResponse } from '@shapetrees/DocumentResponse';
import { ResourceAttributes } from '@shapetrees/ResourceAttributes';
import { InstanceResource } from '@shapetrees/InstanceResource';
import { ResourceAccessor } from '@shapetrees/ResourceAccessor';
import { ManagerResource } from '@shapetrees/ManagerResource';
import { MissingManageableResource } from '@shapetrees/MissingManageableResource';
import { MissingManagerResource } from '@shapetrees/MissingManagerResource';
import { UnmanagedResource } from '@shapetrees/UnmanagedResource';
import { ManagedResource } from '@shapetrees/ManagedResource';
import { HttpHeaders } from '@shapetrees/enums/HttpHeaders';
import { LinkRelations } from '@shapetrees/enums/LinkRelations';
import { ShapeTreeResourceType } from '@shapetrees/enums/ShapeTreeResourceType';
import { ShapeTreeException } from '@shapetrees/exceptions/ShapeTreeException';
import { LdpVocabulary } from '@shapetrees/vocabularies/LdpVocabulary';
import * as Graph from 'org/apache/jena/graph';
import * as Node from 'org/apache/jena/graph';
import * as NodeFactory from 'org/apache/jena/graph';
import * as Triple from 'org/apache/jena/graph';
import * as MalformedURLException from 'java/net';
import * as Set from 'java/util';
import * as Collections from 'java/util';
import { readStringIntoGraph } from '@shapetrees/helpers/GraphHelper/readStringIntoGraph';
import { urlToUri } from '@shapetrees/helpers/GraphHelper/urlToUri';
import { HttpRequest } from './HttpRequest';
import { HttpClient } from './HttpClient';

/**
 * Allows the {@link com.janeirodigital.shapetrees.core shapetrees-core} to access
 * {@link ManageableInstance}s and {@link InstanceResource}s over the network via HTTP. This is
 * particularly effective when employing client-side shape-tree validation in a
 * <a href="https://shapetrees.org/TR/specification/index.html#shapetree-support-from-proxy-or-client-side-library">proxy scenario</a>.
 *
 * <p>Given the fact that resources are accessed via HTTP, some inferences must be made on
 * resource state based on responses to HTTP requests.</p>
 */
export class HttpResourceAccessor implements ResourceAccessor {

   private static readonly supportedRDFContentTypes: Set<string> = Set.of("text/turtle", "application/rdf+xml", "application/n-triples", "application/ld+json");

  /**
   * Return a {@link ManageableInstance} constructed based on the provided <code>resourceUrl</code>,
   * which could target either a {@link ManageableResource} or a {@link ManagerResource}.
   * Both are retrieved via HTTP and loaded as specifically
   * typed sub-classes that indicate whether they exist, or (in the case of manageable resource)
   * whether they are managed.
   *
   * @param context {@link ShapeTreeContext}
   * @param resourceUrl URL of the target resource
   * @return {@link ManageableInstance} including {@link ManageableResource} and {@link ManagerResource}
   */
  override public getInstance(context: ShapeTreeContext, resourceUrl: URL): ManageableInstance /* throws ShapeTreeException */ {
    let resource: InstanceResource = this.getResource(context, resourceUrl);
    if (resource instanceof MissingManageableResource) {
      // Get is for a manageable resource that doesn't exist
      return getInstanceFromMissingManageableResource(context, (MissingManageableResource) resource);
    } else if (resource instanceof MissingManagerResource) {
      // Get is for a manager resource that doesn't exist
      return getInstanceFromMissingManagerResource(context, (MissingManagerResource) resource);
    } else if (resource instanceof ManageableResource) {
      // Get is for an existing manageable resource
      return getInstanceFromManageableResource(context, (ManageableResource) resource);
    } else if (resource instanceof ManagerResource) {
      // Get is for an existing manager resource
      return getInstanceFromManagerResource(context, (ManagerResource) resource);
    }
    throw new ShapeTreeException(500, "Can get instance from resource of unsupported type: " + resource.getUrl());
  }

  /**
   * Gets a {@link ManageableInstance} given a {@link MissingManageableResource}, which means that
   * a corresponding {@link ManagerResource} cannot exist, so a {@link MissingManagerResource} is
   * constructed and included as part of instance construction.
   * @param context {@link ShapeTreeContext}
   * @param missing {@link MissingManageableResource}
   * @return {@link ManageableInstance} including {@link MissingManageableResource} and {@link MissingManagerResource}
   */
  private getInstanceFromMissingManageableResource(context: ShapeTreeContext, missing: MissingManageableResource): ManageableInstance {
    let missingManager: MissingManagerResource = new MissingManagerResource(missing, null);
    return new ManageableInstance(context, this, false, missing, missingManager);
  }

  /**
   * Gets a {@link ManageableInstance} given a {@link MissingManagerResource}, which means that
   * a {@link ManagerResource} doesn't exist, but an {@link UnmanagedResource} that would be associated
   * with it may, so it is looked up over HTTP and populated with the appropriate resulting type
   * based on its existence.
   * @param context {@link ShapeTreeContext}
   * @param missing {@link MissingManagerResource}
   * @return {@link ManageableInstance} including {@link UnmanagedResource}|{@link MissingManageableResource} and {@link MissingManagerResource}
   * @throws ShapeTreeException
   */
  private getInstanceFromMissingManagerResource(context: ShapeTreeContext, missing: MissingManagerResource): ManageableInstance /* throws ShapeTreeException */ {
    let manageable: InstanceResource = this.getResource(context, calculateManagedUrl(missing.getUrl(), missing.getAttributes()));
    if (manageable.isExists()) {
      let unmanaged: UnmanagedResource = new UnmanagedResource((ManageableResource) manageable, Optional.of(missing.getUrl()));
      return new ManageableInstance(context, this, true, unmanaged, missing);
    } else {
      throw new ShapeTreeException(500, "Cannot have a shape tree manager " + missing.getUrl() + " for a missing manageable resource " + manageable.getUrl());
    }
  }

  /**
   * Gets a {@link ManageableInstance} given a {@link ManageableResource}, which could be a
   * {@link ManagedResource} or an {@link UnmanagedResource}. Which type is determined by
   * the presence of the {@link ManagerResource}, which is looked up and the instance is
   * populated with the appropriate resulting types.*
   * @param context {@link ShapeTreeContext}
   * @param manageable {@link ManagedResource} or {@link UnmanagedResource}
   * @return {@link ManageableInstance} including {@link UnmanagedResource}|{@link ManagedResource} and {@link ManagerResource}|{@link MissingManagerResource}
   * @throws ShapeTreeException
   */
  private getInstanceFromManageableResource(context: ShapeTreeContext, manageable: ManageableResource): ManageableInstance /* throws ShapeTreeException */ {
    let managerResourceUrl: URL = manageable.getManagerResourceUrl().orElseThrow(() -> new ShapeTreeException(500, "Cannot discover shape tree manager for " + manageable.getUrl()));
    let manager: InstanceResource = this.getResource(context, managerResourceUrl);
    if (manager instanceof MissingManagerResource) {
      // If the manager does exist it is unmanaged - Get and store both in instance
      let unmanaged: UnmanagedResource = new UnmanagedResource(manageable, Optional.of(manager.getUrl()));
      return new ManageableInstance(context, this, false, unmanaged, (ManagerResource) manager);
    } else if (manager instanceof ManagerResource) {
      // If the manager exists then it is managed - get and store manager and managed resource in instance
      let managed: ManagedResource = new ManagedResource(manageable, Optional.of(manager.getUrl()));
      return new ManageableInstance(context, this, false, managed, (ManagerResource) manager);
    } else {
      throw new ShapeTreeException(500, "Error looking up corresponding shape tree manager for " + manageable.getUrl());
    }
  }

  /**
   * Gets a {@link ManageableInstance} given a {@link ManagerResource}. The corresponding
   * {@link ManagedResource} is looked up and the instance is populated with it.
   * @param context {@link ShapeTreeContext}
   * @param manager Existing {@link ManagerResource}
   * @return {@link ManageableInstance} including {@link ManagerResource} and {@link ManagedResource}
   * @throws ShapeTreeException
   */
  private getInstanceFromManagerResource(context: ShapeTreeContext, manager: ManagerResource): ManageableInstance /* throws ShapeTreeException */ {
    let manageable: InstanceResource = this.getResource(context, manager.getManagedResourceUrl());
    if (manageable instanceof MissingManageableResource) {
      throw new ShapeTreeException(500, "Cannot have a shape tree manager at " + manager.getUrl() + " without a corresponding managed resource");
    }
    let managed: ManagedResource = new ManagedResource((ManageableResource) manageable, Optional.of(manager.getUrl()));
    return new ManageableInstance(context, this, true, managed, manager);
  }

  /**
   * Gets a {@link ManageableInstance} by first creating the provided <code>resourceUrl</code>, which could
   * mean creating either a {@link ManageableResource} or a {@link ManagerResource}. The newly created resource
   * is loaded into the instance, and the corresponding {@link ManageableResource} or {@link ManagerResource} is
   * looked up and loaded into the instance alongside it. They are loaded as specifically
   * typed sub-classes that indicate whether they exist, or (in the case of a {@link ManageableResource}),
   * whether they are managed.
   * @param context {@link ShapeTreeContext}
   * @param method HTTP method used for creation
   * @param resourceUrl URL of the resource to create
   * @param headers HTTP headers used for creation
   * @param body Body of the created resource
   * @param contentType Content-type of the created resource
   * @return {@link ManageableInstance} with {@link ManageableResource} and {@link ManagerResource}
   * @throws ShapeTreeException
   */
  override public createInstance(context: ShapeTreeContext, method: string, resourceUrl: URL, headers: ResourceAttributes, body: string, contentType: string): ManageableInstance /* throws ShapeTreeException */ {
    let resource: InstanceResource = this.createResource(context, method, resourceUrl, headers, body, contentType);
    if (resource instanceof ManageableResource) {
      // Managed or unmanaged resource was created
      return createInstanceFromManageableResource(context, (ManageableResource) resource);
    } else if (resource instanceof ManagerResource) {
      // Manager resource was created
      return createInstanceFromManagerResource(context, (ManagerResource) resource);
    }
    throw new ShapeTreeException(500, "Invalid resource type returned from resource creation");
  }

  /**
   * Gets a {@link ManageableInstance} given a newly created {@link ManageableResource}. A corresponding
   * {@link ManagerResource} is looked up. If it exists, a {@link ManagedResource} is initialized and loaded
   * into the instance. If it doesn't, an {@link UnmanagedResource} is initialized and loaded instead.
   * @param context {@link ShapeTreeContext}
   * @param manageable Newly created {@link ManageableResource}
   * @return {@link ManageableInstance} including {@link ManagedResource}|{@link UnmanagedResource} and {@link ManagerResource}|{@link MissingManagerResource}
   * @throws ShapeTreeException
   */
  private createInstanceFromManageableResource(context: ShapeTreeContext, manageable: ManageableResource): ManageableInstance /* throws ShapeTreeException */ {
    // Lookup the corresponding ManagerResource for the ManageableResource
    let managerResourceUrl: URL = manageable.getManagerResourceUrl().orElseThrow(() -> new ShapeTreeException(500, "Cannot discover shape tree manager for " + manageable.getUrl()));
    let manager: InstanceResource = this.getResource(context, managerResourceUrl);
    if (manager instanceof MissingManagerResource) {
      // Create and store an UnmanagedResource in instance - if the create was a resource in an unmanaged container
      let unmanaged: UnmanagedResource = new UnmanagedResource(manageable, Optional.of(manager.getUrl()));
      return new ManageableInstance(context, this, false, unmanaged, (ManagerResource) manager);
    } else if (manager instanceof ManagerResource) {
      // Create and store a ManagedResource in instance - if the create was a resource in a managed container
      let managed: ManagedResource = new ManagedResource(manageable, Optional.of(manager.getUrl()));
      return new ManageableInstance(context, this, false, managed, (ManagerResource) manager);
    }
    throw new ShapeTreeException(500, "Error lookup up corresponding shape tree manager for " + manageable.getUrl());
  }

  /**
   * Gets a {@link ManageableInstance} given a newly created {@link ManagerResource}. A corresponding
   * {@link ManagedResource} is looked up (and which must exist and be associated with this
   * manager).
   * @param context {@link ShapeTreeContext}
   * @param manager Newly created {@link ManagerResource}
   * @return {@link ManageableInstance} including {@link ManagerResource} and {@link ManagedResource}
   * @throws ShapeTreeException
   */
  private createInstanceFromManagerResource(context: ShapeTreeContext, manager: ManagerResource): ManageableInstance /* throws ShapeTreeException */ {
    // Lookup the corresponding ManagedResource for the ManagerResource
    let resource: InstanceResource = this.getResource(context, manager.getManagedResourceUrl());
    if (resource instanceof MissingManageableResource) {
      throw new ShapeTreeException(500, "Cannot have an existing manager resource " + manager.getUrl() + " with a non-existing managed resource " + resource.getUrl());
    } else if (resource instanceof ManagerResource) {
      throw new ShapeTreeException(500, "Invalid manager resource " + resource.getUrl() + " seems to be associated with another manager resource " + manager.getUrl());
    }
    let managed: ManagedResource = new ManagedResource((ManageableResource) resource, Optional.of(manager.getUrl()));
    return new ManageableInstance(context, this, true, managed, manager);
  }

  /**
   * Get a {@link InstanceResource} at the provided <code>url</code>, which may or may not exist.
   * Most of the work happens in {@link #generateResource(URL, DocumentResponse)}, which
   * processes the response and returns the corresponding typed resource.
   * @param context {@link ShapeTreeContext}
   * @param url Url of the resource to get
   * @return {@link InstanceResource}
   * @throws ShapeTreeException
   */
  override public getResource(context: ShapeTreeContext, url: URL): InstanceResource /* throws ShapeTreeException */ {
    log.debug("HttpResourceAccessor#getResource({})", url);
    let headers: ResourceAttributes = new ResourceAttributes();
    headers.maybeSet(HttpHeaders.AUTHORIZATION.getValue(), context.getAuthorizationHeaderValue());
    let fetcher: HttpClient = HttpClientFactoryManager.getFactory().get(false);
    let req: HttpRequest = new HttpRequest("GET", url, headers, null, null);
    let response: DocumentResponse = fetcher.fetchShapeTreeResponse(req);
    return generateResource(url, response);
  }

  /**
   * Create a {@link InstanceResource} at the provided <code>url</code> via the provided HTTP
   * <code>method</code>. Most of the work happens in {@link #generateResource(URL, DocumentResponse)},
   * which processes the response and returns the corresponding typed resource.
   * @param context {@link ShapeTreeContext}
   * @param method HTTP method to use for resource creation
   * @param url Url of the resource to create
   * @param headers HTTP headers to use for resource creation
   * @param body Body of resource to create
   * @param contentType HTTP content-type
   * @return {@link InstanceResource}
   * @throws ShapeTreeException
   */
  override public createResource(context: ShapeTreeContext, method: string, url: URL, headers: ResourceAttributes, body: string, contentType: string): InstanceResource /* throws ShapeTreeException */ {
    log.debug("createResource via {}: URL [{}], headers [{}]", method, url, headers.toString());
    let fetcher: HttpClient = HttpClientFactoryManager.getFactory().get(false);
    let allHeaders: ResourceAttributes = headers.maybePlus(HttpHeaders.AUTHORIZATION.getValue(), context.getAuthorizationHeaderValue());
    let response: DocumentResponse = fetcher.fetchShapeTreeResponse(new HttpRequest(method, url, allHeaders, body, contentType));
    if (!response.isExists()) {
      throw new ShapeTreeException(500, "Unable to create resource <" + url + ">");
    }
    return generateResource(url, response);
  }

  /**
   * Generates a typed {@link InstanceResource} based on the response from {@link #getResource(ShapeTreeContext, URL)} or
   * {@link #createResource(ShapeTreeContext, String, URL, ResourceAttributes, String, String)}.
   * Determines whether the resource is an existing {@link ManageableResource} or {@link ManagerResource}.
   * @param url Url of the resource to generate
   * @param response Response from a create or update of <code>url</code>
   * @return Generated {@link InstanceResource}, either {@link ManageableResource} or {@link ManagerResource}
   * @throws ShapeTreeException
   */
  private generateResource(url: URL, response: DocumentResponse): InstanceResource /* throws ShapeTreeException */ {
    // If a resource was created, ensure the URL returned in the Location header is valid
    let location: string | null = response.getResourceAttributes().firstValue(HttpHeaders.LOCATION.getValue());
    if (location.isPresent()) {
      try {
        url = new URL(location.get());
      } catch (ex) {
 if (ex instanceof MalformedURLException) {
         throw new ShapeTreeException(500, "Retrieving <" + url + "> yielded a Location header \"" + location.get() + "\" which doesn't parse as a URL: " + e.getMessage());
       }
    }
    // Determine whether the resource exists based on the response. Even if the resource
    // doesn't exist, additional context and processing is done to provide the appropriate
    // typed resource with adequate context to the caller
    const exists: boolean = response.isExists();
    const container: boolean = isContainerFromHeaders(response.getResourceAttributes(), url);
    const attributes: ResourceAttributes = response.getResourceAttributes();
    const resourceType: ShapeTreeResourceType = getResourceTypeFromHeaders(response.getResourceAttributes());
    const name: string = calculateName(url);
    const body: string = response.getBody();
    if (response.getBody() === null) {
      log.error("Could not retrieve the body string from response for " + url);
    }
    // Parse Link headers from response and populate ResourceAttributes
    const linkHeaders: Array<string> = attributes.allValues(HttpHeaders.LINK.getValue());
    let parsedLinkHeaders: ResourceAttributes = // !!
    linkHeaders.isEmpty() ? new ResourceAttributes() : ResourceAttributes.parseLinkHeaders(linkHeaders);
    // Determine if the resource is a shape tree manager based on the response
    const isManager: boolean = calculateIsManager(url, exists, parsedLinkHeaders);
    if (Boolean.TRUE === isManager) {
      const managedResourceUrl: URL = calculateManagedUrl(url, parsedLinkHeaders);
      if (exists) {
        return new ManagerResource(url, resourceType, attributes, body, name, true, managedResourceUrl);
      } else {
        return new MissingManagerResource(url, resourceType, attributes, body, name, managedResourceUrl);
      }
    } else {
      // Look for presence of st:managedBy in link headers from response and get the target manager URL
      const managerUrl: URL | null = calculateManagerUrl(url, parsedLinkHeaders);
      if (exists) {
        return new ManageableResource(url, resourceType, attributes, body, name, true, managerUrl, container);
      } else {
        return new MissingManageableResource(url, resourceType, attributes, body, name, managerUrl, container);
      }
    }
  }

  /**
   * Gets a List of contained {@link ManageableInstance}s from a given container specified by <code>containerUrl</code>
   * @param context {@link ShapeTreeContext}
   * @param containerUrl URL of target container resource
   * @return List of {@link ManageableInstance}s from the target container
   * @throws ShapeTreeException
   */
  override public getContainedInstances(context: ShapeTreeContext, containerUrl: URL): Array<ManageableInstance> /* throws ShapeTreeException */ {
    try {
      let resource: InstanceResource = this.getResource(context, containerUrl);
      if (!(resource instanceof ManageableResource)) {
        throw new ShapeTreeException(500, "Cannot get contained resources for a manager resource <" + containerUrl + ">");
      }
      let containerResource: ManageableResource = (ManageableResource) resource;
      if (Boolean.FALSE === containerResource.isContainer()) {
        throw new ShapeTreeException(500, "Cannot get contained resources for a resource that is not a Container <" + containerUrl + ">");
      }
      let containerGraph: Graph = readStringIntoGraph(urlToUri(containerUrl), containerResource.getBody(), containerResource.getAttributes().firstValue(HttpHeaders.CONTENT_TYPE.getValue()).orElse(null));
      if (containerGraph.isEmpty()) {
        return Collections.emptyList();
      }
      let containerTriples: Array<Triple> = containerGraph.find(NodeFactory.createURI(containerUrl.toString()), NodeFactory.createURI(LdpVocabulary.CONTAINS), Node.ANY).toList();
      if (containerTriples.isEmpty()) {
        return Collections.emptyList();
      }
      let containedInstances: Array<ManageableInstance> = new Array<>();
      for (const containerTriple of containerTriples) {
        let containedInstance: ManageableInstance = this.getInstance(context, new URL(containerTriple.getObject().getURI()));
        containedInstances.add(containedInstance);
      }
      return containedInstances;
    } catch (ex) {
 if (ex instanceof Exception) {
       throw new ShapeTreeException(500, ex.getMessage());
     }
  }

  /**
   * Updates the provided {@link InstanceResource} <code>updateResource</code> with <code>body</code> via the supplied
   * <code>method</code>
   * @param context Shape tree context
   * @param method HTTP method to use for update
   * @param updateResource {@link InstanceResource} to update
   * @param body Body to use for update
   * @return {@link DocumentResponse} of the result
   * @throws ShapeTreeException
   */
  override public updateResource(context: ShapeTreeContext, method: string, updateResource: InstanceResource, body: string): DocumentResponse /* throws ShapeTreeException */ {
    log.debug("updateResource: URL [{}]", updateResource.getUrl());
    let contentType: string = updateResource.getAttributes().firstValue(HttpHeaders.CONTENT_TYPE.getValue()).orElse(null);
    // [careful] updateResource attributes may contain illegal client headers (connection, content-length, date, expect, from, host, upgrade, via, warning)
    let allHeaders: ResourceAttributes = updateResource.getAttributes().maybePlus(HttpHeaders.AUTHORIZATION.getValue(), context.getAuthorizationHeaderValue());
    let fetcher: HttpClient = HttpClientFactoryManager.getFactory().get(false);
    return fetcher.fetchShapeTreeResponse(new HttpRequest(method, updateResource.getUrl(), allHeaders, body, contentType));
  }

  /**
   * Deletes the provided {@link InstanceResource }<code>deleteResource</code>
   * @param context {@link ShapeTreeContext}
   * @param deleteResource {@link InstanceResource} to delete
   * @return {@link DocumentResponse} of the result
   * @throws ShapeTreeException
   */
  override public deleteResource(context: ShapeTreeContext, deleteResource: ManagerResource): DocumentResponse /* throws ShapeTreeException */ {
    log.debug("deleteResource: URL [{}]", deleteResource.getUrl());
    let fetcher: HttpClient = HttpClientFactoryManager.getFactory().get(false);
    let allHeaders: ResourceAttributes = deleteResource.getAttributes().maybePlus(HttpHeaders.AUTHORIZATION.getValue(), context.getAuthorizationHeaderValue());
    let response: DocumentResponse = fetcher.fetchShapeTreeResponse(new HttpRequest("DELETE", deleteResource.getUrl(), allHeaders, null, null));
    let respCode: number = response.getStatusCode();
    if (respCode < 200 || respCode >= 400) {
      log.error("Error deleting resource {}, Status {}", deleteResource.getUrl(), respCode);
    }
    return response;
  }

  /**
   * Look for a Link rel=type of ldp:Container or ldp:BasicContainer
   * @param headers to parse
   * @return True if headers indicating a container are found
   */
  private isContainerFromHeaders(headers: ResourceAttributes, url: URL): boolean {
    let linkHeaders: Array<string> = headers.allValues(HttpHeaders.LINK.getValue());
    if (linkHeaders.isEmpty()) {
      return url.getPath().endsWith("/");
    }
    let parsedLinkHeaders: ResourceAttributes = ResourceAttributes.parseLinkHeaders(linkHeaders);
    let typeLinks: Array<string> = parsedLinkHeaders.allValues(LinkRelations.TYPE.getValue());
    if (!typeLinks.isEmpty()) {
      return typeLinks.contains(LdpVocabulary.CONTAINER) || typeLinks.contains(LdpVocabulary.BASIC_CONTAINER);
    }
    return false;
  }

  /**
   * Determine a resource type by parsing Link rel=type headers
   * @param headers to parse
   * @return Type of resource
   */
  private getResourceTypeFromHeaders(headers: ResourceAttributes): ShapeTreeResourceType {
    let linkHeaders: Array<string> = headers.allValues(HttpHeaders.LINK.getValue());
    if (linkHeaders === null) {
      return null;
    }
    let parsedLinkHeaders: ResourceAttributes = ResourceAttributes.parseLinkHeaders(linkHeaders);
    let typeLinks: Array<string> = parsedLinkHeaders.allValues(LinkRelations.TYPE.getValue());
    if (typeLinks != null && (typeLinks.contains(LdpVocabulary.CONTAINER) || typeLinks.contains(LdpVocabulary.BASIC_CONTAINER))) {
      return ShapeTreeResourceType.CONTAINER;
    }
    if (supportedRDFContentTypes.contains(headers.firstValue(HttpHeaders.CONTENT_TYPE.getValue()).orElse(""))) {
      // orElse("") because contains(null) throw NPE
      return ShapeTreeResourceType.RESOURCE;
    }
    return ShapeTreeResourceType.NON_RDF;
  }

  /**
   * Looks for the presence of the http://www.w3.org/ns/shapetrees#managedBy HTTP Link Relation in the
   * provided <code>parsedLinkHeaders</code>, with a valid target URL of a {@link ShapeTreeManager} associated
   * with the provided <code>url</code>.
   * @param url URL of the (potentially) managed resource
   * @param parsedLinkHeaders Parsed HTTP Link headers to evaluate
   * @return
   * @throws ShapeTreeException
   */
  private calculateManagerUrl(url: URL, parsedLinkHeaders: ResourceAttributes): URL | null /* throws ShapeTreeException */ {
    const optManagerString: string | null = parsedLinkHeaders.firstValue(LinkRelations.MANAGED_BY.getValue());
    if (optManagerString.isEmpty()) {
      log.info("The resource {} does not contain a link header of {}", url, LinkRelations.MANAGED_BY.getValue());
      return Optional.empty();
    }
    let managerUrlString: string = optManagerString.get();
    try {
      return Optional.of(new URL(url, managerUrlString));
    } catch (ex) {
 if (ex instanceof MalformedURLException) {
       throw new ShapeTreeException(500, "Malformed relative URL <" + managerUrlString + "> (resolved from <" + url + ">)");
     }
  }

  /**
   * Looks for the presence of the http://www.w3.org/ns/shapetrees#manages HTTP Link Relation in the
   * provided <code>parsedLinkHeaders</code>, with a valid target URL of a {@link ManagedResource}. Falls
   * back to a relatively crude inference when the more reliable header isn't available
   * @param managerUrl URL of the {@link ShapeTreeManager}
   * @param parsedLinkHeaders Parsed link headers from {@link ManagerResource} response
   * @return URL of {@link ManagedResource}
   * @throws ShapeTreeException
   */
  private calculateManagedUrl(managerUrl: URL, parsedLinkHeaders: ResourceAttributes): URL /* throws ShapeTreeException */ {
    let managedUrlString: string;
    let managedResourceUrl: URL;
    const optManagedString: string | null = parsedLinkHeaders.firstValue(LinkRelations.MANAGES.getValue());
    if (!optManagedString.isEmpty()) {
      managedUrlString = optManagedString.get();
    } else {
      // Attempt to (crudely) infer based on path calculation
      // If this implementation uses a dot notation for meta, trim it from the path
      // Rebuild without the query string in case that was employed
      managedUrlString = managerUrl.getPath().replaceAll("\\.shapetree$", "");
    }
    try {
      managedResourceUrl = new URL(managerUrl, managedUrlString);
    } catch (ex) {
 if (ex instanceof MalformedURLException) {
       throw new ShapeTreeException(500, "Can't calculate managed resource for shape tree manager <" + managerUrl + ">");
     }
    return managedResourceUrl;
  }

  /**
   * Calculates the name of the resource itself, removing any leading path and any trailing slash. In
   * the event that the resource is '/', then '/' will be returned.
   * @param url URL of the resource to evaluate
   * @return Name of resource
   */
  private calculateName(url: URL): string {
    let path: string = url.getPath();
    if (path === "/")
      return "/";
    // if this is a container, trim the trailing slash
    if (path.endsWith("/")) {
      path = path.substring(0, path.length() - 1);
    }
    let pathIndex: number = path.lastIndexOf('/');
    // No slashes in the path
    if (pathIndex === -1) {
      return path;
    }
    return path.substring(path.lastIndexOf('/') + 1);
  }

  /**
   * Determine whether <code>url</code> is a {@link ManagerResource}. Since this is a completely HTTP based
   * resource processor, this determination can't be made with special server-side knowledge about
   * the nature of the resources it serves. Instead, this must be derived based on information
   * present in the HTTP response from the server.
   * @param url URL of the resource that is being evaluated
   * @param exists whether the resource at <code>url</code> exists
   * @param parsedLinkHeaders Parsed HTTP Link headers from the response for <code>url</code>
   * @return True if {@link ManagerResource}
   */
  private calculateIsManager(url: URL, exists: boolean, parsedLinkHeaders: ResourceAttributes): boolean {
    // If the resource has an HTTP Link header of type of https://www.w3.org/ns/shapetrees#managedBy
    // with a manager target, it is not a manager resource (because it is managed by one)
    if (Boolean.TRUE === exists && parsedLinkHeaders.firstValue(LinkRelations.MANAGED_BY.getValue()).isPresent()) {
      return false;
    }
    // If the resource has an HTTP Link header of type of https://www.w3.org/ns/shapetrees#manages
    // it is a manager resource (because it manages another one).
    if (Boolean.TRUE === exists && parsedLinkHeaders.firstValue(LinkRelations.MANAGES.getValue()).isPresent()) {
      return true;
    }
    // If the resource doesn't exist, attempt to infer based on the URL
    if (url.getPath() != null && url.getPath().matches(".*\\.shapetree$")) {
      return true;
    }
    return url.getQuery() != null && url.getQuery().matches(".*ext\\=shapetree$");
  }

  public constructor() {
  }
}
