// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.tests
import { ShapeTreeException } from '@shapetrees/core/src/exceptions/ShapeTreeException';
import { ShapeTreeContext } from '@shapetrees/core/src/ShapeTreeContext';
import { ShapeTreeManager } from '@shapetrees/core/src/ShapeTreeManager';
import { DispatcherEntry } from './fixtures/DispatcherEntry';
import { RequestMatchingFixtureDispatcher } from './fixtures/RequestMatchingFixtureDispatcher';
import * as MockWebServer from 'okhttp3/mockwebserver';
import * as Assertions from 'org/junit/jupiter/api';
import * as BeforeAll from 'org/junit/jupiter/api';
import * as DisplayName from 'org/junit/jupiter/api';
import * as Test from 'org/junit/jupiter/api';
import * as MalformedURLException from 'java/net';
import * as assertFalse from 'org/junit/jupiter/api/Assertions';
import * as assertTrue from 'org/junit/jupiter/api/Assertions';

export class AbstractResourceAccessorTests {

   protected resourceAccessor: ResourceAccessor = null;

   protected readonly context: ShapeTreeContext;

   protected static server: MockWebServer = null;

   protected static dispatcher: RequestMatchingFixtureDispatcher = null;

  public constructor() {
    this.context = new ShapeTreeContext(null);
  }

  public toUrl(server: MockWebServer, path: string): URL /* throws MalformedURLException */ {
    // TODO: duplicates com.janeirodigital.shapetrees.tests.fixtures.MockWebServerHelper.getURL;
    return new URL(server.url(path).toString());
  }

  // @BeforeAll
  static beforeAll(): void {
    dispatcher = new RequestMatchingFixtureDispatcher(List.of(new DispatcherEntry(List.of("resourceAccessor/resource-no-link-headers"), "GET", "/static/resource/resource-no-link-headers", null), new DispatcherEntry(List.of("resourceAccessor/resource-empty-link-header"), "GET", "/static/resource/resource-empty-link-header", null), new DispatcherEntry(List.of("resourceAccessor/resource-container-link-header"), "GET", "/static/resource/resource-container-link-header", null), new DispatcherEntry(List.of("resourceAccessor/resource-container-link-header"), "GET", "/static/resource/resource-container-link-header/", null), new DispatcherEntry(List.of("resourceAccessor/resource-container-invalid-link-header"), "GET", "/static/resource/resource-container-invalid-link-header/", null), new DispatcherEntry(List.of("resourceAccessor/managed-container-1"), "GET", "/static/resource/managed-container-1/", null), new DispatcherEntry(List.of("resourceAccessor/managed-resource-1-create"), "PUT", "/static/resource/managed-container-1/managed-resource-1/", null), new DispatcherEntry(List.of("resourceAccessor/managed-resource-1-manager"), "GET", "/static/resource/managed-container-1/managed-resource-1/.shapetree", null), new DispatcherEntry(List.of("resourceAccessor/managed-container-1-manager"), "GET", "/static/resource/managed-container-1/.shapetree", null), new DispatcherEntry(List.of("resourceAccessor/unmanaged-container-2"), "GET", "/static/resource/unmanaged-container-2/", null), new DispatcherEntry(List.of("resourceAccessor/managed-container-2"), "GET", "/static/resource/managed-container-2/", null), new DispatcherEntry(List.of("resourceAccessor/unmanaged-resource-1-create"), "PUT", "/static/resource/unmanaged-resource-1", null), new DispatcherEntry(List.of("resourceAccessor/managed-container-2-manager-create"), "PUT", "/static/resource/managed-container-2/.shapetree", null), new DispatcherEntry(List.of("errors/404"), "GET", "/static/resource/missing-resource-1.shapetree", null), new DispatcherEntry(List.of("errors/404"), "GET", "/static/resource/missing-resource-2", null), new DispatcherEntry(List.of("resourceAccessor/missing-resource-2-manager-create"), "PUT", "/static/resource/missing-resource-2.shapetree", null), new DispatcherEntry(List.of("shapetrees/project-shapetree-ttl"), "GET", "/static/shapetrees/project/shapetree", null), new DispatcherEntry(List.of("schemas/project-shex"), "GET", "/static/shex/project/shex", null), new DispatcherEntry(List.of("errors/404"), "GET", "/static/resource/notpresent", null)));
    server = new MockWebServer();
    server.setDispatcher(dispatcher);
  }

  // Tests to Get ManageableInstances
  // @Test, @SneakyThrows, @DisplayName("Get instance from missing resource")
  getInstanceFromMissingResource(): void {
    let instance: ManageableInstance = this.resourceAccessor.getInstance(context, toUrl(server, "/static/resource/notpresent"));
    Assertions.assertTrue(instance.getManageableResource() instanceof MissingManageableResource);
    Assertions.assertTrue(instance.getManagerResource() instanceof MissingManagerResource);
    Assertions.assertFalse(instance.isManaged());
    Assertions.assertEquals(instance.getManageableResource().getUrl(), toUrl(server, "/static/resource/notpresent"));
    Assertions.assertFalse(instance.getManageableResource().isExists());
    Assertions.assertFalse(instance.getManagerResource().isExists());
  }

  // @Test, @SneakyThrows, @DisplayName("Get instance from managed resource")
  getInstanceFromManagedResource(): void {
    // If the resource is Manageable - determine if it is managed by getting manager
    // Get and store a ManagedResource in instance - Manager exists - store manager in instance too
    let instance: ManageableInstance = this.resourceAccessor.getInstance(context, toUrl(server, "/static/resource/managed-container-1/"));
    Assertions.assertTrue(instance.getManageableResource() instanceof ManagedResource);
    Assertions.assertNotNull(instance.getManagerResource());
    Assertions.assertFalse(instance.getManagerResource() instanceof MissingManagerResource);
    Assertions.assertTrue(instance.isManaged());
    Assertions.assertFalse(instance.isUnmanaged());
    let managerResource: ManagerResource = instance.getManagerResource();
    let manager: ShapeTreeManager = managerResource.getManager();
    Assertions.assertEquals(1, manager.getAssignments().size());
  }

  // @Test, @SneakyThrows, @DisplayName("Get instance for managed resource from manager request")
  getInstanceFromManagedResourceFromManager(): void {
    let instance: ManageableInstance = this.resourceAccessor.getInstance(context, toUrl(server, "/static/resource/managed-container-1/.shapetree"));
    Assertions.assertNotNull(instance.getManagerResource());
    Assertions.assertFalse(instance.getManagerResource() instanceof MissingManagerResource);
    Assertions.assertTrue(instance.getManagerResource().isExists());
    Assertions.assertTrue(instance.getManageableResource() instanceof ManagedResource);
    Assertions.assertEquals(instance.getManagerResource().getManagedResourceUrl(), instance.getManageableResource().getUrl());
  }

  // @Test, @SneakyThrows, @DisplayName("Fail to get instance for missing resource from manager request")
  failToGetInstanceForMissingManageableResourceFromManager(): void {
    // Note that in this request, the manager is also non-existent
    Assertions.assertThrows(ShapeTreeException.class, () -> {
      let instance: ManageableInstance = this.resourceAccessor.getInstance(context, toUrl(server, "/static/resource/missing-resource-1.shapetree"));
    });
  }

  // @Test, @SneakyThrows, @DisplayName("Get instance from unmanaged resource")
  getInstanceFromUnmanagedResource(): void {
    // If the resource is Manageable - determine if it is managed by getting manager
    // Get and store an UnmanagedResource in instance - No manager exists - store the location of the manager url
    let instance: ManageableInstance = this.resourceAccessor.getInstance(context, toUrl(server, "/static/resource/unmanaged-container-2/"));
    Assertions.assertTrue(instance.getManageableResource() instanceof UnmanagedResource);
    Assertions.assertTrue(instance.getManagerResource() instanceof MissingManagerResource);
    Assertions.assertTrue(instance.isUnmanaged());
    Assertions.assertFalse(instance.isManaged());
  }

  // @Test, @SneakyThrows, @DisplayName("Get instance from unmanaged resource from manager request")
  getInstanceFromUnmanagedResourceFromManager(): void {
    // Manager resource doesn't exist. Unmanaged resource associated with it does exist
    let instance: ManageableInstance = this.resourceAccessor.getInstance(context, toUrl(server, "/static/resource/unmanaged-container-2/.shapetree"));
    Assertions.assertTrue(instance.getManageableResource() instanceof UnmanagedResource);
    Assertions.assertTrue(instance.getManagerResource() instanceof MissingManagerResource);
    Assertions.assertTrue(instance.wasRequestForManager());
    Assertions.assertTrue(instance.isUnmanaged());
    Assertions.assertFalse(instance.isManaged());
  }

  // Tests to Create ManageableInstances
  // @Test, @SneakyThrows, @DisplayName("Create instance from managed resource")
  createInstanceFromManagedResource(): void {
    let headers: ResourceAttributes = new ResourceAttributes();
    let instance: ManageableInstance = this.resourceAccessor.createInstance(context, "PUT", toUrl(server, "/static/resource/managed-container-1/managed-resource-1/"), headers, getMilestoneThreeBodyGraph(), "text/turtle");
    Assertions.assertTrue(instance.isManaged());
    Assertions.assertTrue(instance.getManageableResource() instanceof ManagedResource);
    Assertions.assertFalse(instance.getManageableResource() instanceof MissingManageableResource);
    Assertions.assertTrue(instance.getManagerResource().isExists());
    Assertions.assertEquals(instance.getManagerResource().getManagedResourceUrl(), instance.getManageableResource().getUrl());
    let managerResource: ManagerResource = instance.getManagerResource();
    let manager: ShapeTreeManager = managerResource.getManager();
    Assertions.assertEquals(1, manager.getAssignments().size());
  }

  // @Test, @SneakyThrows, @DisplayName("Create instance from unmanaged resource")
  createInstanceFromUnmanagedResource(): void {
    let headers: ResourceAttributes = new ResourceAttributes();
    let instance: ManageableInstance = this.resourceAccessor.createInstance(context, "PUT", toUrl(server, "/static/resource/unmanaged-resource-1"), headers, "<#a> <#b> <#c>", "text/turtle");
    Assertions.assertTrue(instance.isUnmanaged());
    Assertions.assertTrue(instance.getManageableResource() instanceof UnmanagedResource);
    Assertions.assertTrue(instance.getManagerResource() instanceof MissingManagerResource);
  }

  // @Test, @SneakyThrows, @DisplayName("Fail to create instance from existing manageable resource")
  failToCreateInstanceFromExistingResource(): void {
    // Resource exists - ERROR - can't create a manageable resource when one already exists
    // May need to populate this
    let headers: ResourceAttributes = new ResourceAttributes();
    Assertions.assertThrows(ShapeTreeException.class, () -> {
      let instance: ManageableInstance = this.resourceAccessor.createInstance(context, "PUT", toUrl(server, "/static/resource/unmanaged-container-2/"), headers, "<#a> <#b> <#c>", "text/turtle");
    });
  }

  // @Test, @SneakyThrows, @DisplayName("Create instance from manager resource")
  createInstanceFromManagerResource(): void {
    // Create a new manager and store in instance and load the managed resource and store in instance (possibly just pre-fetch metadata if lazily loading)
    let headers: ResourceAttributes = new ResourceAttributes();
    let instance: ManageableInstance = this.resourceAccessor.createInstance(context, "PUT", toUrl(server, "/static/resource/managed-container-2/.shapetree"), headers, getProjectTwoManagerGraph(), "text/turtle");
    Assertions.assertTrue(instance.isManaged());
    Assertions.assertTrue(instance.getManageableResource() instanceof ManagedResource);
    Assertions.assertFalse(instance.getManagerResource() instanceof MissingManagerResource);
    // Probably need some additional tests
  }

  // @Test, @DisplayName("Fail to create instance from isolated manager resource")
  failToCreateInstanceFromIsolatedManagerResource(): void {
    let headers: ResourceAttributes = new ResourceAttributes();
    Assertions.assertThrows(ShapeTreeException.class, () -> {
      let instance: ManageableInstance = this.resourceAccessor.createInstance(context, "PUT", toUrl(server, "/static/resource/missing-resource-2.shapetree"), headers, getProjectTwoManagerGraph(), "text/turtle");
    });
  }

  // TODO - currently missing dedicated tests for create and delete. only one test for update (which is a failure test)
  // @Test, @DisplayName("Get a resource without any link headers")
  getResourceWithNoLinkHeaders(): void /* throws MalformedURLException, ShapeTreeException */ {
    let resource: InstanceResource = this.resourceAccessor.getResource(this.context, toUrl(server, "/static/resource/resource-no-link-headers"));
    // This is a strange way to check whether something has no link headers
    assertTrue(resource.isExists());
    assertTrue(((ManageableResource) resource).getManagerResourceUrl().isEmpty());
  }

  // @Test, @DisplayName("Get a resource with an empty link header")
  getResourceWithEmptyLinkHeader(): void /* throws MalformedURLException, ShapeTreeException */ {
    // Link header is present but has nothing in it
    let resource: InstanceResource = this.resourceAccessor.getResource(context, toUrl(server, "/static/resource/resource-empty-link-header"));
    assertTrue(resource.isExists());
    Assertions.assertTrue(((ManageableResource) resource).getManagerResourceUrl().isEmpty());
  }

  // @Test, @DisplayName("Fail to get a resource with an invalid URL string")
  failToAccessResourceWithInvalidUrlString(): void /* throws MalformedURLException, ShapeTreeException */ {
    // TODO: Test: may as well deleted as it's only testing URL.create()
    Assertions.assertThrows(MalformedURLException.class, () -> this.resourceAccessor.getResource(context, new URL(":invalid")));
    // TODO - this should also test create, update, delete, getContained, (also get/create instance)
  }

  // @Test, @DisplayName("Get a missing resource with no slash")
  getMissingResourceWithNoSlash(): void /* throws MalformedURLException, ShapeTreeException */ {
    let resource: InstanceResource = this.resourceAccessor.getResource(context, toUrl(server, "/static/resource/not-existing-no-slash"));
    assertFalse(resource.isExists());
    assertFalse(((ManageableResource) resource).isContainer());
  }

  // @Test, @DisplayName("Get a missing container with slash")
  getMissingContainerWithSlash(): void /* throws MalformedURLException, ShapeTreeException */ {
    let resource: InstanceResource = this.resourceAccessor.getResource(context, toUrl(server, "/static/resource/not-existing-slash/"));
    assertFalse(resource.isExists());
    assertTrue(((ManageableResource) resource).isContainer());
  }

  // @Test, @DisplayName("Get a missing container with slash and fragment")
  getMissingContainerWithSlashAndFragment(): void /* throws MalformedURLException, ShapeTreeException */ {
    let resource: InstanceResource = this.resourceAccessor.getResource(context, toUrl(server, "/static/resource/not-existing-slash/#withfragment"));
    assertFalse(resource.isExists());
    assertTrue(((ManageableResource) resource).isContainer());
  }

  // @Test, @DisplayName("Get an existing container with no slash")
  getExistingContainerNoSlash(): void /* throws MalformedURLException, ShapeTreeException */ {
    // TODO - In Solid at least, the slash must be present, so I question whether setting this as a container helps or hurts
    let resource: InstanceResource = this.resourceAccessor.getResource(context, toUrl(server, "/static/resource/resource-container-link-header"));
    assertTrue(resource.isExists());
    assertTrue(((ManageableResource) resource).isContainer());
  }

  // @Test, @DisplayName("Get an existing container")
  getExistingContainer(): void /* throws MalformedURLException, ShapeTreeException */ {
    let resource: InstanceResource = this.resourceAccessor.getResource(context, toUrl(server, "/static/resource/resource-container-link-header/"));
    assertTrue(resource.isExists());
    assertTrue(((ManageableResource) resource).isContainer());
  }

  // @Test, @DisplayName("Fail to lookup invalid resource attributes")
  failToLookupInvalidAttributes(): void /* throws MalformedURLException, ShapeTreeException */ {
    let resource: InstanceResource = this.resourceAccessor.getResource(context, toUrl(server, "/static/resource/resource-container-link-header"));
    assertTrue(resource.isExists());
    Assertions.assertNull(resource.getAttributes().firstValue("invalid").orElse(null));
  }

  // @Test, @DisplayName("Get a missing resource")
  getMissingResource(): void /* throws MalformedURLException, ShapeTreeException */ {
    let resource: InstanceResource = this.resourceAccessor.getResource(context, toUrl(server, "/static/resource/notpresent"));
    Assertions.assertEquals("", resource.getBody());
    // TODO - what other tests and assertions should be included here? isExists()?
  }

  // @Test, @DisplayName("Get a container with an invalid link type header")
  getContainerWithInvalidLinkTypeHeader(): void /* throws MalformedURLException, ShapeTreeException */ {
    // TODO - at the moment we process this happily. Aside from not marking it as a container, should there be a more severe handling?
    let resource: InstanceResource = this.resourceAccessor.getResource(context, toUrl(server, "/static/resource/resource-container-invalid-link-header/"));
    assertTrue(resource.isExists());
    assertFalse(((ManageableResource) resource).isContainer());
  }

  // @Test, @DisplayName("Fail to update resource")
  failToUpdateResource(): void /* throws MalformedURLException, ShapeTreeException */ {
    // Succeed in getting a resource
    let resource: InstanceResource = this.resourceAccessor.getResource(context, toUrl(server, "/static/resource/resource-container-link-header/"));
    // Fail to update it
    let response: DocumentResponse = this.resourceAccessor.updateResource(context, "PUT", resource, "BODY");
    assertFalse(response.isExists());
  }

  private getMilestoneThreeBodyGraph(): string {
    return "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \n" + "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \n" + "PREFIX xml: <http://www.w3.org/XML/1998/namespace> \n" + "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> \n" + "PREFIX ex: <http://www.example.com/ns/ex#> \n" + "\n" + "\n" + "<#milestone> \n" + "    ex:uri </static/resource/managed-container-1/managed-resource-1/#milestone> ; \n" + "    ex:id 12345 ; \n" + "    ex:name \"Milestone 3 of Project 1\" ; \n" + "    ex:created_at \"2021-04-04T20:15:47.000Z\"^^xsd:dateTime ; \n" + "    ex:target \"2021-06-05T20:15:47.000Z\"^^xsd:dateTime ; \n" + "    ex:inProject </static/resource/managed-container-1/#project> . \n";
  }

  private getProjectTwoManagerGraph(): string {
    return "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \n" + "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \n" + "PREFIX xml: <http://www.w3.org/XML/1998/namespace> \n" + "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> \n" + "PREFIX ex: <http://www.example.com/ns/ex#> \n" + "\n" + "<> \n" + "    a st:Manager ; \n" + "    st:hasAssignment <#ln1> . \n" + "\n" + "<#ln1> \n" + "    st:assigns <${SERVER_BASE}/static/shapetrees/project/shapetree#ProjectTree> ; \n" + "    st:manages </static/resource/managed-container-2/> ; \n" + "    st:hasRootAssignment <#ln1> ; \n" + "    st:focusNode </static/resource/managed-container-2/#project> ; \n" + "    st:shape <${SERVER_BASE}/static/shex/project/shex#ProjectShape> . \n";
  }
}
