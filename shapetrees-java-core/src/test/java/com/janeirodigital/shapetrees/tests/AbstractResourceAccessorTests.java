package com.janeirodigital.shapetrees.tests;

import com.janeirodigital.shapetrees.core.*;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.ShapeTreeContext;
import com.janeirodigital.shapetrees.core.ShapeTreeManager;
import com.janeirodigital.shapetrees.tests.fixtures.DispatcherEntry;
import com.janeirodigital.shapetrees.tests.fixtures.RequestMatchingFixtureDispatcher;
import lombok.SneakyThrows;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.List;

import static com.janeirodigital.shapetrees.core.ManageableInstance.createInstance;
import static com.janeirodigital.shapetrees.core.ManageableInstance.getInstance;
import static com.janeirodigital.shapetrees.tests.fixtures.MockWebServerHelper.toUrl;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class AbstractResourceAccessorTests {

    protected ResourceAccessor resourceAccessor = null;
    protected final ShapeTreeContext context;
    protected static MockWebServer server = null;
    protected static RequestMatchingFixtureDispatcher dispatcher = null;

    public AbstractResourceAccessorTests() {
        this.context = new ShapeTreeContext(null);
    }

    @BeforeAll
    static void beforeAll() {
        dispatcher = new RequestMatchingFixtureDispatcher(List.of(
                new DispatcherEntry(List.of("resourceAccessor/resource-no-link-headers"), "GET", "/static/resource/resource-no-link-headers", null),
                new DispatcherEntry(List.of("resourceAccessor/resource-empty-link-header"), "GET", "/static/resource/resource-empty-link-header", null),
                new DispatcherEntry(List.of("resourceAccessor/resource-container-link-header"), "GET", "/static/resource/resource-container-link-header", null),
                new DispatcherEntry(List.of("resourceAccessor/resource-container-link-header"), "GET", "/static/resource/resource-container-link-header/", null),
                new DispatcherEntry(List.of("resourceAccessor/resource-container-invalid-link-header"), "GET", "/static/resource/resource-container-invalid-link-header/", null),
                new DispatcherEntry(List.of("resourceAccessor/managed-container-1"), "GET", "/static/resource/managed-container-1/", null),
                new DispatcherEntry(List.of("resourceAccessor/managed-resource-1-create"), "PUT", "/static/resource/managed-container-1/managed-resource-1/", null),
                new DispatcherEntry(List.of("resourceAccessor/managed-resource-1-manager"), "GET", "/static/resource/managed-container-1/managed-resource-1/.shapetree", null),
                new DispatcherEntry(List.of("resourceAccessor/managed-container-1-manager"), "GET", "/static/resource/managed-container-1/.shapetree", null),
                new DispatcherEntry(List.of("resourceAccessor/unmanaged-container-2"), "GET", "/static/resource/unmanaged-container-2/", null),
                new DispatcherEntry(List.of("resourceAccessor/managed-container-2"), "GET", "/static/resource/managed-container-2/", null),
                new DispatcherEntry(List.of("resourceAccessor/unmanaged-resource-1-create"), "PUT", "/static/resource/unmanaged-resource-1", null),
                new DispatcherEntry(List.of("resourceAccessor/managed-container-2-manager-create"), "PUT", "/static/resource/managed-container-2/.shapetree", null),
                new DispatcherEntry(List.of("errors/404"), "GET", "/static/resource/missing-resource-1.shapetree", null),
                new DispatcherEntry(List.of("errors/404"), "GET", "/static/resource/missing-resource-2", null),
                new DispatcherEntry(List.of("resourceAccessor/missing-resource-2-manager-create"), "PUT", "/static/resource/missing-resource-2.shapetree", null),
                new DispatcherEntry(List.of("shapetrees/project-shapetree-ttl"), "GET", "/static/shapetrees/project/shapetree", null),
                new DispatcherEntry(List.of("schemas/project-shex"), "GET", "/static/shex/project/shex", null),
                new DispatcherEntry(List.of("errors/404"), "GET", "/static/resource/notpresent", null)
        ));
        server = new MockWebServer();
        server.setDispatcher(dispatcher);
    }

    // Tests to Get ManageableInstances

    @Test
    @SneakyThrows
    @DisplayName("Get instance from missing resource")
    void getInstanceFromMissingResource() {
        ManageableInstance instance = getInstance(this.resourceAccessor, context, toUrl(server, "/static/resource/notpresent"));
        Assertions.assertTrue(instance.getManageableResource() instanceof MissingManageableResource);
        Assertions.assertTrue(instance.getManagerResource() instanceof MissingManagerResource);
        Assertions.assertFalse(instance.isManaged());
        Assertions.assertEquals(instance.getManageableResource().getUrl(), toUrl(server, "/static/resource/notpresent"));
        Assertions.assertFalse(instance.getManageableResource().isExists());
        Assertions.assertFalse(instance.getManagerResource().isExists());
    }

    @Test
    @SneakyThrows
    @DisplayName("Get instance from managed resource")
    void getInstanceFromManagedResource() {
        // If the resource is Manageable - determine if it is managed by getting manager
        // Get and store a ManagedResource in instance - Manager exists - store manager in instance too
        ManageableInstance instance = getInstance(this.resourceAccessor, context, toUrl(server, "/static/resource/managed-container-1/"));

        Assertions.assertTrue(instance.getManageableResource() instanceof ManagedResource);
        Assertions.assertNotNull(instance.getManagerResource());
        Assertions.assertFalse(instance.getManagerResource() instanceof MissingManagerResource);
        Assertions.assertTrue(instance.isManaged());
        Assertions.assertFalse(instance.isUnmanaged());

        ManagerResource managerResource = instance.getManagerResource();
        ShapeTreeManager manager = managerResource.getManager();
        Assertions.assertEquals(1, manager.getAssignments().size());
    }

    @Test
    @SneakyThrows
    @DisplayName("Get instance for managed resource from manager request")
    void getInstanceFromManagedResourceFromManager() {
        ManageableInstance instance = getInstance(this.resourceAccessor, context, toUrl(server, "/static/resource/managed-container-1/.shapetree"));
        Assertions.assertNotNull(instance.getManagerResource());
        Assertions.assertFalse(instance.getManagerResource() instanceof MissingManagerResource);
        Assertions.assertTrue(instance.getManagerResource().isExists());
        Assertions.assertTrue(instance.getManageableResource() instanceof ManagedResource);
        Assertions.assertEquals(instance.getManagerResource().getManagedResourceUrl(), instance.getManageableResource().getUrl());
    }

    @Test
    @SneakyThrows
    @DisplayName("Fail to get instance for missing resource from manager request")
    void failToGetInstanceForMissingManageableResourceFromManager() {
        // Note that in this request, the manager is also non-existent
        Assertions.assertThrows(ShapeTreeException.class, () -> {
            ManageableInstance instance = getInstance(this.resourceAccessor, context, toUrl(server, "/static/resource/missing-resource-1.shapetree"));
        });
    }

    @Test
    @SneakyThrows
    @DisplayName("Get instance from unmanaged resource")
    void getInstanceFromUnmanagedResource() {
        // If the resource is Manageable - determine if it is managed by getting manager
        // Get and store an UnmanagedResource in instance - No manager exists - store the location of the manager url
        ManageableInstance instance = getInstance(this.resourceAccessor, context, toUrl(server, "/static/resource/unmanaged-container-2/"));
        Assertions.assertTrue(instance.getManageableResource() instanceof UnmanagedResource);
        Assertions.assertTrue(instance.getManagerResource() instanceof MissingManagerResource);
        Assertions.assertTrue(instance.isUnmanaged());
        Assertions.assertFalse(instance.isManaged());
    }

    @Test
    @SneakyThrows
    @DisplayName("Get instance from unmanaged resource from manager request")
    void getInstanceFromUnmanagedResourceFromManager() {
        // Manager resource doesn't exist. Unmanaged resource associated with it does exist
        ManageableInstance instance = getInstance(this.resourceAccessor, context, toUrl(server, "/static/resource/unmanaged-container-2/.shapetree"));
        Assertions.assertTrue(instance.getManageableResource() instanceof UnmanagedResource);
        Assertions.assertTrue(instance.getManagerResource() instanceof MissingManagerResource);
        Assertions.assertTrue(instance.wasRequestForManager());
        Assertions.assertTrue(instance.isUnmanaged());
        Assertions.assertFalse(instance.isManaged());
    }

    // Tests to Create ManageableInstances
    @Test
    @SneakyThrows
    @DisplayName("Create instance from managed resource")
    void createInstanceFromManagedResource() {
        ResourceAttributes headers = new ResourceAttributes();
        ManageableInstance instance = createInstance(this.resourceAccessor, context, "PUT", toUrl(server, "/static/resource/managed-container-1/managed-resource-1/"), headers, getMilestoneThreeBodyGraph(), "text/turtle");
        Assertions.assertTrue(instance.isManaged());
        Assertions.assertTrue(instance.getManageableResource() instanceof ManagedResource);
        Assertions.assertFalse(instance.getManageableResource() instanceof MissingManageableResource);
        Assertions.assertTrue(instance.getManagerResource().isExists());
        Assertions.assertEquals(instance.getManagerResource().getManagedResourceUrl(), instance.getManageableResource().getUrl());

        ManagerResource managerResource = instance.getManagerResource();
        ShapeTreeManager manager = managerResource.getManager();
        Assertions.assertEquals(1, manager.getAssignments().size());
    }

    @Test
    @SneakyThrows
    @DisplayName("Create instance from unmanaged resource")
    void createInstanceFromUnmanagedResource() {
        ResourceAttributes headers = new ResourceAttributes();
        ManageableInstance instance = createInstance(this.resourceAccessor, context, "PUT", toUrl(server, "/static/resource/unmanaged-resource-1"), headers, "<#a> <#b> <#c>", "text/turtle");
        Assertions.assertTrue(instance.isUnmanaged());
        Assertions.assertTrue(instance.getManageableResource() instanceof UnmanagedResource);
        Assertions.assertTrue(instance.getManagerResource() instanceof MissingManagerResource);
    }

    @Test
    @SneakyThrows
    @DisplayName("Fail to create instance from existing manageable resource")
    void failToCreateInstanceFromExistingResource() {
        // Resource exists - ERROR - can't create a manageable resource when one already exists
        ResourceAttributes headers = new ResourceAttributes(); // May need to populate this
        Assertions.assertThrows(ShapeTreeException.class, () -> {
            ManageableInstance instance = createInstance(this.resourceAccessor, context, "PUT", toUrl(server, "/static/resource/unmanaged-container-2/"), headers, "<#a> <#b> <#c>", "text/turtle");
        });
    }

    @Test
    @SneakyThrows
    @DisplayName("Create instance from manager resource")
    void createInstanceFromManagerResource() {
        // Create a new manager and store in instance and load the managed resource and store in instance (possibly just pre-fetch metadata if lazily loading)
        ResourceAttributes headers = new ResourceAttributes();
        ManageableInstance instance = createInstance(this.resourceAccessor, context, "PUT", toUrl(server, "/static/resource/managed-container-2/.shapetree"), headers, getProjectTwoManagerGraph(), "text/turtle");
        Assertions.assertTrue(instance.isManaged());
        Assertions.assertTrue(instance.getManageableResource() instanceof ManagedResource);
        Assertions.assertFalse(instance.getManagerResource() instanceof MissingManagerResource);
        // Probably need some additional tests
    }

    @Test
    @DisplayName("Fail to create instance from isolated manager resource")
    void failToCreateInstanceFromIsolatedManagerResource() {
        ResourceAttributes headers = new ResourceAttributes();
        Assertions.assertThrows(ShapeTreeException.class, () -> {
            ManageableInstance instance = createInstance(this.resourceAccessor, context, "PUT", toUrl(server, "/static/resource/missing-resource-2.shapetree"), headers, getProjectTwoManagerGraph(), "text/turtle");
        });
    }

    // TODO - currently missing dedicated tests for create and delete. only one test for update (which is a failure test)

    @Test
    @DisplayName("Get a resource without any link headers")
    void getResourceWithNoLinkHeaders() throws MalformedURLException, ShapeTreeException {
        InstanceResource resource = this.resourceAccessor.getResource(this.context, toUrl(server, "/static/resource/resource-no-link-headers"));
        // This is a strange way to check whether something has no link headers
        assertTrue(resource.isExists());
        assertTrue(((ManageableResource) resource).getManagerResourceUrl().isEmpty());
    }

    @Test
    @DisplayName("Get a resource with an empty link header")
    void getResourceWithEmptyLinkHeader() throws MalformedURLException, ShapeTreeException {
        // Link header is present but has nothing in it
        InstanceResource resource = this.resourceAccessor.getResource(context, toUrl(server, "/static/resource/resource-empty-link-header"));
        assertTrue(resource.isExists());
        Assertions.assertTrue(((ManageableResource) resource).getManagerResourceUrl().isEmpty());
    }

    @Test
    @DisplayName("Fail to get a resource with an invalid URL string")
    void failToAccessResourceWithInvalidUrlString() throws MalformedURLException, ShapeTreeException { // TODO: Test: may as well deleted as it's only testing URL.create()
        Assertions.assertThrows(MalformedURLException.class, () -> this.resourceAccessor.getResource(context, new URL(":invalid")));
        // TODO - this should also test create, update, delete, getContained, (also get/create instance)
    }

    @Test
    @DisplayName("Get a missing resource with no slash")
    void getMissingResourceWithNoSlash() throws MalformedURLException, ShapeTreeException {
        InstanceResource resource = this.resourceAccessor.getResource(context, toUrl(server, "/static/resource/not-existing-no-slash"));
        assertFalse(resource.isExists());
        assertFalse(((ManageableResource) resource).isContainer());
    }

    @Test
    @DisplayName("Get a missing container with slash")
    void getMissingContainerWithSlash() throws MalformedURLException, ShapeTreeException {
        InstanceResource resource = this.resourceAccessor.getResource(context, toUrl(server, "/static/resource/not-existing-slash/"));
        assertFalse(resource.isExists());
        assertTrue(((ManageableResource) resource).isContainer());
    }

    @Test
    @DisplayName("Get a missing container with slash and fragment")
    void getMissingContainerWithSlashAndFragment() throws MalformedURLException, ShapeTreeException {
        InstanceResource resource = this.resourceAccessor.getResource(context, toUrl(server, "/static/resource/not-existing-slash/#withfragment"));
        assertFalse(resource.isExists());
        assertTrue(((ManageableResource) resource).isContainer());
    }

    @Test
    @DisplayName("Get an existing container with no slash")
    void getExistingContainerNoSlash() throws MalformedURLException, ShapeTreeException {
        // TODO - In Solid at least, the slash must be present, so I question whether setting this as a container helps or hurts
        InstanceResource resource = this.resourceAccessor.getResource(context, toUrl(server, "/static/resource/resource-container-link-header"));
        assertTrue(resource.isExists());
        assertTrue(((ManageableResource) resource).isContainer());
    }

    @Test
    @DisplayName("Get an existing container")
    void getExistingContainer() throws MalformedURLException, ShapeTreeException {
        InstanceResource resource = this.resourceAccessor.getResource(context, toUrl(server, "/static/resource/resource-container-link-header/"));
        assertTrue(resource.isExists());
        assertTrue(((ManageableResource) resource).isContainer());
    }

    @Test
    @DisplayName("Fail to lookup invalid resource attributes")
    void failToLookupInvalidAttributes() throws MalformedURLException, ShapeTreeException {
        InstanceResource resource = this.resourceAccessor.getResource(context, toUrl(server, "/static/resource/resource-container-link-header"));
        assertTrue(resource.isExists());
        Assertions.assertNull(resource.getAttributes().firstValue("invalid").orElse(null));
    }

    @Test
    @DisplayName("Get a missing resource")
    void getMissingResource() throws MalformedURLException, ShapeTreeException {
        InstanceResource resource = this.resourceAccessor.getResource(context, toUrl(server, "/static/resource/notpresent"));
        Assertions.assertEquals("", resource.getBody());
        // TODO - what other tests and assertions should be included here? isExists()?
    }

    @Test
    @DisplayName("Get a container with an invalid link type header")
    void getContainerWithInvalidLinkTypeHeader() throws MalformedURLException, ShapeTreeException {
        // TODO - at the moment we process this happily. Aside from not marking it as a container, should there be a more severe handling?
        InstanceResource resource = this.resourceAccessor.getResource(context, toUrl(server, "/static/resource/resource-container-invalid-link-header/"));
        assertTrue(resource.isExists());
        assertFalse(((ManageableResource) resource).isContainer());
    }

    @Test
    @DisplayName("Fail to update resource")
    void failToUpdateResource() throws MalformedURLException, ShapeTreeException {
        // Succeed in getting a resource
        InstanceResource resource =  this.resourceAccessor.getResource(context, toUrl(server, "/static/resource/resource-container-link-header/"));
        // Fail to update it
        DocumentResponse response = this.resourceAccessor.updateResource(context, "PUT", resource, "BODY");
        assertFalse(response.isExists());
    }

    private String getMilestoneThreeBodyGraph() {
        return "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \n" +
                "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \n" +
                "PREFIX xml: <http://www.w3.org/XML/1998/namespace> \n" +
                "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> \n" +
                "PREFIX ex: <http://www.example.com/ns/ex#> \n" +
                "\n" +
                "\n" +
                "<#milestone> \n" +
                "    ex:uri </static/resource/managed-container-1/managed-resource-1/#milestone> ; \n" +
                "    ex:id 12345 ; \n" +
                "    ex:name \"Milestone 3 of Project 1\" ; \n" +
                "    ex:created_at \"2021-04-04T20:15:47.000Z\"^^xsd:dateTime ; \n" +
                "    ex:target \"2021-06-05T20:15:47.000Z\"^^xsd:dateTime ; \n" +
                "    ex:inProject </static/resource/managed-container-1/#project> . \n";
    }

    private String getProjectTwoManagerGraph() {
        return "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \n" +
                "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \n" +
                "PREFIX xml: <http://www.w3.org/XML/1998/namespace> \n" +
                "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> \n" +
                "PREFIX ex: <http://www.example.com/ns/ex#> \n" +
                "\n" +
                "<> \n" +
                "    a st:Manager ; \n" +
                "    st:hasAssignment <#ln1> . \n" +
                "\n" +
                "<#ln1> \n" +
                "    st:assigns <${SERVER_BASE}/static/shapetrees/project/shapetree#ProjectTree> ; \n" +
                "    st:manages </static/resource/managed-container-2/> ; \n" +
                "    st:hasRootAssignment <#ln1> ; \n" +
                "    st:focusNode </static/resource/managed-container-2/#project> ; \n" +
                "    st:shape <${SERVER_BASE}/static/shex/project/shex#ProjectShape> . \n" ;
    }

}
