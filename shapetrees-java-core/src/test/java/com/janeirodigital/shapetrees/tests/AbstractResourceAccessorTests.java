package com.janeirodigital.shapetrees.tests;

import com.janeirodigital.shapetrees.core.*;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.tests.fixtures.RequestMatchingFixtureDispatcher;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.net.MalformedURLException;
import java.net.URL;

import static com.janeirodigital.shapetrees.core.ManageableInstance.createInstanceResource;
import static com.janeirodigital.shapetrees.core.ManageableInstance.getInstance;
import static com.janeirodigital.shapetrees.tests.fixtures.DispatcherHelper.mockOnGet;
import static com.janeirodigital.shapetrees.tests.fixtures.DispatcherHelper.mockOnPut;
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
    static void initializeDispatcher() {
        dispatcher = new RequestMatchingFixtureDispatcher();
        // Add fixture for shapetree resource containing shapetrees used in these tests
        mockOnGet(dispatcher, "/static/shapetrees/project/shapetree", "shapetrees/project-shapetree-ttl");
        // Add fixture for shape resource containing shapes used by the above shape trees
        mockOnGet(dispatcher, "/static/shex/project/shex", "schemas/project-shex");
        server = new MockWebServer();
        server.setDispatcher(dispatcher);
    }

    @Test
    @DisplayName("Get instance from missing resource")
    void getInstanceFromMissingResource() throws ShapeTreeException {
        mockOnGet(dispatcher, "/notpresent", "http/404");
        
        ManageableInstance instance = getInstance(this.resourceAccessor, context, toUrl(server, "/notpresent"));
        Assertions.assertTrue(instance.getManageableResource() instanceof MissingManageableResource);
        Assertions.assertTrue(instance.getManagerResource() instanceof MissingManagerResource);
        Assertions.assertFalse(instance.isManaged());
        Assertions.assertEquals(instance.getManageableResource().getUrl(), toUrl(server, "/notpresent"));
        Assertions.assertFalse(instance.getManageableResource().isExists());
        Assertions.assertFalse(instance.getManagerResource().isExists());
    }

    @Test
    @DisplayName("Get instance from managed resource")
    void getInstanceFromManagedResource() throws ShapeTreeException {
        mockOnGet(dispatcher, "/managed-container-1/", "resourceAccessor/managed-container-1");
        mockOnGet(dispatcher, "/managed-container-1/.shapetree", "resourceAccessor/managed-container-1-manager");
        // If the resource is Manageable - determine if it is managed by getting manager
        // Get and store a ManagedResource in instance - Manager exists - store manager in instance too
        ManageableInstance instance = getInstance(this.resourceAccessor, context, toUrl(server, "/managed-container-1/"));

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
    @DisplayName("Get instance for managed resource from manager request")
    void getInstanceFromManagedResourceFromManager() throws ShapeTreeException {
        mockOnGet(dispatcher, "/managed-container-1/", "resourceAccessor/managed-container-1");
        mockOnGet(dispatcher, "/managed-container-1/.shapetree", "resourceAccessor/managed-container-1-manager");

        ManageableInstance instance = getInstance(this.resourceAccessor, context, toUrl(server, "/managed-container-1/.shapetree"));
        Assertions.assertNotNull(instance.getManagerResource());
        Assertions.assertFalse(instance.getManagerResource() instanceof MissingManagerResource);
        Assertions.assertTrue(instance.getManagerResource().isExists());
        Assertions.assertTrue(instance.getManageableResource() instanceof ManagedResource);
        Assertions.assertEquals(instance.getManagerResource().getManagedResourceUrl(), instance.getManageableResource().getUrl());
    }

    @Test
    @DisplayName("Fail to get instance for missing resource from manager request")
    void failToGetInstanceForMissingManageableResourceFromManager() {
        mockOnGet(dispatcher, "/missing-resource-1.shapetree", "http/404");
        
        // Note that in this request, the manager is also non-existent
        Assertions.assertThrows(ShapeTreeException.class, () -> {
            ManageableInstance instance = getInstance(this.resourceAccessor, context, toUrl(server, "/missing-resource-1.shapetree"));
        });
    }

    @Test
    @DisplayName("Get instance from unmanaged resource")
    void getInstanceFromUnmanagedResource() throws ShapeTreeException {
        mockOnGet(dispatcher, "/unmanaged-container-2/", "resourceAccessor/unmanaged-container-2");
        // If the resource is Manageable - determine if it is managed by getting manager
        // Get and store an UnmanagedResource in instance - No manager exists - store the location of the manager url

        ManageableInstance instance = getInstance(this.resourceAccessor, context, toUrl(server, "/unmanaged-container-2/"));
        Assertions.assertTrue(instance.getManageableResource() instanceof UnmanagedResource);
        Assertions.assertTrue(instance.getManagerResource() instanceof MissingManagerResource);
        Assertions.assertTrue(instance.isUnmanaged());
        Assertions.assertFalse(instance.isManaged());
    }

    @Test
    @DisplayName("Get instance from unmanaged resource from manager request")
    void getInstanceFromUnmanagedResourceFromManager() throws ShapeTreeException {
        mockOnGet(dispatcher, "/unmanaged-container-2/", "resourceAccessor/unmanaged-container-2");
        // Manager resource doesn't exist. Unmanaged resource associated with it does exist

        ManageableInstance instance = getInstance(this.resourceAccessor, context, toUrl(server, "/unmanaged-container-2/.shapetree"));
        Assertions.assertTrue(instance.getManageableResource() instanceof UnmanagedResource);
        Assertions.assertTrue(instance.getManagerResource() instanceof MissingManagerResource);
        Assertions.assertTrue(instance.wasRequestForManager());
        Assertions.assertTrue(instance.isUnmanaged());
        Assertions.assertFalse(instance.isManaged());
    }

    @Test
    @DisplayName("Create instance from managed resource")
    void createInstanceFromManagedResource() throws ShapeTreeException {
        mockOnPut(dispatcher, "/managed-container-1/managed-resource-1/", "http/201");
        mockOnGet(dispatcher, "/managed-container-1/managed-resource-1/", "resourceAccessor/managed-resource-1-get");
        mockOnGet(dispatcher, "/managed-container-1/managed-resource-1/.shapetree", "resourceAccessor/managed-resource-1-manager");
        
        ManageableInstance instance = createInstanceResource(this.resourceAccessor, context, toUrl(server, "/managed-container-1/managed-resource-1/"), null, getMilestoneThreeBodyGraph(), "text/turtle");
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
    @DisplayName("Create instance from unmanaged resource")
    void createInstanceFromUnmanagedResource() throws ShapeTreeException {
        mockOnPut(dispatcher, "/unmanaged-resource-1", "http/201");
        mockOnGet(dispatcher, "/unmanaged-resource-1", "resourceAccessor/unmanaged-resource-1-get");

        ManageableInstance instance = createInstanceResource(this.resourceAccessor, context, toUrl(server, "/unmanaged-resource-1"), null, "<#a> <#b> <#c>", "text/turtle");
        Assertions.assertTrue(instance.isUnmanaged());
        Assertions.assertTrue(instance.getManageableResource() instanceof UnmanagedResource);
        Assertions.assertTrue(instance.getManagerResource() instanceof MissingManagerResource);
    }

    @Test
    @DisplayName("Fail to create instance from existing manageable resource")
    void failToCreateInstanceFromExistingResource() {
        mockOnPut(dispatcher, "/unmanaged-container-2/", "http/412");
        // Resource exists - ERROR - can't create a manageable resource when one already exists
        Assertions.assertThrows(ShapeTreeException.class, () -> {
            ManageableInstance instance = createInstanceResource(this.resourceAccessor, context, toUrl(server, "/unmanaged-container-2/"), null, "<#a> <#b> <#c>", "text/turtle");
        });
    }

    @Test
    @DisplayName("Create instance from manager resource")
    void createInstanceFromManagerResource() throws ShapeTreeException {

        mockOnGet(dispatcher, "/managed-container-2/", "resourceAccessor/managed-container-2");
        mockOnGet(dispatcher, "/managed-container-2/.shapetree", "resourceAccessor/managed-container-2-manager-get");
        mockOnPut(dispatcher, "/managed-container-2/.shapetree", "http/201");

        // Create a new manager and store in instance and load the managed resource and store in instance
        ManageableInstance instance = createInstanceResource(this.resourceAccessor, context, toUrl(server, "/managed-container-2/.shapetree"), null, getProjectTwoManagerGraph(), "text/turtle");
        Assertions.assertTrue(instance.isManaged());
        Assertions.assertTrue(instance.getManageableResource() instanceof ManagedResource);
        Assertions.assertFalse(instance.getManagerResource() instanceof MissingManagerResource);
        // Probably need some additional tests
    }

    @Test
    @DisplayName("Fail to create instance from isolated manager resource")
    void failToCreateInstanceFromIsolatedManagerResource() {
        mockOnGet(dispatcher, "/missing-resource-2.shapetree", "resourceAccessor/missing-resource-2-manager-get");
        mockOnPut(dispatcher, "/missing-resource-2.shapetree", "http/201");

        Assertions.assertThrows(ShapeTreeException.class, () -> {
            ManageableInstance instance = createInstanceResource(this.resourceAccessor, context, toUrl(server, "/missing-resource-2.shapetree"), null, getProjectTwoManagerGraph(), "text/turtle");
        });
    }

    // TODO - currently missing dedicated tests for create and delete. only one test for update (which is a failure test)

    @Test
    @DisplayName("Get a resource without any link headers")
    void getResourceWithNoLinkHeaders() throws ShapeTreeException {
        mockOnGet(dispatcher, "/resource-no-link-headers", "resourceAccessor/resource-no-link-headers");
        InstanceResource resource = this.resourceAccessor.getResource(this.context, toUrl(server, "/resource-no-link-headers"));
        // This is a strange way to check whether something has no link headers
        assertTrue(resource.isExists());
        assertTrue(((ManageableResource) resource).getManagerResourceUrl().isEmpty());
    }

    @Test
    @DisplayName("Get a resource with an empty link header")
    void getResourceWithEmptyLinkHeader() throws ShapeTreeException {
        mockOnGet(dispatcher, "/resource-empty-link-header", "resourceAccessor/resource-empty-link-header");
        // Link header is present but has nothing in it
        InstanceResource resource = this.resourceAccessor.getResource(context, toUrl(server, "/resource-empty-link-header"));
        assertTrue(resource.isExists());
        Assertions.assertTrue(((ManageableResource) resource).getManagerResourceUrl().isEmpty());
    }

    @Test
    @DisplayName("Fail to get a resource with an invalid URL string")
    void failToAccessResourceWithInvalidUrlString() { // TODO: Test: may as well deleted as it's only testing URL.create()
        Assertions.assertThrows(MalformedURLException.class, () -> this.resourceAccessor.getResource(context, new URL(":invalid")));
        // TODO - this should also test create, update, delete, getContained, (also get/create instance)
    }

    @Test
    @DisplayName("Get a missing resource with no slash")
    void getMissingResourceWithNoSlash() throws ShapeTreeException {
        InstanceResource resource = this.resourceAccessor.getResource(context, toUrl(server, "/not-existing-no-slash"));
        assertFalse(resource.isExists());
        assertTrue(resource instanceof MissingManageableResource);
        assertFalse(((ManageableResource) resource).isContainer());
    }

    @Test
    @DisplayName("Get a missing container with slash")
    void getMissingContainerWithSlash() throws MalformedURLException, ShapeTreeException {
        InstanceResource resource = this.resourceAccessor.getResource(context, toUrl(server, "/not-existing-slash/"));
        assertFalse(resource.isExists());
        assertTrue(resource instanceof MissingManageableResource);
        assertFalse(((ManageableResource) resource).isContainer());
    }

    @Test
    @DisplayName("Get a missing container with slash and fragment")
    void getMissingContainerWithSlashAndFragment() throws MalformedURLException, ShapeTreeException {
        InstanceResource resource = this.resourceAccessor.getResource(context, toUrl(server, "/not-existing-slash/#withfragment"));
        assertFalse(resource.isExists());
        assertFalse(((ManageableResource) resource).isContainer());
        assertTrue(resource instanceof MissingManageableResource);
    }

    @Test
    @DisplayName("Get an existing container with no slash")
    void getExistingContainerNoSlash() throws ShapeTreeException {
        mockOnGet(dispatcher, "/resource-container-link-header", "resourceAccessor/resource-container-link-header");
        InstanceResource resource = this.resourceAccessor.getResource(context, toUrl(server, "/resource-container-link-header"));
        assertTrue(resource.isExists());
        assertTrue(((ManageableResource) resource).isContainer());
    }

    @Test
    @DisplayName("Get an existing container")
    void getExistingContainer() throws ShapeTreeException {
        mockOnGet(dispatcher, "/resource-container-link-header/", "resourceAccessor/resource-container-link-header");
        InstanceResource resource = this.resourceAccessor.getResource(context, toUrl(server, "/resource-container-link-header/"));
        assertTrue(resource.isExists());
        assertTrue(((ManageableResource) resource).isContainer());
    }

    @Test
    @DisplayName("Fail to lookup invalid resource attributes")
    void failToLookupInvalidAttributes() throws ShapeTreeException {
        mockOnGet(dispatcher, "/resource-container-link-header", "resourceAccessor/resource-container-link-header");
        InstanceResource resource = this.resourceAccessor.getResource(context, toUrl(server, "/resource-container-link-header"));
        assertTrue(resource.isExists());
        Assertions.assertNull(resource.getAttributes().firstValue("invalid").orElse(null));
    }

    @Test
    @DisplayName("Get a missing resource")
    void getMissingResource() throws ShapeTreeException {
        mockOnGet(dispatcher, "/notpresent", "http/404");
        InstanceResource resource = this.resourceAccessor.getResource(context, toUrl(server, "/notpresent"));
        Assertions.assertEquals("", resource.getBody());
        Assertions.assertTrue(resource instanceof MissingManageableResource);
    }

    @Test
    @DisplayName("Get a container with an invalid link type header")
    void getContainerWithInvalidLinkTypeHeader() throws ShapeTreeException {
        mockOnGet(dispatcher, "/resource-container-invalid-link-header/", "resourceAccessor/resource-container-invalid-link-header");
        InstanceResource resource = this.resourceAccessor.getResource(context, toUrl(server, "/resource-container-invalid-link-header/"));
        assertTrue(resource.isExists());
        assertFalse(((ManageableResource) resource).isContainer());
    }

    @Test
    @DisplayName("Fail to update resource")
    void failToUpdateResource() throws ShapeTreeException {
        mockOnGet(dispatcher, "/resource-container-link-header/", "resourceAccessor/resource-container-link-header");
        // Succeed in getting a resource
        InstanceResource resource =  this.resourceAccessor.getResource(context, toUrl(server, "/resource-container-link-header/"));
        // Fail to update it
        Assertions.assertThrows(ShapeTreeException.class, () -> {
            this.resourceAccessor.updateResource(context, resource, "BODY");
        });
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
                "    ex:uri </managed-container-1/managed-resource-1/#milestone> ; \n" +
                "    ex:id 12345 ; \n" +
                "    ex:name \"Milestone 3 of Project 1\" ; \n" +
                "    ex:created_at \"2021-04-04T20:15:47.000Z\"^^xsd:dateTime ; \n" +
                "    ex:target \"2021-06-05T20:15:47.000Z\"^^xsd:dateTime ; \n" +
                "    ex:inProject </managed-container-1/#project> . \n";
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
                "    st:manages </managed-container-2/> ; \n" +
                "    st:hasRootAssignment <#ln1> ; \n" +
                "    st:focusNode </managed-container-2/#project> ; \n" +
                "    st:shape <${SERVER_BASE}/static/shex/project/shex#ProjectShape> . \n" ;
    }

}
