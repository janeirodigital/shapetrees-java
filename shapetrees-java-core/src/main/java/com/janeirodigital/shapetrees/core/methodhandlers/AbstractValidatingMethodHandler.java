package com.janeirodigital.shapetrees.core.methodhandlers;

import com.janeirodigital.shapetrees.core.*;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.helpers.RequestHelper;
import com.janeirodigital.shapetrees.core.models.*;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.apache.jena.graph.Graph;

import java.io.IOException;
import java.io.Serializable;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.*;
/**
 * Abstract class providing reusable functionality to different method handlers
 */
@Slf4j
public abstract class AbstractValidatingMethodHandler {
    private static final String DELETE = "DELETE";
    protected final ResourceAccessor resourceAccessor;

    protected AbstractValidatingMethodHandler(ResourceAccessor resourceAccessor) {
        this.resourceAccessor = resourceAccessor;
    }

    protected DocumentResponse manageShapeTree(ShapeTreeInstance shapeTreeInstance, ShapeTreeRequest shapeTreeRequest) throws ShapeTreeException {

        Optional<DocumentResponse> validationResponse = null;
        ShapeTreeManager updatedRootManager = RequestHelper.getIncomingShapeTreeManager(shapeTreeRequest, shapeTreeInstance.getManagerResource());
        ShapeTreeManager existingRootManager = shapeTreeInstance.getManagerResource().getManager();

        // Determine assignments that have been removed, added, and/or updated
        ShapeTreeManagerDelta delta = ShapeTreeManagerDelta.evaluate(existingRootManager, updatedRootManager);

        // It is invalid for a manager resource to be left with no assignments.
        // Shape Trees, §3: A shape tree manager includes one or more shape tree assignments via st:hasAssignment.
        if (delta.allRemoved()) { ensureAllRemovedFromManagerByDelete(shapeTreeRequest); }

        if (delta.wasReduced()) {
            // An existing assignment has been removed from the manager for the managed resource.
            validationResponse = unplantShapeTree(shapeTreeInstance, shapeTreeInstance.getShapeTreeContext(), delta);
            if (validationResponse.isPresent()) { return validationResponse.get(); }
        }

        if (delta.isUpdated()) {
            // An existing assignment has been updated, or new assignments have been added
            validationResponse = plantShapeTree(shapeTreeInstance, shapeTreeInstance.getShapeTreeContext(), updatedRootManager, delta);
            if (validationResponse.isPresent()) { return validationResponse.get(); }
        }

        // TODO: Test: Need a test with reduce and updated delta to make sure we never return success from plant or unplant.

        return successfulValidation();
    }

    /**
     * Plants a shape tree on an existing resource
     * @param shapeTreeInstance
     * @param shapeTreeContext
     * @param updatedRootManager
     * @param delta
     * @return DocumentResponse
     * @throws IOException
     * @throws MalformedURLException
     */
    protected Optional<DocumentResponse> plantShapeTree(ShapeTreeInstance shapeTreeInstance, ShapeTreeContext shapeTreeContext, ShapeTreeManager updatedRootManager, ShapeTreeManagerDelta delta) throws ShapeTreeException {

        // Cannot directly update assignments that are not root locations
        ensureUpdatedAssignmentIsRoot(delta);

        // Run recursive assignment for each updated assignment in the root manager
        for (ShapeTreeAssignment rootAssignment : delta.getUpdatedAssignments()) {
            Optional<DocumentResponse> validationResponse = assignShapeTreeToResource(shapeTreeInstance, shapeTreeContext, updatedRootManager, rootAssignment, rootAssignment, null);
            if (validationResponse.isPresent()) { return validationResponse; }
        }

        return Optional.empty();
    }

    protected  Optional<DocumentResponse> unplantShapeTree(ShapeTreeInstance shapeTreeInstance, ShapeTreeContext shapeTreeContext, ShapeTreeManagerDelta delta) throws ShapeTreeException {

        ensureRemovedAssignmentsAreRoot(delta); // Cannot unplant a non-root location

        // Run recursive unassignment for each removed assignment in the updated root manager
        for (ShapeTreeAssignment rootAssignment : delta.getRemovedAssignments()) {
            Optional<DocumentResponse> validationResponse = unassignShapeTreeFromResource(shapeTreeInstance, shapeTreeContext, rootAssignment);
            if (validationResponse.isPresent()) { return validationResponse; }
        }

        return Optional.empty();
    }

    // TODO: #87: do sanity checks on meta of meta, c.f. @see https://github.com/xformativ/shapetrees-java/issues/87
    protected Optional<DocumentResponse> createShapeTreeInstance(ShapeTreeInstance shapeTreeInstance, ShapeTreeInstance containerResource, ShapeTreeRequest shapeTreeRequest, String proposedName) throws ShapeTreeException {
        // Sanity check user-owned resource @@ delete 'cause type checks
        ensureShapeTreeInstanceResourceExists(containerResource.getManagedResource(),"Target container for resource creation not found");
        ensureRequestResourceIsContainer(containerResource.getManagedResource(),"Cannot create a shape tree instance in a non-container resource");

        // Prepare the target resource for validation and creation
        URL targetResourceUrl = RequestHelper.normalizeSolidResourceUrl(containerResource.getManagedResource().getUrl(), proposedName, shapeTreeRequest.getResourceType());
        ensureTargetManagedResourceDoesNotExist(shapeTreeInstance.getShapeTreeContext(), targetResourceUrl,"Cannot create a shape tree instance in a non-container resource " + targetResourceUrl);

        ensureShapeTreeInstanceResourceExists(containerResource.getManagerResource(), "Should not be creating a shape tree instance on an unmanaged target container");

        ShapeTreeManager containerManager = containerResource.getManagerResource().getManager();
        ensureShapeTreeManagerExists(containerManager, "Cannot have a shape tree manager resource without a shape tree manager containing at least one shape tree assignment");

        // Get the shape tree associated that specifies what resources can be contained by the target container (st:contains)
        ShapeTreeAssignment containingAssignment = containerManager.getContainingAssignment();

        if (containingAssignment == null) {
            // If there is no containing shape tree for the target container, then the request is valid and can
            // be passed straight through
            return Optional.empty();
        }

        URL containerShapeTreeUrl = containingAssignment.getShapeTree();
        ShapeTree containerShapeTree = ShapeTreeFactory.getShapeTree(containerShapeTreeUrl);

        URL targetShapeTree = RequestHelper.getIncomingTargetShapeTree(shapeTreeRequest, targetResourceUrl);
        URL incomingFocusNode = RequestHelper.getIncomingFocusNode(shapeTreeRequest, targetResourceUrl);
        Graph incomingBodyGraph = RequestHelper.getIncomingBodyGraph(shapeTreeRequest, targetResourceUrl, null);

        ValidationResult validationResult = containerShapeTree.validateContainedResource(proposedName, shapeTreeRequest.getResourceType(), targetShapeTree, incomingBodyGraph, incomingFocusNode);
        if (Boolean.FALSE.equals(validationResult.isValid())) {
            return failValidation(validationResult);
        }

        log.debug("Creating shape tree instance at {}", targetResourceUrl);

        ShapeTreeInstance createdResource = new ShapeTreeInstance(targetResourceUrl, this.resourceAccessor, shapeTreeInstance.getShapeTreeContext(), shapeTreeRequest);

        ShapeTreeAssignment rootShapeTreeAssignment = getRootAssignment(shapeTreeInstance.getShapeTreeContext(), containingAssignment);
        ensureAssignmentExists(rootShapeTreeAssignment, "Unable to find root shape tree assignment at " + containingAssignment.getRootAssignment());

        log.debug("Assigning shape tree to created resource: {}", createdResource.getManagerResource().getUrl());
        // Note: By providing the positive advance validationResult, we let the assignment operation know that validation
        // has already been performed with a positive result, and avoid having it perform the validation a second time
        Optional<DocumentResponse> assignResult = assignShapeTreeToResource(createdResource, shapeTreeInstance.getShapeTreeContext(), null, rootShapeTreeAssignment, containingAssignment, validationResult);
        if (assignResult.isPresent()) { return assignResult; }

        return Optional.of(successfulValidation());
    }

    protected Optional<DocumentResponse> updateShapeTreeInstance(ShapeTreeInstance targetResource, ShapeTreeContext shapeTreeContext, ShapeTreeRequest shapeTreeRequest) throws ShapeTreeException {


        ensureShapeTreeInstanceResourceExists(targetResource.getManagedResource(),"Target resource to update not found");
        ensureShapeTreeInstanceResourceExists(targetResource.getManagerResource(), "Should not be updating an unmanaged resource as a shape tree instance");

        ShapeTreeManager manager = targetResource.getManagerResource().getManager();
        ensureShapeTreeManagerExists(manager, "Cannot have a shape tree manager resource without a shape tree manager with at least one shape tree assignment");

        for (ShapeTreeAssignment assignment : manager.getAssignments()) {

            // Evaluate the update against each ShapeTreeAssignment managing the resource.
            // All must pass for the update to validate
            ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(assignment.getShapeTree());
            URL managedResourceUrl = targetResource.getManagedResource().getUrl();
            ValidationResult validationResult = shapeTree.validateResource(null, shapeTreeRequest.getResourceType(), RequestHelper.getIncomingBodyGraph(shapeTreeRequest, managedResourceUrl, targetResource.getManagedResource()), RequestHelper.getIncomingFocusNode(shapeTreeRequest, managedResourceUrl));
            if (Boolean.FALSE.equals(validationResult.isValid())) { return failValidation(validationResult); }

        }

        // No issues with validation, so the request is passed along
        return Optional.empty();

    }

    protected Optional<DocumentResponse> deleteShapeTreeInstance() {
        // Nothing to validate in a delete request, so the request is passed along
        return Optional.empty();
    }

    protected Optional<DocumentResponse> assignShapeTreeToResource(ShapeTreeInstance shapeTreeInstance,
                                                                   ShapeTreeContext shapeTreeContext,
                                                                   ShapeTreeManager rootManager,
                                                                   ShapeTreeAssignment rootAssignment,
                                                                   ShapeTreeAssignment parentAssignment,
                                                                   ValidationResult advanceValidationResult)
            throws ShapeTreeException {

        ShapeTree managedResourceShapeTree = null;
        ShapeTreeManager shapeTreeManager = null;
        URL matchingFocusNode = null;
        ShapeTreeAssignment managedResourceAssignment = null;
        Optional<DocumentResponse> validationResponse = null;

        ensureValidationResultIsUsableForAssignment(advanceValidationResult, "Invalid advance validation result provided for resource assignment");
        if (advanceValidationResult != null) { managedResourceShapeTree = advanceValidationResult.getMatchingShapeTree(); }
        if (advanceValidationResult != null) { matchingFocusNode = advanceValidationResult.getMatchingFocusNode(); }

        if (atRootOfPlantHierarchy(rootAssignment, shapeTreeInstance.getManagedResource())) {

            // If we are at the root of the plant hierarchy we don't need to validate the managed resource against
            // a shape tree managing a parent container. We only need to validate the managed resource against
            // the shape tree that is being planted at the root to ensure it conforms.
            managedResourceShapeTree = ShapeTreeFactory.getShapeTree(rootAssignment.getShapeTree());
            if (advanceValidationResult == null) {    // If this validation wasn't performed in advance
                ValidationResult validationResult = managedResourceShapeTree.validateResource(shapeTreeInstance.getManagedResource());
                if (Boolean.FALSE.equals(validationResult.isValid())) { return failValidation(validationResult); }
                matchingFocusNode = validationResult.getMatchingFocusNode();
            }

        } else {

            // Not at the root of the plant hierarchy. Validate proposed resource against the shape tree
            // managing the parent container, then extract the matching shape tree and focus node on success
            if (advanceValidationResult == null) {    // If this validation wasn't performed in advance
                ShapeTree parentShapeTree = ShapeTreeFactory.getShapeTree(parentAssignment.getShapeTree());
                ValidationResult validationResult = parentShapeTree.validateContainedResource(shapeTreeInstance.getManagedResource());
                if (Boolean.FALSE.equals(validationResult.isValid())) { return failValidation(validationResult); }
                managedResourceShapeTree = validationResult.getMatchingShapeTree();
                matchingFocusNode = validationResult.getMatchingFocusNode();
            }

        }

        shapeTreeManager = getManagerForAssignment(shapeTreeInstance, rootManager, rootAssignment);
        managedResourceAssignment = getAssignment(shapeTreeInstance.getManagedResource(), shapeTreeManager, rootAssignment, managedResourceShapeTree, matchingFocusNode);

        // If the primary resource is a container, and its shape tree specifies its contents with st:contains
        // Recursively traverse the hierarchy and perform shape tree assignment
        if (shapeTreeInstance.getManagedResource().isContainer() && managedResourceShapeTree.getContains() != null && !managedResourceShapeTree.getContains().isEmpty()) {

            // If the container is not empty, perform a recursive, depth first validation and assignment for each
            // contained resource by recursively calling this method (assignShapeTreeToResource)
            // TODO - Provide a configurable maximum limit on contained resources for a recursive plant, generate ShapeTreeException
            List<ShapeTreeInstance> containedResources = this.resourceAccessor.getContainedResources(shapeTreeContext, shapeTreeInstance.getManagedResource().getUrl());
            if (!containedResources.isEmpty()) {
                Collections.sort(containedResources, new SortByShapeTreeResourceType());  // Evaluate containers, then resources
                for (ShapeTreeInstance containedResource : containedResources) {
                    validationResponse = assignShapeTreeToResource(containedResource, shapeTreeContext, null, rootAssignment, managedResourceAssignment, null);
                    if (validationResponse.isPresent()) { return validationResponse; }
                }
            }
        }
        shapeTreeInstance.createOrUpdateManagerResource(shapeTreeManager);

        return Optional.empty();

    }

    protected Optional<DocumentResponse> unassignShapeTreeFromResource(ShapeTreeInstance shapeTreeInstance, ShapeTreeContext shapeTreeContext,
                                                                       ShapeTreeAssignment rootAssignment) throws ShapeTreeException {


        ensureShapeTreeInstanceResourceExists(shapeTreeInstance.getManagedResource(), "Cannot remove assignment from non-existent managed resource");
        ensureShapeTreeInstanceResourceExists(shapeTreeInstance.getManagerResource(), "Cannot remove assignment from non-existent manager resource");

        ShapeTreeManager shapeTreeManager = shapeTreeInstance.getManagerResource().getManager();
        ShapeTreeAssignment removeAssignment = shapeTreeManager.getAssignmentForRoot(rootAssignment);
        ShapeTree managedResourceShapeTree = ShapeTreeFactory.getShapeTree(removeAssignment.getShapeTree());

        Optional<DocumentResponse> validationResponse = null;

        // If the managed resource is a container, and its shape tree specifies its contents with st:contains
        // Recursively traverse the hierarchy and perform shape tree unassignment
        if (shapeTreeInstance.getManagedResource().isContainer() && managedResourceShapeTree.getContains() != null && !managedResourceShapeTree.getContains().isEmpty()) {

            // TODO - Should there also be a configurable maximum limit on unplanting?
            List<ShapeTreeInstance> containedResources = this.resourceAccessor.getContainedResources(shapeTreeContext, shapeTreeInstance.getManagedResource().getUrl());
            // If the container is not empty
            if (!containedResources.isEmpty()) {
                // Sort contained resources so that containers are evaluated first, then resources
                Collections.sort(containedResources, new SortByShapeTreeResourceType());
                // Perform a depth first unassignment for each contained resource
                for (ShapeTreeInstance containedResource : containedResources) {
                    // Recursively call this function on the contained resource
                    validationResponse = unassignShapeTreeFromResource(containedResource, shapeTreeContext, rootAssignment);
                    if (validationResponse.isPresent()) { return validationResponse; }
                }
            }
        }

        shapeTreeManager.removeAssignment(removeAssignment);

        deleteOrUpdateManagerResource(shapeTreeContext, shapeTreeInstance.getManagerResource(), shapeTreeManager);

        return Optional.empty();

    }

    private void deleteOrUpdateManagerResource(ShapeTreeContext shapeTreeContext,
                                               ShapeTreeInstance.ManagerResource managerResource,
                                               ShapeTreeManager shapeTreeManager) throws ShapeTreeException {

        if (shapeTreeManager.getAssignments().isEmpty()) {
            DocumentResponse response = this.resourceAccessor.deleteResource(shapeTreeContext, managerResource);
            ensureDeleteIsSuccessful(response);
        } else {
            // Update the existing manager resource for the managed resource
            this.resourceAccessor.updateResource(shapeTreeContext, "PUT", managerResource, shapeTreeManager.getGraph().toString());
        }

    }

    private ShapeTreeManager getManagerForAssignment(ShapeTreeInstance shapeTreeInstance,
                                                     ShapeTreeManager rootManager,
                                                     ShapeTreeAssignment rootAssignment) throws ShapeTreeException {

        ShapeTreeManager shapeTreeManager = null;

        // When at the top of the plant hierarchy, use the root manager from the initial plant request body
        if (atRootOfPlantHierarchy(rootAssignment, shapeTreeInstance.getManagedResource())) { return rootManager; }

        if (!shapeTreeInstance.getManagerResource().wasSuccessful()) {
            // If the existing manager resource doesn't exist make a new shape tree manager
            shapeTreeManager = new ShapeTreeManager(shapeTreeInstance.getManagerResource().getUrl());
        } else {
            // Get the existing shape tree manager from the manager resource graph
            // TODO - this was seemingly incorrect before it was adjusted. Needs to be debugged and confirmed as working properly now
            Graph managerGraph = shapeTreeInstance.getManagerResource().getGraph(shapeTreeInstance.getManagerResource().getUrl());
            shapeTreeManager = ShapeTreeManager.getFromGraph(shapeTreeInstance.getManagerResource().getUrl(), managerGraph);
        }

        return shapeTreeManager;

    }

    private ShapeTreeAssignment getAssignment(ShapeTreeInstance.ManagedResource managedResource,
                                              ShapeTreeManager shapeTreeManager,
                                              ShapeTreeAssignment rootAssignment,
                                              ShapeTree managedResourceShapeTree,
                                              URL matchingFocusNode) throws ShapeTreeException {

        URL assignmentUrl = null;

        if (!atRootOfPlantHierarchy(rootAssignment, managedResource)) {
            // Mint a new assignment URL, since it wouldn't have been passed in the initial request body
            assignmentUrl = shapeTreeManager.mintAssignment();
        }

        // Build the managed resource assignment
        URL matchingNode = matchingFocusNode == null ? null : matchingFocusNode;
        ShapeTreeAssignment managedResourceAssignment = new ShapeTreeAssignment(managedResourceShapeTree.getId(),
                managedResource.getUrl(),
                rootAssignment.getUrl(),
                matchingNode,
                managedResourceShapeTree.getShape(),
                assignmentUrl);

        if (!atRootOfPlantHierarchy(rootAssignment, managedResource)) {
            // Add the shape tree assignment to the shape tree managed for the managed resource
            shapeTreeManager.addAssignment(managedResourceAssignment);
        }

        return managedResourceAssignment;

    }

    private boolean atRootOfPlantHierarchy(ShapeTreeAssignment rootAssignment, ShapeTreeInstance.ManagedResource managedResource) {
        return rootAssignment.getManagedResource().equals(managedResource.getUrl());
    }

    // Return a root shape tree manager associated with a given shape tree assignment
    private ShapeTreeManager getRootManager(ShapeTreeContext shapeTreeContext, ShapeTreeAssignment assignment) throws ShapeTreeException {

        URL rootAssignmentUrl = assignment.getRootAssignment();
        ShapeTreeInstance.ManagerResource managerResource = new ShapeTreeInstance(rootAssignmentUrl, this.resourceAccessor, shapeTreeContext).getManagerResource(); // this.resourceAccessor.getResource(shapeTreeContext, rootLocationBaseUrl);

        return managerResource.getManager();

    }

    // Return a root shape tree manager associated with a given shape tree assignment
    private ShapeTreeAssignment getRootAssignment(ShapeTreeContext shapeTreeContext, ShapeTreeAssignment assignment) throws ShapeTreeException {

        ShapeTreeManager rootManager = getRootManager(shapeTreeContext, assignment);

        for (ShapeTreeAssignment rootAssignment : rootManager.getAssignments()) {
            if (rootAssignment.getUrl() != null && rootAssignment.getUrl().equals(assignment.getRootAssignment())) {
                return rootAssignment;
            }
        }
        return null;

    }

    private void ensureValidationResultIsUsableForAssignment(ValidationResult validationResult, String message) throws ShapeTreeException {
        // Null is a usable state of the validation result in the context of assignment
        if (validationResult != null &&
            (validationResult.getValid() == null ||
            validationResult.getMatchingShapeTree() == null ||
            validationResult.getValidatingShapeTree() == null)) {
                throw new ShapeTreeException(400, message);
        }
    }

    private void ensureShapeTreeInstanceResourceExists(ShapeTreeInstance.Resource shapeTreeResource, String message) throws ShapeTreeException {
        if (shapeTreeResource == null || !shapeTreeResource.wasSuccessful()) {
            throw new ShapeTreeException(404, message);
        }
    }

    private void ensureRequestResourceIsContainer(ShapeTreeInstance.ManagedResource shapeTreeResource, String message) throws ShapeTreeException {
        if (!shapeTreeResource.isContainer()) {
            throw new ShapeTreeException(400, message);
        }
    }

    private void ensureTargetManagedResourceDoesNotExist(ShapeTreeContext shapeTreeContext, URL targetResourceUrl, String message) throws ShapeTreeException {
        ShapeTreeInstance targetManagedResource = new ShapeTreeInstance(targetResourceUrl, this.resourceAccessor, shapeTreeContext);
        if (targetManagedResource.wasCreatedFromManager() || targetManagedResource.getManagedResource().wasSuccessful()) {
            throw new ShapeTreeException(409, message);
        }
    }

    private void ensureShapeTreeManagerExists(ShapeTreeManager manager, String message) throws ShapeTreeException {
        if (manager == null || manager.getAssignments() == null || manager.getAssignments().isEmpty()) {
            throw new ShapeTreeException(400, message);
        }
    }

    private void ensureAssignmentExists(ShapeTreeAssignment assignment, String message) throws ShapeTreeException {
        if (assignment == null) {
            throw new ShapeTreeException(400, message);
        }
    }

    private void ensureAllRemovedFromManagerByDelete(ShapeTreeRequest shapeTreeRequest) throws ShapeTreeException {
        if (!shapeTreeRequest.getMethod().equals(DELETE)) {
            throw new ShapeTreeException(500, "Removal of all ShapeTreeAssignments from a ShapeTreeManager MUST use HTTP DELETE");
        }
    }

    private void ensureRemovedAssignmentsAreRoot(ShapeTreeManagerDelta delta) throws ShapeTreeException {
        for (ShapeTreeAssignment assignment : delta.getRemovedAssignments()) {
            if (!assignment.isRootAssignment()) {
                throw new ShapeTreeException(500, "Cannot remove non-root assignment: " + assignment.getUrl().toString() + ". Must unplant root assignment at: " + assignment.getRootAssignment().toString());
            }
        }
    }

    private void ensureUpdatedAssignmentIsRoot(ShapeTreeManagerDelta delta) throws ShapeTreeException {
        for (ShapeTreeAssignment updatedAssignment : delta.getUpdatedAssignments()) {
            if (!updatedAssignment.isRootAssignment()) {
                throw new ShapeTreeException(500, "Cannot update non-root assignment: " + updatedAssignment.getUrl().toString() + ". Must update root assignment at: " + updatedAssignment.getRootAssignment().toString());
            }
        }
    }

    private void ensureDeleteIsSuccessful(DocumentResponse response) throws ShapeTreeException {
        List<Integer> successCodes = Arrays.asList(202,204,200);
        if (!successCodes.contains(response.getStatusCode())) {
            throw new ShapeTreeException(500, "Failed to delete manager resource. Received " + response.getStatusCode() + ": " + response.getBody());
        }
    }

    private DocumentResponse successfulValidation() {
        return new DocumentResponse(new ResourceAttributes(), "OK", 201);
    }

    private Optional<DocumentResponse> failValidation(ValidationResult validationResult) {
        return Optional.of(new DocumentResponse(new ResourceAttributes(), validationResult.getMessage(),422));
    }
}



class SortByShapeTreeResourceType implements Comparator<ShapeTreeInstance>, Serializable {

    // Used for sorting by shape tree resource type with the following order
    // 1. Containers
    // 2. Resources
    // 3. Non-RDF Resources

    @SneakyThrows // @@ These are known to be user-owned
    public int compare (ShapeTreeInstance a, ShapeTreeInstance b) {
        return a.getManagedResource().getResourceType().compareTo(b.getManagedResource().getResourceType());
    }

}
