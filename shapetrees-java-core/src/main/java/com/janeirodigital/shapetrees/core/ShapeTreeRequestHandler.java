package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.comparators.ResourceTypeAssignmentPriority;
import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.helpers.RequestHelper;
import lombok.extern.slf4j.Slf4j;
import org.apache.jena.graph.Graph;

import java.net.URL;
import java.util.HashMap;
import java.util.Optional;
import java.util.Collections;
import java.util.List;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Arrays;

import static com.janeirodigital.shapetrees.core.ManageableInstance.TEXT_TURTLE;

@Slf4j
public class ShapeTreeRequestHandler {

    private static final String DELETE = "DELETE";
    ResourceAccessor resourceAccessor;

    public ShapeTreeRequestHandler(ResourceAccessor resourceAccessor) {
        this.resourceAccessor = resourceAccessor;
    }

    public DocumentResponse manageShapeTree(ManageableInstance manageableInstance, ShapeTreeRequest shapeTreeRequest) throws ShapeTreeException {

        Optional<DocumentResponse> validationResponse;
        ShapeTreeManager updatedRootManager = RequestHelper.getIncomingShapeTreeManager(shapeTreeRequest, manageableInstance.getManagerResource());
        ShapeTreeManager existingRootManager = manageableInstance.getManagerResource().getManager();

        // Determine assignments that have been removed, added, and/or updated
        ShapeTreeManagerDelta delta = ShapeTreeManagerDelta.evaluate(existingRootManager, updatedRootManager);

        // It is invalid for a manager resource to be left with no assignments.
        // Shape Trees, §3: A shape tree manager includes one or more shape tree assignments via st:hasAssignment.
        if (delta.allRemoved()) { ensureAllRemovedFromManagerByDelete(shapeTreeRequest); }

        if (delta.wasReduced()) {
            // An existing assignment has been removed from the manager for the managed resource.
            validationResponse = unplantShapeTree(manageableInstance, manageableInstance.getShapeTreeContext(), delta);
            if (validationResponse.isPresent()) { return validationResponse.get(); }
        }

        if (delta.isUpdated()) {
            // An existing assignment has been updated, or new assignments have been added
            validationResponse = plantShapeTree(manageableInstance, manageableInstance.getShapeTreeContext(), updatedRootManager, delta);
            if (validationResponse.isPresent()) { return validationResponse.get(); }
        }

        // TODO: Test: Need a test with reduce and updated delta to make sure we never return success from plant or unplant.

        return successfulValidation();
    }

    /**
     * Plants a shape tree on an existing resource
     * @param manageableInstance
     * @param shapeTreeContext
     * @param updatedRootManager
     * @param delta
     * @return DocumentResponse
     * @throws ShapeTreeException
     */
    public Optional<DocumentResponse> plantShapeTree(ManageableInstance manageableInstance, ShapeTreeContext shapeTreeContext, ShapeTreeManager updatedRootManager, ShapeTreeManagerDelta delta) throws ShapeTreeException {

        // Cannot directly update assignments that are not root locations
        ensureUpdatedAssignmentIsRoot(delta);

        // Run recursive assignment for each updated assignment in the root manager
        for (ShapeTreeAssignment rootAssignment : delta.getUpdatedAssignments()) {
            Optional<DocumentResponse> validationResponse = assignShapeTreeToResource(manageableInstance, shapeTreeContext, updatedRootManager, rootAssignment, rootAssignment, null);
            if (validationResponse.isPresent()) { return validationResponse; }
        }

        return Optional.empty();
    }

    public Optional<DocumentResponse> unplantShapeTree(ManageableInstance manageableInstance, ShapeTreeContext shapeTreeContext, ShapeTreeManagerDelta delta) throws ShapeTreeException {

        ensureRemovedAssignmentsAreRoot(delta); // Cannot unplant a non-root location

        // Run recursive unassignment for each removed assignment in the updated root manager
        for (ShapeTreeAssignment rootAssignment : delta.getRemovedAssignments()) {
            Optional<DocumentResponse> validationResponse = unassignShapeTreeFromResource(manageableInstance, shapeTreeContext, rootAssignment);
            if (validationResponse.isPresent()) { return validationResponse; }
        }

        return Optional.empty();
    }

    // TODO: #87: do sanity checks on meta of meta, c.f. @see https://github.com/xformativ/shapetrees-java/issues/87
    public Optional<DocumentResponse> createShapeTreeInstance(ManageableInstance manageableInstance, ManageableInstance containerResource, ShapeTreeRequest shapeTreeRequest, String proposedName) throws ShapeTreeException {
        // Sanity check user-owned resource @@ delete 'cause type checks
        ensureInstanceResourceExists(containerResource.getManageableResource(),"Target container for resource creation not found");
        ensureRequestResourceIsContainer(containerResource.getManageableResource(),"Cannot create a shape tree instance in a non-container resource");

        // Prepare the target resource for validation and creation
        URL targetResourceUrl = RequestHelper.normalizeSolidResourceUrl(containerResource.getManageableResource().getUrl(), proposedName, shapeTreeRequest.getResourceType());
        ensureTargetResourceDoesNotExist(manageableInstance.getShapeTreeContext(), targetResourceUrl,"Cannot create target resource at " + targetResourceUrl + " because it already exists");

        ensureInstanceResourceExists(containerResource.getManagerResource(), "Should not be creating a shape tree instance on an unmanaged target container");

        ShapeTreeManager containerManager = containerResource.getManagerResource().getManager();
        ensureShapeTreeManagerExists(containerManager, "Cannot have a shape tree manager resource without a shape tree manager containing at least one shape tree assignment");

        // Get the shape tree associated that specifies what resources can be contained by the target container (st:contains)

        List<ShapeTreeAssignment> containingAssignments = containerManager.getContainingAssignments();

        // If there are no containing shape trees for the target container, request is valid and can be passed through
        if (containingAssignments.isEmpty()) { return Optional.empty(); }

        List<URL> targetShapeTrees = RequestHelper.getIncomingTargetShapeTrees(shapeTreeRequest, targetResourceUrl);
        List<URL> incomingFocusNodes = RequestHelper.getIncomingFocusNodes(shapeTreeRequest, targetResourceUrl);
        Graph incomingBodyGraph = RequestHelper.getIncomingBodyGraph(shapeTreeRequest, targetResourceUrl, null);
        HashMap<ShapeTreeAssignment, ValidationResult> validationResults = new HashMap<>();

        for (ShapeTreeAssignment containingAssignment : containingAssignments) {
            URL containerShapeTreeUrl = containingAssignment.getShapeTree();
            ShapeTree containerShapeTree = ShapeTreeFactory.getShapeTree(containerShapeTreeUrl);
            ValidationResult validationResult = containerShapeTree.validateContainedResource(proposedName, shapeTreeRequest.getResourceType(), targetShapeTrees, incomingBodyGraph, incomingFocusNodes);
            if (Boolean.FALSE.equals(validationResult.isValid())) { return failValidation(validationResult); }
            validationResults.put(containingAssignment, validationResult);
        }

        // if any of the provided focus nodes weren't matched validation must fail
        List<URL> unmatchedNodes  = getUnmatchedFocusNodes(validationResults.values(), incomingFocusNodes);
        if (!unmatchedNodes.isEmpty()) { return failValidation(new ValidationResult(false, "Failed to match target focus nodes: " + unmatchedNodes)); }

        log.debug("Creating shape tree instance at {}", targetResourceUrl);

        ManageableInstance createdInstance = this.resourceAccessor.createInstance(manageableInstance.getShapeTreeContext(), shapeTreeRequest.getMethod(), targetResourceUrl, shapeTreeRequest.getHeaders(), shapeTreeRequest.getBody(), shapeTreeRequest.getContentType());

        for (ShapeTreeAssignment containingAssignment : containingAssignments) {

            ShapeTreeAssignment rootShapeTreeAssignment = getRootAssignment(manageableInstance.getShapeTreeContext(), containingAssignment);
            ensureAssignmentExists(rootShapeTreeAssignment, "Unable to find root shape tree assignment at " + containingAssignment.getRootAssignment());

            log.debug("Assigning shape tree to created resource: {}", createdInstance.getManagerResource().getUrl());
            // Note: By providing the positive advance validationResult, we let the assignment operation know that validation
            // has already been performed with a positive result, and avoid having it perform the validation a second time
            Optional<DocumentResponse> assignResult = assignShapeTreeToResource(createdInstance, manageableInstance.getShapeTreeContext(), null, rootShapeTreeAssignment, containingAssignment, validationResults.get(containingAssignment));
            if (assignResult.isPresent()) { return assignResult; }

        }

        return Optional.of(successfulValidation());
    }

    public Optional<DocumentResponse> updateShapeTreeInstance(ManageableInstance targetResource, ShapeTreeContext shapeTreeContext, ShapeTreeRequest shapeTreeRequest) throws ShapeTreeException {


        ensureInstanceResourceExists(targetResource.getManageableResource(),"Target resource to update not found");
        ensureInstanceResourceExists(targetResource.getManagerResource(), "Should not be updating an unmanaged resource as a shape tree instance");

        ShapeTreeManager manager = targetResource.getManagerResource().getManager();
        ensureShapeTreeManagerExists(manager, "Cannot have a shape tree manager resource without a shape tree manager with at least one shape tree assignment");

        for (ShapeTreeAssignment assignment : manager.getAssignments()) {

            // Evaluate the update against each ShapeTreeAssignment managing the resource.
            // All must pass for the update to validate
            ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(assignment.getShapeTree());
            URL managedResourceUrl = targetResource.getManageableResource().getUrl();
            ValidationResult validationResult = shapeTree.validateResource(null, shapeTreeRequest.getResourceType(), RequestHelper.getIncomingBodyGraph(shapeTreeRequest, managedResourceUrl, targetResource.getManageableResource()), RequestHelper.getIncomingFocusNodes(shapeTreeRequest, managedResourceUrl));
            if (Boolean.FALSE.equals(validationResult.isValid())) { return failValidation(validationResult); }

        }

        // No issues with validation, so the request is passed along
        return Optional.empty();

    }

    public Optional<DocumentResponse> deleteShapeTreeInstance() {
        // Nothing to validate in a delete request, so the request is passed along
        return Optional.empty();
    }

    protected Optional<DocumentResponse> assignShapeTreeToResource(ManageableInstance manageableInstance,
                                                                   ShapeTreeContext shapeTreeContext,
                                                                   ShapeTreeManager rootManager,
                                                                   ShapeTreeAssignment rootAssignment,
                                                                   ShapeTreeAssignment parentAssignment,
                                                                   ValidationResult advanceValidationResult)
            throws ShapeTreeException {

        ShapeTree managingShapeTree = null;
        ShapeTreeManager shapeTreeManager = null;
        URL matchingFocusNode = null;
        ShapeTreeAssignment managingAssignment = null;
        Optional<DocumentResponse> validationResponse;

        ensureValidationResultIsUsableForAssignment(advanceValidationResult, "Invalid advance validation result provided for resource assignment");
        if (advanceValidationResult != null) { managingShapeTree = advanceValidationResult.getMatchingShapeTree(); }
        if (advanceValidationResult != null) { matchingFocusNode = advanceValidationResult.getMatchingFocusNode(); }

        if (atRootOfPlantHierarchy(rootAssignment, manageableInstance.getManageableResource())) {

            // If we are at the root of the plant hierarchy we don't need to validate the managed resource against
            // a shape tree managing a parent container. We only need to validate the managed resource against
            // the shape tree that is being planted at the root to ensure it conforms.
            managingShapeTree = ShapeTreeFactory.getShapeTree(rootAssignment.getShapeTree());
            if (advanceValidationResult == null) {    // If this validation wasn't performed in advance
                ValidationResult validationResult = managingShapeTree.validateResource(manageableInstance.getManageableResource());
                if (Boolean.FALSE.equals(validationResult.isValid())) { return failValidation(validationResult); }
                matchingFocusNode = validationResult.getMatchingFocusNode();
            }

        } else {

            // Not at the root of the plant hierarchy. Validate proposed resource against the shape tree
            // managing the parent container, then extract the matching shape tree and focus node on success
            if (advanceValidationResult == null) {    // If this validation wasn't performed in advance
                ShapeTree parentShapeTree = ShapeTreeFactory.getShapeTree(parentAssignment.getShapeTree());
                ValidationResult validationResult = parentShapeTree.validateContainedResource(manageableInstance.getManageableResource());
                if (Boolean.FALSE.equals(validationResult.isValid())) { return failValidation(validationResult); }
                managingShapeTree = validationResult.getMatchingShapeTree();
                matchingFocusNode = validationResult.getMatchingFocusNode();
            }

        }

        shapeTreeManager = getManagerForAssignment(manageableInstance, rootManager, rootAssignment);
        managingAssignment = getAssignment(manageableInstance.getManageableResource(), shapeTreeManager, rootAssignment, managingShapeTree, matchingFocusNode);

        // If the primary resource is a container, and its shape tree specifies its contents with st:contains
        // Recursively traverse the hierarchy and perform shape tree assignment
        if (manageableInstance.getManageableResource().isContainer() && !managingShapeTree.getContains().isEmpty()) {

            // If the container is not empty, perform a recursive, depth first validation and assignment for each
            // contained resource by recursively calling this method (assignShapeTreeToResource)
            // TODO - Provide a configurable maximum limit on contained resources for a recursive plant, generate ShapeTreeException
            List<ManageableInstance> containedResources = this.resourceAccessor.getContainedInstances(shapeTreeContext, manageableInstance.getManageableResource().getUrl());
            if (!containedResources.isEmpty()) {
                Collections.sort(containedResources, new ResourceTypeAssignmentPriority());  // Evaluate containers, then resources
                for (ManageableInstance containedResource : containedResources) {
                    validationResponse = assignShapeTreeToResource(containedResource, shapeTreeContext, null, rootAssignment, managingAssignment, null);
                    if (validationResponse.isPresent()) { return validationResponse; }
                }
            }
        }

        if (manageableInstance.getManagerResource().isExists()) {
            // update manager resource
            this.resourceAccessor.updateResource(shapeTreeContext, "PUT", manageableInstance.getManagerResource(), shapeTreeManager.getGraph().toString());
        } else {
            // create manager resource
            ResourceAttributes headers = new ResourceAttributes();
            headers.setAll(HttpHeaders.CONTENT_TYPE.getValue(), Collections.singletonList(TEXT_TURTLE));
            this.resourceAccessor.createResource(shapeTreeContext, "POST", manageableInstance.getManagerResource().getUrl(), headers, shapeTreeManager.getGraph().toString(), TEXT_TURTLE);
        }

        return Optional.empty();

    }

    protected Optional<DocumentResponse> unassignShapeTreeFromResource(ManageableInstance manageableInstance, ShapeTreeContext shapeTreeContext,
                                                                       ShapeTreeAssignment rootAssignment) throws ShapeTreeException {


        ensureInstanceResourceExists(manageableInstance.getManageableResource(), "Cannot remove assignment from non-existent managed resource");
        ensureInstanceResourceExists(manageableInstance.getManagerResource(), "Cannot remove assignment from non-existent manager resource");

        ShapeTreeManager shapeTreeManager = manageableInstance.getManagerResource().getManager();
        ShapeTreeAssignment assignmentToRemove = shapeTreeManager.getAssignmentForRoot(rootAssignment);
        ShapeTree assignedShapeTree = ShapeTreeFactory.getShapeTree(assignmentToRemove.getShapeTree());

        Optional<DocumentResponse> validationResponse;

        // If the managed resource is a container, and its shape tree specifies its contents with st:contains
        // Recursively traverse the hierarchy and perform shape tree unassignment
        if (manageableInstance.getManageableResource().isContainer() && !assignedShapeTree.getContains().isEmpty()) {

            // TODO - Should there also be a configurable maximum limit on unplanting?
            List<ManageableInstance> containedResources = this.resourceAccessor.getContainedInstances(shapeTreeContext, manageableInstance.getManageableResource().getUrl());
            // If the container is not empty
            if (!containedResources.isEmpty()) {
                // Sort contained resources so that containers are evaluated first, then resources
                Collections.sort(containedResources, new ResourceTypeAssignmentPriority());
                // Perform a depth first unassignment for each contained resource
                for (ManageableInstance containedResource : containedResources) {
                    // Recursively call this function on the contained resource
                    validationResponse = unassignShapeTreeFromResource(containedResource, shapeTreeContext, rootAssignment);
                    if (validationResponse.isPresent()) { return validationResponse; }
                }
            }
        }

        shapeTreeManager.removeAssignment(assignmentToRemove);

        deleteOrUpdateManagerResource(shapeTreeContext, manageableInstance.getManagerResource(), shapeTreeManager);

        return Optional.empty();

    }

    private void deleteOrUpdateManagerResource(ShapeTreeContext shapeTreeContext,
                                               ManagerResource managerResource,
                                               ShapeTreeManager shapeTreeManager) throws ShapeTreeException {

        if (shapeTreeManager.getAssignments().isEmpty()) {
            DocumentResponse response = this.resourceAccessor.deleteResource(shapeTreeContext, managerResource);
            ensureDeleteIsSuccessful(response);
        } else {
            // Update the existing manager resource for the managed resource
            this.resourceAccessor.updateResource(shapeTreeContext, "PUT", managerResource, shapeTreeManager.getGraph().toString());
        }

    }

    private ShapeTreeManager getManagerForAssignment(ManageableInstance manageableInstance,
                                                     ShapeTreeManager rootManager,
                                                     ShapeTreeAssignment rootAssignment) throws ShapeTreeException {

        ShapeTreeManager shapeTreeManager = null;
        URL managerResourceUrl = manageableInstance.getManagerResource().getUrl();

        // When at the top of the plant hierarchy, use the root manager from the initial plant request body
        if (atRootOfPlantHierarchy(rootAssignment, manageableInstance.getManageableResource())) { return rootManager; }

        if (!manageableInstance.getManagerResource().isExists()) {
            // If the existing manager resource doesn't exist make a new shape tree manager
            shapeTreeManager = new ShapeTreeManager(managerResourceUrl);
        } else {
            // Get the existing shape tree manager from the manager resource graph
            // TODO - this was seemingly incorrect before it was adjusted. Needs to be debugged and confirmed as working properly now
            Graph managerGraph = manageableInstance.getManagerResource().getGraph(managerResourceUrl);
            shapeTreeManager = ShapeTreeManager.getFromGraph(managerResourceUrl, managerGraph);
        }

        return shapeTreeManager;

    }

    private ShapeTreeAssignment getAssignment(ManageableResource manageableResource,
                                              ShapeTreeManager shapeTreeManager,
                                              ShapeTreeAssignment rootAssignment,
                                              ShapeTree managingShapeTree,
                                              URL matchingFocusNode) throws ShapeTreeException {

        if (atRootOfPlantHierarchy(rootAssignment, manageableResource)) { return rootAssignment; }

        // Mint a new assignment URL, since it wouldn't have been passed in the initial request body
        URL assignmentUrl = shapeTreeManager.mintAssignmentUrl();

        // Build the managed resource assignment
        URL matchingNode = matchingFocusNode == null ? null : matchingFocusNode;
        ShapeTreeAssignment managedResourceAssignment = new ShapeTreeAssignment(managingShapeTree.getId(),
                manageableResource.getUrl(),
                rootAssignment.getUrl(),
                matchingNode,
                managingShapeTree.getShape(),
                assignmentUrl);

        // Add the shape tree assignment to the shape tree managed for the managed resource
        shapeTreeManager.addAssignment(managedResourceAssignment);

        return managedResourceAssignment;

    }

    private boolean atRootOfPlantHierarchy(ShapeTreeAssignment rootAssignment, ManageableResource manageableResource) {
        return rootAssignment.getManagedResource().equals(manageableResource.getUrl());
    }

    // Return a root shape tree manager associated with a given shape tree assignment
    private ShapeTreeManager getRootManager(ShapeTreeContext shapeTreeContext, ShapeTreeAssignment assignment) throws ShapeTreeException {

        URL rootAssignmentUrl = assignment.getRootAssignment();
        ManageableInstance instance = this.resourceAccessor.getInstance(shapeTreeContext, rootAssignmentUrl);

        return instance.getManagerResource().getManager();

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

    private List<URL>
    getUnmatchedFocusNodes(Collection<ValidationResult> validationResults, List<URL> focusNodes) {
        List<URL> unmatchedNodes = new ArrayList<>();
        for (URL focusNode : focusNodes) {
            // Determine if each target focus node was matched
            boolean matched = false;
            for (ValidationResult validationResult : validationResults) {
                if (validationResult.getMatchingShapeTree().getShape() != null) {
                    if (validationResult.getMatchingFocusNode().equals(focusNode)) { matched = true; }
                }
            }
            if (!matched) { unmatchedNodes.add(focusNode); }
        }
        return unmatchedNodes;
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

    private void ensureInstanceResourceExists(InstanceResource instanceResource, String message) throws ShapeTreeException {
        if (instanceResource == null || !instanceResource.isExists()) {
            throw new ShapeTreeException(404, message);
        }
    }

    private void ensureRequestResourceIsContainer(ManageableResource shapeTreeResource, String message) throws ShapeTreeException {
        if (!shapeTreeResource.isContainer()) {
            throw new ShapeTreeException(400, message);
        }
    }

    private void ensureTargetResourceDoesNotExist(ShapeTreeContext shapeTreeContext, URL targetResourceUrl, String message) throws ShapeTreeException {
        ManageableInstance targetInstance = this.resourceAccessor.getInstance(shapeTreeContext, targetResourceUrl);
        if (targetInstance.wasRequestForManager() || targetInstance.getManageableResource().isExists()) {
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
