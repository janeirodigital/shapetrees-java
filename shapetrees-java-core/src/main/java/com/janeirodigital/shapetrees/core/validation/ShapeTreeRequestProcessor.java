package com.janeirodigital.shapetrees.core.validation;

import com.janeirodigital.shapetrees.core.resources.DocumentResponse;
import com.janeirodigital.shapetrees.core.resources.ResourceAttributes;
import com.janeirodigital.shapetrees.core.comparators.ResourceTypeAssignmentPriority;
import com.janeirodigital.shapetrees.core.enums.HttpHeader;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.helpers.RequestHelper;
import com.janeirodigital.shapetrees.core.resources.InstanceResource;
import com.janeirodigital.shapetrees.core.resources.ManageableInstance;
import com.janeirodigital.shapetrees.core.resources.ManageableResource;
import com.janeirodigital.shapetrees.core.resources.ResourceAccessor;
import lombok.extern.slf4j.Slf4j;
import org.apache.jena.graph.Graph;

import java.net.URL;
import java.util.*;

import static com.janeirodigital.shapetrees.core.resources.ManageableInstance.*;

@Slf4j
public class ShapeTreeRequestProcessor {

    private ShapeTreeRequestProcessor() { }

    public static ValidationResult updateManager(ResourceAccessor accessor, ManageableInstance manageableInstance, ShapeTreeRequest shapeTreeRequest) throws ShapeTreeException {

        ValidationResult result = new ValidationResult(false, "A request to manage shape tree assignment is invalid until it is proven valid");
        ShapeTreeManager updatedRootManager = RequestHelper.getIncomingShapeTreeManager(shapeTreeRequest, manageableInstance.getManagerResource());
        ShapeTreeManager existingRootManager = manageableInstance.getManagerResource().getManager();

        // Determine assignments that have been removed, added, and/or updated
        ShapeTreeManagerDelta delta = ShapeTreeManagerDelta.evaluate(existingRootManager, updatedRootManager);

        // It is invalid for a manager resource to be left with no assignments.
        // Shape Trees, §3: A shape tree manager includes one or more shape tree assignments via st:hasAssignment.
        if (delta.allRemoved() && !shapeTreeRequest.getMethod().equals("DELETE")) {
            throw new ShapeTreeException(500, "Removal of all ShapeTreeAssignments from a ShapeTreeManager MUST use HTTP DELETE");
        }

        if (delta.wasReduced()) {
            // An existing assignment has been removed from the manager for the managed resource.
            result = unplant(accessor, manageableInstance, manageableInstance.getShapeTreeContext(), delta);
            if (!result.isValid()) { return result; }
        }

        if (delta.isUpdated()) {
            // An existing assignment has been updated, or new assignments have been added
            result = plant(accessor, manageableInstance, manageableInstance.getShapeTreeContext(), updatedRootManager, delta);
            if (!result.isValid()) { return result; }
        }

        return result;
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
    public static ValidationResult plant(ResourceAccessor accessor, ManageableInstance manageableInstance, ShapeTreeContext shapeTreeContext, ShapeTreeManager updatedRootManager, ShapeTreeManagerDelta delta) throws ShapeTreeException {

        // Cannot directly update assignments that are not root locations
        ensureUpdatedAssignmentIsRoot(delta);

        ValidationResult result = new ValidationResult(false, "All plant operations are invalid until they can be proven valid");
        // Run recursive assignment for each updated assignment in the root manager
        for (ShapeTreeAssignment rootAssignment : delta.getUpdatedAssignments()) {
            result = assign(accessor, manageableInstance, shapeTreeContext, updatedRootManager, rootAssignment, rootAssignment, null);
            if (!result.isValid()) { return result; }
        }
        return result;
    }

    public static ValidationResult unplant(ResourceAccessor accessor, ManageableInstance manageableInstance, ShapeTreeContext shapeTreeContext, ShapeTreeManagerDelta delta) throws ShapeTreeException {

        ensureRemovedAssignmentsAreRoot(delta); // Cannot unplant a non-root location

        ValidationResult result = new ValidationResult(false, "All unplant operations are invalid until they can be proven valid");
        // Run recursive unassignment for each removed assignment in the updated root manager
        for (ShapeTreeAssignment rootAssignment : delta.getRemovedAssignments()) {
            result = unassign(accessor, manageableInstance, shapeTreeContext, rootAssignment);
            if (!result.isValid()) { return result; }
        }

        return result;
    }

    // TODO: #87: do sanity checks on meta of meta, c.f. @see https://github.com/xformativ/shapetrees-java/issues/87
    public static ContainingValidationResult validateCreate(ResourceAccessor accessor, ManageableInstance targetInstance, ManageableInstance parentContainer, ShapeTreeRequest shapeTreeRequest, String proposedName) throws ShapeTreeException {
        // Sanity check user-owned resource @@ delete 'cause type checks
        ensureInstanceResourceExists(parentContainer.getManageableResource(),"Target container for resource creation not found");
        ensureRequestResourceIsContainer(parentContainer.getManageableResource(),"Cannot create a shape tree instance in a non-container resource");

        // Prepare the target resource for validation and creation
        // TODO - CLEANUP - this is likely not needed anymore because target instance is accurate even in POST
        URL targetResourceUrl = RequestHelper.normalizeSolidResourceUrl(parentContainer.getManageableResource().getUrl(), proposedName, shapeTreeRequest.getResourceType());
        ensureInstanceResourceExists(parentContainer.getManagerResource(), "Should not be creating a shape tree instance on an unmanaged target container");

        ShapeTreeManager containerManager = parentContainer.getManagerResource().getManager();
        ensureShapeTreeManagerExists(containerManager, "Cannot have a shape tree manager resource without a shape tree manager containing at least one shape tree assignment");

        // Determine what can be contained by the parent container based on associated shape tree assignments (st:contains)
        List<ShapeTreeAssignment> containingAssignments = containerManager.getContainingAssignments();
        // If there are no containing shape trees for the target container, request is valid and can be passed through
        if (containingAssignments.isEmpty()) {
            return new ContainingValidationResult(true, "Parent container " + parentContainer.getManageableResource().getUrl() + "doesn't restrict members.");
        }

        List<URL> targetShapeTrees = RequestHelper.getIncomingTargetShapeTrees(shapeTreeRequest, targetResourceUrl);
        List<URL> incomingFocusNodes = RequestHelper.getIncomingFocusNodes(shapeTreeRequest, targetResourceUrl);
        Graph incomingBodyGraph = RequestHelper.getIncomingBodyGraph(shapeTreeRequest, targetResourceUrl, null);

        ContainingValidationResult containingResult = new ContainingValidationResult();
        for (ShapeTreeAssignment containingAssignment : containingAssignments) {
            URL containerShapeTreeUrl = containingAssignment.getShapeTree();
            ShapeTree containerShapeTree = ShapeTreeFactory.getShapeTree(containerShapeTreeUrl);
            ValidationResult result = containerShapeTree.validateContainedResource(proposedName, shapeTreeRequest.getResourceType(), targetShapeTrees, incomingBodyGraph, incomingFocusNodes);
            containingResult.add(containingAssignment, result);
            if (!result.isValid()) { return containingResult; }
        }

        // if any of the provided focus nodes weren't matched validation must fail
        List<URL> unmatchedNodes  = getUnmatchedFocusNodes(containingResult.getResults(), incomingFocusNodes);
        if (!unmatchedNodes.isEmpty()) { return new ContainingValidationResult(false, "Failed to match target focus nodes: " + unmatchedNodes); }

        return containingResult;
    }

    public static ValidationResult validateUpdate(ResourceAccessor accessor, ManageableInstance targetInstance, ShapeTreeRequest shapeTreeRequest) throws ShapeTreeException {

        ensureInstanceResourceExists(targetInstance.getManageableResource(),"Target resource to update not found");
        ensureInstanceResourceExists(targetInstance.getManagerResource(), "Should not be updating an unmanaged resource as a shape tree instance");

        ShapeTreeManager manager = targetInstance.getManagerResource().getManager();
        ensureShapeTreeManagerExists(manager, "Cannot have a shape tree manager resource without a shape tree manager with at least one shape tree assignment");

        for (ShapeTreeAssignment assignment : manager.getAssignments()) {

            // Evaluate the update against each ShapeTreeAssignment managing the resource.
            // All must pass for the update to validate
            ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(assignment.getShapeTree());
            URL managedResourceUrl = targetInstance.getManageableResource().getUrl();
            ValidationResult result = shapeTree.validateResource(null, shapeTreeRequest.getResourceType(), RequestHelper.getIncomingBodyGraph(shapeTreeRequest, managedResourceUrl, targetInstance.getManageableResource()), RequestHelper.getIncomingFocusNodes(shapeTreeRequest, managedResourceUrl));
            if (!result.isValid()) { return result; }
        }

        // No issues with validation, so the request is passed along
        return new ValidationResult(true, "Update to " + targetInstance.getManageableResource().getUrl() + "passed shape tree validation");
    }

    public static ValidationResult assign(ResourceAccessor accessor,
                                          ManageableInstance manageableInstance,
                                          ShapeTreeContext shapeTreeContext,
                                          ShapeTreeManager rootManager,
                                          ShapeTreeAssignment rootAssignment,
                                          ShapeTreeAssignment parentAssignment,
                                          ValidationResult advanceValidationResult) throws ShapeTreeException {

        ShapeTree managingShapeTree = null;
        ShapeTreeManager shapeTreeManager;
        URL matchingFocusNode = null;
        ShapeTreeAssignment managingAssignment;
        ValidationResult result;

        ensureValidationResultIsUsableForAssignment(advanceValidationResult, "Invalid advance validation result provided for resource assignment");
        if (advanceValidationResult != null) { managingShapeTree = advanceValidationResult.getMatchingShapeTree(); }
        if (advanceValidationResult != null) { matchingFocusNode = advanceValidationResult.getMatchingFocusNode(); }

        if (atRootOfPlantHierarchy(rootAssignment, manageableInstance.getManageableResource())) {

            // If we are at the root of the plant hierarchy we don't need to validate the managed resource against
            // a shape tree managing a parent container. We only need to validate the managed resource against
            // the shape tree that is being planted at the root to ensure it conforms.
            managingShapeTree = ShapeTreeFactory.getShapeTree(rootAssignment.getShapeTree());
            if (advanceValidationResult == null) {    // If this validation wasn't performed in advance
                result = managingShapeTree.validateResource(manageableInstance.getManageableResource());
                if (!result.isValid()) { return result; }
                matchingFocusNode = result.getMatchingFocusNode();
            }

        } else {

            // Not at the root of the plant hierarchy. Validate proposed resource against the shape tree
            // managing the parent container, then extract the matching shape tree and focus node on success
            if (advanceValidationResult == null) {    // If this validation wasn't performed in advance
                ShapeTree parentShapeTree = ShapeTreeFactory.getShapeTree(parentAssignment.getShapeTree());
                result = parentShapeTree.validateContainedResource(manageableInstance.getManageableResource());
                if (!result.isValid()) { return result; }
                managingShapeTree = result.getMatchingShapeTree();
                matchingFocusNode = result.getMatchingFocusNode();
            }

        }

        shapeTreeManager = getManagerForAssignment(manageableInstance, rootManager, rootAssignment);
        managingAssignment = getAssignment(manageableInstance.getManageableResource(), shapeTreeManager, rootAssignment, managingShapeTree, matchingFocusNode);

        // If the primary resource is a container, and its shape tree specifies its contents with st:contains
        // Recursively traverse the hierarchy and perform shape tree assignment
        if (manageableInstance.getManageableResource().isContainer() && !managingShapeTree.getContains().isEmpty()) {

            // If the container is not empty, perform a recursive, depth first validation and assignment for each
            // contained resource by recursively calling this method (assignShapeTreeToResource)
            List<ManageableInstance> containedInstances = manageableInstance.getContainedInstances();
            if (!containedInstances.isEmpty()) {
                Collections.sort(containedInstances, new ResourceTypeAssignmentPriority());  // Evaluate containers, then resources
                for (ManageableInstance containedResource : containedInstances) {
                    result = assign(accessor, containedResource, shapeTreeContext, null, rootAssignment, managingAssignment, null);
                    if (!result.isValid()) { return result; }
                }
            }
        }

        if (manageableInstance.getManagerResource().isExists()) {
            // update manager resource
            accessor.updateResource(shapeTreeContext, manageableInstance.getManagerResource(), shapeTreeManager.getGraph().toString());
        } else {
            // create manager resource
            ResourceAttributes headers = new ResourceAttributes();
            headers.setAll(HttpHeader.CONTENT_TYPE.getValue(), Collections.singletonList(TEXT_TURTLE));
            accessor.createResource(shapeTreeContext, manageableInstance.getManagerResource().getUrl(), headers, shapeTreeManager.getGraph().toString(), TEXT_TURTLE);
        }

        return new ValidationResult(true, "Assignment successful.");

    }

    public static ValidationResult unassign(ResourceAccessor accessor,
                                            ManageableInstance manageableInstance,
                                            ShapeTreeContext shapeTreeContext,
                                            ShapeTreeAssignment rootAssignment) throws ShapeTreeException {

        ensureInstanceResourceExists(manageableInstance.getManageableResource(), "Cannot remove assignment from non-existent managed resource");
        ensureInstanceResourceExists(manageableInstance.getManagerResource(), "Cannot remove assignment from non-existent manager resource");

        ShapeTreeManager shapeTreeManager = manageableInstance.getManagerResource().getManager();
        ShapeTreeAssignment assignmentToRemove = shapeTreeManager.getAssignmentForRoot(rootAssignment);
        ShapeTree assignedShapeTree = ShapeTreeFactory.getShapeTree(assignmentToRemove.getShapeTree());

        // If the managed resource is a container, and its shape tree specifies its contents with st:contains
        // Recursively traverse the hierarchy and perform shape tree unassignment
        if (manageableInstance.getManageableResource().isContainer() && !assignedShapeTree.getContains().isEmpty()) {

            List<ManageableInstance> containedInstances = manageableInstance.getContainedInstances();
            // If the container is not empty
            if (!containedInstances.isEmpty()) {
                // Sort contained resources so that containers are evaluated first, then resources
                Collections.sort(containedInstances, new ResourceTypeAssignmentPriority());
                // Perform a depth first unassignment for each contained resource
                for (ManageableInstance containedResource : containedInstances) {
                    // Recursively call this function on the contained resource
                    ValidationResult result = unassign(accessor, containedResource, shapeTreeContext, rootAssignment);
                    if (!result.isValid()) { return result; }
                }
            }
        }

        shapeTreeManager.removeAssignment(assignmentToRemove);

        if (shapeTreeManager.getAssignments().isEmpty()) {
            DocumentResponse response = accessor.deleteResource(shapeTreeContext, manageableInstance.getManagerResource());
            ensureDeleteIsSuccessful(response);
        } else {
            // Update the existing manager resource for the managed resource
            accessor.updateResource(shapeTreeContext, manageableInstance.getManagerResource(), shapeTreeManager.getGraph().toString());
        }

        return new ValidationResult(true, "Successfully removed assignment from " + shapeTreeManager.getId());
    }

    private static ShapeTreeManager getManagerForAssignment(ManageableInstance manageableInstance,
                                                            ShapeTreeManager rootManager,
                                                            ShapeTreeAssignment rootAssignment) throws ShapeTreeException {

        ShapeTreeManager shapeTreeManager;
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

    private static ShapeTreeAssignment getAssignment(ManageableResource manageableResource,
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

    private static boolean atRootOfPlantHierarchy(ShapeTreeAssignment rootAssignment, ManageableResource manageableResource) {
        return rootAssignment.getManagedResource().equals(manageableResource.getUrl());
    }

    // Return a root shape tree manager associated with a given shape tree assignment
    private static ShapeTreeManager getRootManager(ResourceAccessor accessor, ShapeTreeContext shapeTreeContext, ShapeTreeAssignment assignment) throws ShapeTreeException {

        URL rootAssignmentUrl = assignment.getRootAssignment();
        ManageableInstance instance = getInstance(accessor, shapeTreeContext, rootAssignmentUrl);

        return instance.getManagerResource().getManager();

    }

    // Return a root shape tree manager associated with a given shape tree assignment
    public static ShapeTreeAssignment getRootAssignment(ResourceAccessor accessor, ShapeTreeContext shapeTreeContext, ShapeTreeAssignment assignment) throws ShapeTreeException {

        ShapeTreeManager rootManager = getRootManager(accessor, shapeTreeContext, assignment);

        for (ShapeTreeAssignment rootAssignment : rootManager.getAssignments()) {
            if (rootAssignment.getUrl() != null && rootAssignment.getUrl().equals(assignment.getRootAssignment())) {
                return rootAssignment;
            }
        }
        return null;

    }

    private static List<URL>
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

    private static void ensureValidationResultIsUsableForAssignment(ValidationResult validationResult, String message) throws ShapeTreeException {
        // Null is a usable state of the validation result in the context of assignment
        if (validationResult != null &&
           (validationResult.getMatchingShapeTree() == null ||
            validationResult.getValidatingShapeTree() == null)) {
           throw new ShapeTreeException(400, message);
        }
    }

    private static void ensureInstanceResourceExists(InstanceResource instanceResource, String message) throws ShapeTreeException {
        if (instanceResource == null || !instanceResource.isExists()) {
            throw new ShapeTreeException(404, message);
        }
    }

    private static void ensureRequestResourceIsContainer(ManageableResource shapeTreeResource, String message) throws ShapeTreeException {
        if (!shapeTreeResource.isContainer()) {
            throw new ShapeTreeException(400, message);
        }
    }

    private static void ensureShapeTreeManagerExists(ShapeTreeManager manager, String message) throws ShapeTreeException {
        if (manager == null || manager.getAssignments() == null || manager.getAssignments().isEmpty()) {
            throw new ShapeTreeException(400, message);
        }
    }

    private static void ensureRemovedAssignmentsAreRoot(ShapeTreeManagerDelta delta) throws ShapeTreeException {
        for (ShapeTreeAssignment assignment : delta.getRemovedAssignments()) {
            if (!assignment.isRootAssignment()) {
                throw new ShapeTreeException(422, "Cannot remove non-root assignment: " + assignment.getUrl().toString() + ". Must unplant root assignment at: " + assignment.getRootAssignment().toString());
            }
        }
    }

    private static void ensureUpdatedAssignmentIsRoot(ShapeTreeManagerDelta delta) throws ShapeTreeException {
        for (ShapeTreeAssignment updatedAssignment : delta.getUpdatedAssignments()) {
            if (!updatedAssignment.isRootAssignment()) {
                throw new ShapeTreeException(500, "Cannot update non-root assignment: " + updatedAssignment.getUrl().toString() + ". Must update root assignment at: " + updatedAssignment.getRootAssignment().toString());
            }
        }
    }

    private static void ensureDeleteIsSuccessful(DocumentResponse response) throws ShapeTreeException {
        List<Integer> successCodes = Arrays.asList(202,204,200);
        if (!successCodes.contains(response.getStatusCode())) {
            throw new ShapeTreeException(500, "Failed to delete manager resource. Received " + response.getStatusCode() + ": " + response.getBody());
        }
    }

}
