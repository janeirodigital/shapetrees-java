package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import lombok.Getter;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;

@Getter
public class ShapeTreeManagerDelta {

    ShapeTreeManager existingManager;
    ShapeTreeManager updatedManager;
    List<ShapeTreeAssignment> updatedAssignments;
    List<ShapeTreeAssignment> removedAssignments;

    /**
     * Compares an updated ShapeTreeManager (updatedManager) with an existing one (existingManager). Neither may
     * be null, managers with no assignments are acceptable for the purposes of comparison.
     * @param existingManager
     * @param updatedManager
     * @return ShapeTreeManagerDelta
     */
    public static ShapeTreeManagerDelta evaluate(ShapeTreeManager existingManager, ShapeTreeManager updatedManager) throws ShapeTreeException {

        if (existingManager == null && updatedManager == null) {
            throw new ShapeTreeException(422, "Cannot compare two null managers");
        }

        ShapeTreeManagerDelta delta = new ShapeTreeManagerDelta();

        delta.existingManager = existingManager;
        delta.updatedManager = updatedManager;
        delta.updatedAssignments = new ArrayList<>();
        delta.removedAssignments = new ArrayList<>();


        if (updatedManager == null || updatedManager.getAssignments() == null || updatedManager.getAssignments().isEmpty()) {
            // All assignments have been removed in the updated manager, so any existing assignments should
            // similarly be removed. No need for further comparison.
            delta.removedAssignments = existingManager.getAssignments();
            return delta;
        }

        if (existingManager == null || existingManager.getAssignments() == null || existingManager.getAssignments().isEmpty()) {
            // This existing manager doesn't have any assignments (which means it shouldn't exist)
            // Anything in the updated manager is being added as new. No need for further comparison.
            delta.updatedAssignments = updatedManager.getAssignments();
            return delta;
        }

        for (ShapeTreeAssignment existingAssignment : existingManager.getAssignments()) {

            // Assignments match, and are unchanged, so continue
            if (updatedManager.getAssignments().contains(existingAssignment)) { continue; }

            // Assignments have the same URL but are different, so update
            ShapeTreeAssignment updatedAssignment = containsSameUrl(existingAssignment, updatedManager.getAssignments());

            if (updatedAssignment != null) {
                delta.updatedAssignments.add(updatedAssignment);
                continue;
            }

            // existing assignment isn't in the updated assignment, so remove
            delta.removedAssignments.add(existingAssignment);

        }

        for (ShapeTreeAssignment updatedAssignment : updatedManager.getAssignments()) {

            // Assignments match, and are unchanged, so continue
            if (existingManager.getAssignments().contains(updatedAssignment)) { continue; }

            // If this was already processed and marked as updated continue
            if (delta.updatedAssignments.contains(updatedAssignment)) { continue; }

            // updated assignment isn't in the existing assignments, so it is new, add it
            delta.updatedAssignments.add(updatedAssignment);
        }

        return delta;

    }

    public static ShapeTreeAssignment containsSameUrl(ShapeTreeAssignment assignment, List<ShapeTreeAssignment> targetAssignments) throws ShapeTreeException {
        for (ShapeTreeAssignment targetAssignment : targetAssignments) {
            URI assignmentUri;
            URI targetAssignmentUri;
            try {
                assignmentUri = assignment.getUrl().toURI();
                targetAssignmentUri = targetAssignment.getUrl().toURI();
            } catch (URISyntaxException ex) {
                throw new ShapeTreeException(500, "Unable to convert assignment URLs for comparison: " + ex.getMessage());
            }
            if (assignmentUri.equals(targetAssignmentUri)) { return targetAssignment; }
        }
        return null;
    }

    public boolean allRemoved() {
        return (!this.isUpdated() && this.removedAssignments.size() == this.existingManager.getAssignments().size());
    }

    public boolean isUpdated() {
        return !this.updatedAssignments.isEmpty();
    }

    public boolean wasReduced() {
        return !this.removedAssignments.isEmpty();
    }

}
