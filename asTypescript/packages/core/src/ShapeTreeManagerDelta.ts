// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.core
import { ShapeTreeException } from './exceptions/ShapeTreeException';
import * as URI from 'java/net';
import * as URISyntaxException from 'java/net';
import { ShapeTreeAssignment } from './ShapeTreeAssignment';
import { ShapeTreeManager } from './ShapeTreeManager';

export class ShapeTreeManagerDelta {

   existingManager: ShapeTreeManager;

   updatedManager: ShapeTreeManager;

   updatedAssignments: Array<ShapeTreeAssignment>;

   removedAssignments: Array<ShapeTreeAssignment>;

  /**
   * Compares an updated ShapeTreeManager (updatedManager) with an existing one (existingManager). Neither may
   * be null, managers with no assignments are acceptable for the purposes of comparison.
   * @param existingManager
   * @param updatedManager
   * @return ShapeTreeManagerDelta
   */
  public static evaluate(existingManager: ShapeTreeManager, updatedManager: ShapeTreeManager): ShapeTreeManagerDelta /* throws ShapeTreeException */ {
    if (existingManager === null && updatedManager === null) {
      throw new ShapeTreeException(422, "Cannot compare two null managers");
    }
    let delta: ShapeTreeManagerDelta = new ShapeTreeManagerDelta();
    delta.existingManager = existingManager;
    delta.updatedManager = updatedManager;
    delta.updatedAssignments = new Array<>();
    delta.removedAssignments = new Array<>();
    if (updatedManager === null || updatedManager.getAssignments().isEmpty()) {
      // All assignments have been removed in the updated manager, so any existing assignments should
      // similarly be removed. No need for further comparison.
      delta.removedAssignments = existingManager.getAssignments();
      return delta;
    }
    if (existingManager === null || existingManager.getAssignments().isEmpty()) {
      // This existing manager doesn't have any assignments (which means it shouldn't exist)
      // Anything in the updated manager is being added as new. No need for further comparison.
      delta.updatedAssignments = updatedManager.getAssignments();
      return delta;
    }
    for (const existingAssignment of existingManager.getAssignments()) {
      // Assignments match, and are unchanged, so continue
      if (updatedManager.getAssignments().contains(existingAssignment)) {
        continue;
      }
      // Assignments have the same URL but are different, so update
      let updatedAssignment: ShapeTreeAssignment = containsSameUrl(existingAssignment, updatedManager.getAssignments());
      if (updatedAssignment != null) {
        delta.updatedAssignments.add(updatedAssignment);
        continue;
      }
      // existing assignment isn't in the updated assignment, so remove
      delta.removedAssignments.add(existingAssignment);
    }
    for (const updatedAssignment of updatedManager.getAssignments()) {
      // Assignments match, and are unchanged, so continue
      if (existingManager.getAssignments().contains(updatedAssignment)) {
        continue;
      }
      // If this was already processed and marked as updated continue
      if (delta.updatedAssignments.contains(updatedAssignment)) {
        continue;
      }
      // updated assignment isn't in the existing assignments, so it is new, add it
      delta.updatedAssignments.add(updatedAssignment);
    }
    return delta;
  }

  public static containsSameUrl(assignment: ShapeTreeAssignment, targetAssignments: Array<ShapeTreeAssignment>): ShapeTreeAssignment /* throws ShapeTreeException */ {
    for (const targetAssignment of targetAssignments) {
      let assignmentUri: URI;
      let targetAssignmentUri: URI;
      try {
        assignmentUri = assignment.getUrl().toURI();
        targetAssignmentUri = targetAssignment.getUrl().toURI();
      } catch (ex) {
 if (ex instanceof URISyntaxException) {
         throw new ShapeTreeException(500, "Unable to convert assignment URLs for comparison: " + ex.getMessage());
       }
      if (assignmentUri === targetAssignmentUri) {
        return targetAssignment;
      }
    }
    return null;
  }

  public allRemoved(): boolean {
    return (!this.isUpdated() && this.removedAssignments.size() === this.existingManager.getAssignments().size());
  }

  public isUpdated(): boolean {
    return !this.updatedAssignments.isEmpty();
  }

  public wasReduced(): boolean {
    return !this.removedAssignments.isEmpty();
  }

  public getExistingManager(): ShapeTreeManager {
    return this.existingManager;
  }

  public getUpdatedManager(): ShapeTreeManager {
    return this.updatedManager;
  }

  public getUpdatedAssignments(): Array<ShapeTreeAssignment> {
    return this.updatedAssignments;
  }

  public getRemovedAssignments(): Array<ShapeTreeAssignment> {
    return this.removedAssignments;
  }
}
