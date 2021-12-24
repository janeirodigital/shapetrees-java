// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.tests
import { ShapeTreeException } from '@shapetrees/core/src/exceptions/ShapeTreeException';
import { ShapeTreeAssignment } from '@shapetrees/core/src/ShapeTreeAssignment';
import { ShapeTreeManager } from '@shapetrees/core/src/ShapeTreeManager';
import { ShapeTreeManagerDelta } from '@shapetrees/core/src/ShapeTreeManagerDelta';
import * as Label from 'jdk/jfr';
import * as MalformedURLException from 'java/net';

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class ShapeTreeManagerDeltaTests {

   private static existingManager: ShapeTreeManager = null;

   private static updatedManager: ShapeTreeManager = null;

   private static assignmentOne: ShapeTreeAssignment = null;

   private static assignmentTwo: ShapeTreeAssignment = null;

   private static assignmentThree: ShapeTreeAssignment = null;

   private static assignmentFour: ShapeTreeAssignment = null;

   private static assignmentFive: ShapeTreeAssignment = null;

  // @BeforeEach
  beforeEach(): void /* throws ShapeTreeException, MalformedURLException */ {
    existingManager = new ShapeTreeManager(new URL("https://manager.example/#existing"));
    updatedManager = new ShapeTreeManager(new URL("https://manager.example/#updated"));
    assignmentOne = new ShapeTreeAssignment(// ShapeTree
    new URL("http://shapetrees.example/#firstTree"), // ManageableResource
    new URL("http://data.example/resourceOne"), // RootAssignment
    new URL("http://data.example/resourceOne.shapetree#assignmentOne"), // FocusNode
    new URL("http://data.example/resourceOne#focus"), // Shape
    new URL("http://shapes.example/#firstShape"), // Uri
    new URL("http://data.example/resourceOne.shapetree#assignmentOne"));
    assignmentTwo = new ShapeTreeAssignment(// ShapeTree
    new URL("http://shapetrees.example/#secondTree"), // ManageableResource
    new URL("http://data.example/resourceTwo"), // RootAssignment
    new URL("http://data.example/resourceTwo.shapetree#assignmentTwo"), // FocusNode
    new URL("http://data.example/resourceTwo#focus"), // Shape
    new URL("http://shapes.example/#secondShape"), // Uri
    new URL("http://data.example/resourceTwo.shapetree#assignmentTwo"));
    assignmentThree = new ShapeTreeAssignment(// ShapeTree
    new URL("http://shapetrees.example/#thirdTree"), // ManageableResource
    new URL("http://data.example/resourceThree"), // RootAssignment
    new URL("http://data.example/resourceThree.shapetree#assignmentThree"), // FocusNode
    new URL("http://data.example/resourceThree#focus"), // Shape
    new URL("http://shapes.example/#thirdShape"), // Uri
    new URL("http://data.example/resourceThree.shapetree#assignmentThree"));
    assignmentFour = new ShapeTreeAssignment(// ShapeTree
    new URL("http://shapetrees.example/#fourthTree"), // ManageableResource
    new URL("http://data.example/resourceFour"), // RootAssignment
    new URL("http://data.example/resourceFour.shapetree#assignmentFour"), // FocusNode
    new URL("http://data.example/resourceFour#focus"), // Shape
    new URL("http://shapes.example/#fourthShape"), // Uri
    new URL("http://data.example/resourceFour.shapetree#assignmentFour"));
    assignmentFive = new ShapeTreeAssignment(// ShapeTree
    new URL("http://shapetrees.example/#fifthTree"), // ManageableResource
    new URL("http://data.example/resourceFive"), // RootAssignment
    new URL("http://data.example/resourceFive.shapetree#assignmentFive"), // FocusNode
    new URL("http://data.example/resourceFive#focus"), // Shape
    new URL("http://shapes.example/#fifthShape"), // Uri
    new URL("http://data.example/resourceFive.shapetree#assignmentFive"));
  }

  // @SneakyThrows, @Test, @Label("Delete all existing assignments")
  deleteAllExistingAssignments(): void {
    // Compare an existing manager with multiple assignments with an empty updated manager
    // This should show that all assignments are removed with none left
    existingManager.addAssignment(assignmentOne);
    existingManager.addAssignment(assignmentTwo);
    let delta: ShapeTreeManagerDelta = ShapeTreeManagerDelta.evaluate(existingManager, updatedManager);
    Assertions.assertTrue(delta.getUpdatedAssignments().isEmpty());
    Assertions.assertEquals(2, delta.getRemovedAssignments().size());
    Assertions.assertTrue(delta.allRemoved());
    Assertions.assertTrue(delta.getRemovedAssignments().contains(assignmentOne));
    Assertions.assertTrue(delta.getRemovedAssignments().contains(assignmentTwo));
  }

  // @SneakyThrows, @Test, @Label("Delete existing assignments and add new ones")
  deleteAllExistingAssignmentsAndAddNew(): void {
    existingManager.addAssignment(assignmentOne);
    existingManager.addAssignment(assignmentTwo);
    updatedManager.addAssignment(assignmentThree);
    let delta: ShapeTreeManagerDelta = ShapeTreeManagerDelta.evaluate(existingManager, updatedManager);
    Assertions.assertTrue(delta.isUpdated());
    Assertions.assertTrue(delta.wasReduced());
    Assertions.assertFalse(delta.allRemoved());
    Assertions.assertEquals(1, delta.getUpdatedAssignments().size());
    Assertions.assertEquals(2, delta.getRemovedAssignments().size());
    Assertions.assertTrue(delta.getUpdatedAssignments().contains(assignmentThree));
    Assertions.assertTrue(delta.getRemovedAssignments().contains(assignmentOne));
    Assertions.assertTrue(delta.getRemovedAssignments().contains(assignmentTwo));
  }

  // @SneakyThrows, @Test, @Label("Delete an assignment, update another, and add one")
  deleteUpdateAndAddAssignments(): void {
    // remove assignment one
    // update assignment two
    // add assignment four
    let assignmentThreeUpdated: ShapeTreeAssignment = duplicateAssignment(assignmentThree, new URL("http://shapetrees.pub/appleTree"), null);
    existingManager.addAssignment(assignmentOne);
    existingManager.addAssignment(assignmentTwo);
    existingManager.addAssignment(assignmentThree);
    updatedManager.addAssignment(assignmentTwo);
    updatedManager.addAssignment(assignmentThreeUpdated);
    updatedManager.addAssignment(assignmentFour);
    let delta: ShapeTreeManagerDelta = ShapeTreeManagerDelta.evaluate(existingManager, updatedManager);
    Assertions.assertTrue(delta.isUpdated());
    Assertions.assertTrue(delta.wasReduced());
    Assertions.assertFalse(delta.allRemoved());
    Assertions.assertEquals(2, delta.getUpdatedAssignments().size());
    Assertions.assertEquals(1, delta.getRemovedAssignments().size());
    Assertions.assertTrue(delta.getUpdatedAssignments().contains(assignmentThreeUpdated));
    Assertions.assertTrue(delta.getUpdatedAssignments().contains(assignmentFour));
    Assertions.assertTrue(delta.getRemovedAssignments().contains(assignmentOne));
  }

  // @SneakyThrows, @Test, @Label("Update assignment and add another")
  updateAssignmentAndAddAnother(): void {
    let assignmentThreeUpdated: ShapeTreeAssignment = duplicateAssignment(assignmentThree, new URL("http://shapetrees.pub/appleTree"), null);
    existingManager.addAssignment(assignmentThree);
    updatedManager.addAssignment(assignmentThreeUpdated);
    updatedManager.addAssignment(assignmentFour);
    let delta: ShapeTreeManagerDelta = ShapeTreeManagerDelta.evaluate(existingManager, updatedManager);
    Assertions.assertTrue(delta.isUpdated());
    Assertions.assertFalse(delta.wasReduced());
    Assertions.assertFalse(delta.allRemoved());
    Assertions.assertEquals(2, delta.getUpdatedAssignments().size());
    Assertions.assertEquals(0, delta.getRemovedAssignments().size());
    Assertions.assertTrue(delta.getUpdatedAssignments().contains(assignmentThreeUpdated));
    Assertions.assertTrue(delta.getUpdatedAssignments().contains(assignmentFour));
  }

  // @SneakyThrows, @Test, @Label("Delete assignment and update another")
  DeleteAssignmentAndUpdateAnother(): void {
    let assignmentThreeUpdated: ShapeTreeAssignment = duplicateAssignment(assignmentThree, new URL("http://shapetrees.pub/appleTree"), null);
    existingManager.addAssignment(assignmentTwo);
    existingManager.addAssignment(assignmentThree);
    updatedManager.addAssignment(assignmentThreeUpdated);
    let delta: ShapeTreeManagerDelta = ShapeTreeManagerDelta.evaluate(existingManager, updatedManager);
    Assertions.assertTrue(delta.isUpdated());
    Assertions.assertTrue(delta.wasReduced());
    Assertions.assertFalse(delta.allRemoved());
    Assertions.assertEquals(1, delta.getUpdatedAssignments().size());
    Assertions.assertEquals(1, delta.getRemovedAssignments().size());
    Assertions.assertTrue(delta.getUpdatedAssignments().contains(assignmentThreeUpdated));
    Assertions.assertTrue(delta.getRemovedAssignments().contains(assignmentTwo));
  }

  // @SneakyThrows, @Test, @Label("Add a new assignments to an empty set")
  AddNewAssignmentToEmptySet(): void {
    updatedManager.addAssignment(assignmentOne);
    updatedManager.addAssignment(assignmentTwo);
    let delta: ShapeTreeManagerDelta = ShapeTreeManagerDelta.evaluate(existingManager, updatedManager);
    Assertions.assertTrue(delta.isUpdated());
    Assertions.assertFalse(delta.wasReduced());
    Assertions.assertFalse(delta.allRemoved());
    Assertions.assertEquals(2, delta.getUpdatedAssignments().size());
    Assertions.assertEquals(0, delta.getRemovedAssignments().size());
    Assertions.assertTrue(delta.getUpdatedAssignments().contains(assignmentOne));
    Assertions.assertTrue(delta.getUpdatedAssignments().contains(assignmentTwo));
  }

  // @SneakyThrows, @Test, @Label("Update existing assignments")
  UpdateExistingAssignment(): void {
    let assignmentOneUpdated: ShapeTreeAssignment = duplicateAssignment(assignmentOne, null, new URL("http://data.example/resourceOne#Otherfocus"));
    let assignmentTwoUpdated: ShapeTreeAssignment = duplicateAssignment(assignmentTwo, null, new URL("http://data.example/resourceTwo#Otherfocus"));
    existingManager.addAssignment(assignmentOne);
    existingManager.addAssignment(assignmentTwo);
    updatedManager.addAssignment(assignmentOneUpdated);
    updatedManager.addAssignment(assignmentTwoUpdated);
    let delta: ShapeTreeManagerDelta = ShapeTreeManagerDelta.evaluate(existingManager, updatedManager);
    Assertions.assertTrue(delta.isUpdated());
    Assertions.assertFalse(delta.wasReduced());
    Assertions.assertFalse(delta.allRemoved());
    Assertions.assertEquals(2, delta.getUpdatedAssignments().size());
    Assertions.assertEquals(0, delta.getRemovedAssignments().size());
    Assertions.assertTrue(delta.getUpdatedAssignments().contains(assignmentOneUpdated));
    Assertions.assertTrue(delta.getUpdatedAssignments().contains(assignmentTwoUpdated));
  }

  // @Test, @Label("Compare two null managers")
  compareTwoNullManagers(): void {
    Assertions.assertThrows(ShapeTreeException.class, () -> ShapeTreeManagerDelta.evaluate(null, null));
  }

  // @SneakyThrows, @Test, @Label("Check null values on updated manager")
  checkNullsOnUpdatedManager(): void {
    existingManager.addAssignment(assignmentOne);
    existingManager.addAssignment(assignmentTwo);
    let delta: ShapeTreeManagerDelta = ShapeTreeManagerDelta.evaluate(existingManager, null);
    Assertions.assertTrue(delta.allRemoved());
    updatedManager.getAssignments().clear();
    delta = ShapeTreeManagerDelta.evaluate(existingManager, updatedManager);
    Assertions.assertTrue(delta.allRemoved());
  }

  // @SneakyThrows, @Test, @Label("Check null values on existing manager")
  checkNullsOnExistingManager(): void {
    updatedManager.addAssignment(assignmentOne);
    updatedManager.addAssignment(assignmentTwo);
    let delta: ShapeTreeManagerDelta = ShapeTreeManagerDelta.evaluate(null, updatedManager);
    Assertions.assertTrue(delta.isUpdated());
    existingManager.getAssignments().clear();
    delta = ShapeTreeManagerDelta.evaluate(existingManager, updatedManager);
    Assertions.assertTrue(delta.isUpdated());
  }

  private duplicateAssignment(assignment: ShapeTreeAssignment, /*const*/ shapeTree: URL, /*const*/ focusNode: URL): ShapeTreeAssignment /* throws MalformedURLException, ShapeTreeException */ {
    let duplicateAssignment: ShapeTreeAssignment = new ShapeTreeAssignment(shapeTree != null ? shapeTree : assignment.getShapeTree(), assignment.getManagedResource(), assignment.getRootAssignment(), focusNode != null ? focusNode : assignment.getFocusNode(), assignment.getShape(), assignment.getUrl());
    return duplicateAssignment;
  }
}
