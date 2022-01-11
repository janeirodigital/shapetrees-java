package com.janeirodigital.shapetrees.tests;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.validation.ShapeTreeAssignment;
import com.janeirodigital.shapetrees.core.validation.ShapeTreeManager;
import com.janeirodigital.shapetrees.core.validation.ShapeTreeManagerDelta;
import lombok.SneakyThrows;
import org.junit.jupiter.api.*;

import java.net.URL;
import java.net.MalformedURLException;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class ShapeTreeManagerDeltaTests {

    private static ShapeTreeManager existingManager = null;
    private static ShapeTreeManager updatedManager = null;
    private static ShapeTreeAssignment assignmentOne = null;
    private static ShapeTreeAssignment assignmentTwo = null;
    private static ShapeTreeAssignment assignmentThree = null;
    private static ShapeTreeAssignment assignmentFour = null;
    private static ShapeTreeAssignment assignmentFive = null;

    @BeforeEach
    void beforeEach() throws ShapeTreeException, MalformedURLException {

        existingManager = new ShapeTreeManager(new URL("https://manager.example/#existing"));
        updatedManager = new ShapeTreeManager(new URL("https://manager.example/#updated"));
        
        assignmentOne = new ShapeTreeAssignment(
                new URL("http://shapetrees.example/#firstTree"), // ShapeTree
                new URL("http://data.example/resourceOne"), // ManageableResource
                new URL("http://data.example/resourceOne.shapetree#assignmentOne"), // RootAssignment
                new URL("http://data.example/resourceOne#focus"), // FocusNode
                new URL("http://shapes.example/#firstShape"), // Shape
                new URL("http://data.example/resourceOne.shapetree#assignmentOne") // Uri
        );

        assignmentTwo = new ShapeTreeAssignment(
                new URL("http://shapetrees.example/#secondTree"), // ShapeTree
                new URL("http://data.example/resourceTwo"), // ManageableResource
                new URL("http://data.example/resourceTwo.shapetree#assignmentTwo"), // RootAssignment
                new URL("http://data.example/resourceTwo#focus"), // FocusNode
                new URL("http://shapes.example/#secondShape"), // Shape
                new URL("http://data.example/resourceTwo.shapetree#assignmentTwo") // Uri
        );

        assignmentThree = new ShapeTreeAssignment(
                new URL("http://shapetrees.example/#thirdTree"), // ShapeTree
                new URL("http://data.example/resourceThree"), // ManageableResource
                new URL("http://data.example/resourceThree.shapetree#assignmentThree"), // RootAssignment
                new URL("http://data.example/resourceThree#focus"), // FocusNode
                new URL("http://shapes.example/#thirdShape"), // Shape
                new URL("http://data.example/resourceThree.shapetree#assignmentThree") // Uri
        );

        assignmentFour = new ShapeTreeAssignment(
                new URL("http://shapetrees.example/#fourthTree"), // ShapeTree
                new URL("http://data.example/resourceFour"), // ManageableResource
                new URL("http://data.example/resourceFour.shapetree#assignmentFour"), // RootAssignment
                new URL("http://data.example/resourceFour#focus"), // FocusNode
                new URL("http://shapes.example/#fourthShape"), // Shape
                new URL("http://data.example/resourceFour.shapetree#assignmentFour") // Uri
        );

        assignmentFive = new ShapeTreeAssignment(
                new URL("http://shapetrees.example/#fifthTree"), // ShapeTree
                new URL("http://data.example/resourceFive"), // ManageableResource
                new URL("http://data.example/resourceFive.shapetree#assignmentFive"), // RootAssignment
                new URL("http://data.example/resourceFive#focus"), // FocusNode
                new URL("http://shapes.example/#fifthShape"), // Shape
                new URL("http://data.example/resourceFive.shapetree#assignmentFive") // Uri
        );
    }

    @SneakyThrows
    @Test
    @DisplayName("Delete all existing assignments")
    void deleteAllExistingAssignments() {

        // Compare an existing manager with multiple assignments with an empty updated manager
        // This should show that all assignments are removed with none left

        existingManager.addAssignment(assignmentOne);
        existingManager.addAssignment(assignmentTwo);

        ShapeTreeManagerDelta delta = ShapeTreeManagerDelta.evaluate(existingManager, updatedManager);

        Assertions.assertTrue(delta.getUpdatedAssignments().isEmpty());
        Assertions.assertEquals(2, delta.getRemovedAssignments().size());
        Assertions.assertTrue(delta.allRemoved());
        Assertions.assertTrue(delta.getRemovedAssignments().contains(assignmentOne));
        Assertions.assertTrue(delta.getRemovedAssignments().contains(assignmentTwo));

    }

    @SneakyThrows
    @Test
    @DisplayName("Delete existing assignments and add new ones")
    void deleteAllExistingAssignmentsAndAddNew() {

        existingManager.addAssignment(assignmentOne);
        existingManager.addAssignment(assignmentTwo);
        updatedManager.addAssignment(assignmentThree);

        ShapeTreeManagerDelta delta = ShapeTreeManagerDelta.evaluate(existingManager, updatedManager);

        Assertions.assertTrue(delta.isUpdated());
        Assertions.assertTrue(delta.wasReduced());
        Assertions.assertFalse(delta.allRemoved());
        Assertions.assertEquals(1, delta.getUpdatedAssignments().size());
        Assertions.assertEquals(2, delta.getRemovedAssignments().size());
        Assertions.assertTrue(delta.getUpdatedAssignments().contains(assignmentThree));
        Assertions.assertTrue(delta.getRemovedAssignments().contains(assignmentOne));
        Assertions.assertTrue(delta.getRemovedAssignments().contains(assignmentTwo));


    }

    @SneakyThrows
    @Test
    @DisplayName("Delete an assignment, update another, and add one")
    void deleteUpdateAndAddAssignments() {

        // remove assignment one
        // update assignment two
        // add assignment four

        ShapeTreeAssignment assignmentThreeUpdated = duplicateAssignment(assignmentThree, new URL("http://shapetrees.pub/appleTree"), null);

        existingManager.addAssignment(assignmentOne);
        existingManager.addAssignment(assignmentTwo);
        existingManager.addAssignment(assignmentThree);

        updatedManager.addAssignment(assignmentTwo);
        updatedManager.addAssignment(assignmentThreeUpdated);
        updatedManager.addAssignment(assignmentFour);

        ShapeTreeManagerDelta delta = ShapeTreeManagerDelta.evaluate(existingManager, updatedManager);

        Assertions.assertTrue(delta.isUpdated());
        Assertions.assertTrue(delta.wasReduced());
        Assertions.assertFalse(delta.allRemoved());
        Assertions.assertEquals(2, delta.getUpdatedAssignments().size());
        Assertions.assertEquals(1, delta.getRemovedAssignments().size());
        Assertions.assertTrue(delta.getUpdatedAssignments().contains(assignmentThreeUpdated));
        Assertions.assertTrue(delta.getUpdatedAssignments().contains(assignmentFour));
        Assertions.assertTrue(delta.getRemovedAssignments().contains(assignmentOne));

    }

    @SneakyThrows
    @Test
    @DisplayName("Update assignment and add another")
    void updateAssignmentAndAddAnother() {

        ShapeTreeAssignment assignmentThreeUpdated = duplicateAssignment(assignmentThree, new URL("http://shapetrees.pub/appleTree"), null);

        existingManager.addAssignment(assignmentThree);

        updatedManager.addAssignment(assignmentThreeUpdated);
        updatedManager.addAssignment(assignmentFour);

        ShapeTreeManagerDelta delta = ShapeTreeManagerDelta.evaluate(existingManager, updatedManager);

        Assertions.assertTrue(delta.isUpdated());
        Assertions.assertFalse(delta.wasReduced());
        Assertions.assertFalse(delta.allRemoved());
        Assertions.assertEquals(2, delta.getUpdatedAssignments().size());
        Assertions.assertEquals(0, delta.getRemovedAssignments().size());
        Assertions.assertTrue(delta.getUpdatedAssignments().contains(assignmentThreeUpdated));
        Assertions.assertTrue(delta.getUpdatedAssignments().contains(assignmentFour));

    }

    @SneakyThrows
    @Test
    @DisplayName("Delete assignment and update another")
    void DeleteAssignmentAndUpdateAnother() {

        ShapeTreeAssignment assignmentThreeUpdated = duplicateAssignment(assignmentThree, new URL("http://shapetrees.pub/appleTree"), null);

        existingManager.addAssignment(assignmentTwo);
        existingManager.addAssignment(assignmentThree);

        updatedManager.addAssignment(assignmentThreeUpdated);

        ShapeTreeManagerDelta delta = ShapeTreeManagerDelta.evaluate(existingManager, updatedManager);

        Assertions.assertTrue(delta.isUpdated());
        Assertions.assertTrue(delta.wasReduced());
        Assertions.assertFalse(delta.allRemoved());
        Assertions.assertEquals(1, delta.getUpdatedAssignments().size());
        Assertions.assertEquals(1, delta.getRemovedAssignments().size());
        Assertions.assertTrue(delta.getUpdatedAssignments().contains(assignmentThreeUpdated));
        Assertions.assertTrue(delta.getRemovedAssignments().contains(assignmentTwo));

    }

    @SneakyThrows
    @Test
    @DisplayName("Add a new assignments to an empty set")
    void AddNewAssignmentToEmptySet() {

        updatedManager.addAssignment(assignmentOne);
        updatedManager.addAssignment(assignmentTwo);

        ShapeTreeManagerDelta delta = ShapeTreeManagerDelta.evaluate(existingManager, updatedManager);

        Assertions.assertTrue(delta.isUpdated());
        Assertions.assertFalse(delta.wasReduced());
        Assertions.assertFalse(delta.allRemoved());
        Assertions.assertEquals(2, delta.getUpdatedAssignments().size());
        Assertions.assertEquals(0, delta.getRemovedAssignments().size());
        Assertions.assertTrue(delta.getUpdatedAssignments().contains(assignmentOne));
        Assertions.assertTrue(delta.getUpdatedAssignments().contains(assignmentTwo));

    }

    @SneakyThrows
    @Test
    @DisplayName("Update existing assignments")
    void UpdateExistingAssignment() {

        ShapeTreeAssignment assignmentOneUpdated = duplicateAssignment(assignmentOne, null, new URL("http://data.example/resourceOne#Otherfocus"));

        ShapeTreeAssignment assignmentTwoUpdated = duplicateAssignment(assignmentTwo, null, new URL("http://data.example/resourceTwo#Otherfocus"));

        existingManager.addAssignment(assignmentOne);
        existingManager.addAssignment(assignmentTwo);

        updatedManager.addAssignment(assignmentOneUpdated);
        updatedManager.addAssignment(assignmentTwoUpdated);

        ShapeTreeManagerDelta delta = ShapeTreeManagerDelta.evaluate(existingManager, updatedManager);

        Assertions.assertTrue(delta.isUpdated());
        Assertions.assertFalse(delta.wasReduced());
        Assertions.assertFalse(delta.allRemoved());
        Assertions.assertEquals(2, delta.getUpdatedAssignments().size());
        Assertions.assertEquals(0, delta.getRemovedAssignments().size());
        Assertions.assertTrue(delta.getUpdatedAssignments().contains(assignmentOneUpdated));
        Assertions.assertTrue(delta.getUpdatedAssignments().contains(assignmentTwoUpdated));
        
    }

    @Test
    @DisplayName("Compare two null managers")
    void compareTwoNullManagers() {
        Assertions.assertThrows(ShapeTreeException.class, () -> ShapeTreeManagerDelta.evaluate(null, null));
    }

    @SneakyThrows
    @Test
    @DisplayName("Check null values on updated manager")
    void checkNullsOnUpdatedManager() {

        existingManager.addAssignment(assignmentOne);
        existingManager.addAssignment(assignmentTwo);
        ShapeTreeManagerDelta delta = ShapeTreeManagerDelta.evaluate(existingManager, null);
        Assertions.assertTrue(delta.allRemoved());

        updatedManager.getAssignments().clear();
        delta = ShapeTreeManagerDelta.evaluate(existingManager, updatedManager);
        Assertions.assertTrue(delta.allRemoved());

    }

    @SneakyThrows
    @Test
    @DisplayName("Check null values on existing manager")
    void checkNullsOnExistingManager() {

        updatedManager.addAssignment(assignmentOne);
        updatedManager.addAssignment(assignmentTwo);
        ShapeTreeManagerDelta delta = ShapeTreeManagerDelta.evaluate(null, updatedManager);
        Assertions.assertTrue(delta.isUpdated());

        existingManager.getAssignments().clear();
        delta = ShapeTreeManagerDelta.evaluate(existingManager, updatedManager);
        Assertions.assertTrue(delta.isUpdated());

    }

    private ShapeTreeAssignment duplicateAssignment(ShapeTreeAssignment assignment, final URL shapeTree, final URL focusNode) throws MalformedURLException, ShapeTreeException {

        ShapeTreeAssignment duplicateAssignment = new ShapeTreeAssignment(
                shapeTree != null ? shapeTree : assignment.getShapeTree(),
                assignment.getManagedResource(),
                assignment.getRootAssignment(),
                focusNode != null ? focusNode : assignment.getFocusNode(),
                assignment.getShape(),
                assignment.getUrl()
        );
        return duplicateAssignment;

    }

}
