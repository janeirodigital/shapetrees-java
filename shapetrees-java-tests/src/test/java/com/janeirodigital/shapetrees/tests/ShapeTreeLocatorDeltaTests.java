package com.janeirodigital.shapetrees.tests;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.ShapeTreeLocation;
import com.janeirodigital.shapetrees.core.models.ShapeTreeLocator;
import com.janeirodigital.shapetrees.core.models.ShapeTreeLocatorDelta;
import jdk.jfr.Label;
import lombok.SneakyThrows;
import org.junit.jupiter.api.*;

import java.net.URI;
import java.net.URISyntaxException;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class ShapeTreeLocatorDeltaTests {

    private static ShapeTreeLocator existingLocator = null;
    private static ShapeTreeLocator updatedLocator = null;
    private static ShapeTreeLocation locationOne = null;
    private static ShapeTreeLocation locationTwo = null;
    private static ShapeTreeLocation locationThree = null;
    private static ShapeTreeLocation locationFour = null;
    private static ShapeTreeLocation locationFive = null;

    @BeforeEach
    void beforeEach() throws ShapeTreeException, URISyntaxException {

        existingLocator = new ShapeTreeLocator();
        updatedLocator = new ShapeTreeLocator();
        
        existingLocator.setId(URI.create("https://locator.example/#existing"));
        updatedLocator.setId(URI.create("https://locator.example/#updated"));

        locationOne = new ShapeTreeLocation(
                "http://shapetrees.example/#firstTree", // ShapeTree
                "http://data.example/resourceOne", // ManagedResource
                URI.create("http://data.example/resourceOne.shapetree#locationOne"), // RootShapeTreeLocation
                "http://data.example/resourceOne#focus", // FocusNode
                "http://shapes.example/#firstShape", // Shape
                URI.create("http://data.example/resourceOne.shapetree#locationOne") // Uri
        );

        locationTwo = new ShapeTreeLocation(
                "http://shapetrees.example/#secondTree", // ShapeTree
                "http://data.example/resourceTwo", // ManagedResource
                URI.create("http://data.example/resourceTwo.shapetree#locationTwo"), // RootShapeTreeLocation
                "http://data.example/resourceTwo#focus", // FocusNode
                "http://shapes.example/#secondShape", // Shape
                URI.create("http://data.example/resourceTwo.shapetree#locationTwo") // Uri
        );

        locationThree = new ShapeTreeLocation(
                "http://shapetrees.example/#thirdTree", // ShapeTree
                "http://data.example/resourceThree", // ManagedResource
                URI.create("http://data.example/resourceThree.shapetree#locationThree"), // RootShapeTreeLocation
                "http://data.example/resourceThree#focus", // FocusNode
                "http://shapes.example/#thirdShape", // Shape
                URI.create("http://data.example/resourceThree.shapetree#locationThree") // Uri
        );

        locationFour = new ShapeTreeLocation(
                "http://shapetrees.example/#fourthTree", // ShapeTree
                "http://data.example/resourceFour", // ManagedResource
                URI.create("http://data.example/resourceFour.shapetree#locationFour"), // RootShapeTreeLocation
                "http://data.example/resourceFour#focus", // FocusNode
                "http://shapes.example/#fourthShape", // Shape
                URI.create("http://data.example/resourceFour.shapetree#locationFour") // Uri
        );

        locationFive = new ShapeTreeLocation(
                "http://shapetrees.example/#fifthTree", // ShapeTree
                "http://data.example/resourceFive", // ManagedResource
                URI.create("http://data.example/resourceFive.shapetree#locationFive"), // RootShapeTreeLocation
                "http://data.example/resourceFive#focus", // FocusNode
                "http://shapes.example/#fifthShape", // Shape
                URI.create("http://data.example/resourceFive.shapetree#locationFive") // Uri
        );
    }

    @SneakyThrows
    @Test
    @Label("Delete all existing locations")
    void deleteAllExistingLocations() {

        // Compare an existing locator with multiple locations with an empty updated locator
        // This should show that all locations are removed with none left

        existingLocator.addShapeTreeLocation(locationOne);
        existingLocator.addShapeTreeLocation(locationTwo);

        ShapeTreeLocatorDelta delta = ShapeTreeLocatorDelta.evaluate(existingLocator, updatedLocator);

        Assertions.assertTrue(delta.getUpdatedLocations().isEmpty());
        Assertions.assertEquals(2, delta.getRemovedLocations().size());
        Assertions.assertTrue(delta.allRemoved());
        Assertions.assertTrue(delta.getRemovedLocations().contains(locationOne));
        Assertions.assertTrue(delta.getRemovedLocations().contains(locationTwo));

    }

    @SneakyThrows
    @Test
    @Label("Delete existing locations and add new ones")
    void deleteAllExistingLocationsAndAddNew() {

        existingLocator.addShapeTreeLocation(locationOne);
        existingLocator.addShapeTreeLocation(locationTwo);
        updatedLocator.addShapeTreeLocation(locationThree);

        ShapeTreeLocatorDelta delta = ShapeTreeLocatorDelta.evaluate(existingLocator, updatedLocator);

        Assertions.assertTrue(delta.isUpdated());
        Assertions.assertTrue(delta.wasReduced());
        Assertions.assertFalse(delta.allRemoved());
        Assertions.assertEquals(1, delta.getUpdatedLocations().size());
        Assertions.assertEquals(2, delta.getRemovedLocations().size());
        Assertions.assertTrue(delta.getUpdatedLocations().contains(locationThree));
        Assertions.assertTrue(delta.getRemovedLocations().contains(locationOne));
        Assertions.assertTrue(delta.getRemovedLocations().contains(locationTwo));


    }

    @SneakyThrows
    @Test
    @Label("Delete a location, update another, and add one")
    void deleteUpdateAndAddLocations() {

        // remove location one
        // update location two
        // add location four

        ShapeTreeLocation locationThreeUpdated = duplicateLocation(locationThree, "http://shapetrees.pub/appleTree", null);

        existingLocator.addShapeTreeLocation(locationOne);
        existingLocator.addShapeTreeLocation(locationTwo);
        existingLocator.addShapeTreeLocation(locationThree);

        updatedLocator.addShapeTreeLocation(locationTwo);
        updatedLocator.addShapeTreeLocation(locationThreeUpdated);
        updatedLocator.addShapeTreeLocation(locationFour);

        ShapeTreeLocatorDelta delta = ShapeTreeLocatorDelta.evaluate(existingLocator, updatedLocator);

        Assertions.assertTrue(delta.isUpdated());
        Assertions.assertTrue(delta.wasReduced());
        Assertions.assertFalse(delta.allRemoved());
        Assertions.assertEquals(2, delta.getUpdatedLocations().size());
        Assertions.assertEquals(1, delta.getRemovedLocations().size());
        Assertions.assertTrue(delta.getUpdatedLocations().contains(locationThreeUpdated));
        Assertions.assertTrue(delta.getUpdatedLocations().contains(locationFour));
        Assertions.assertTrue(delta.getRemovedLocations().contains(locationOne));

    }

    @SneakyThrows
    @Test
    @Label("Update location and add another")
    void updateLocationAndAddAnother() {

        ShapeTreeLocation locationThreeUpdated = duplicateLocation(locationThree, "http://shapetrees.pub/appleTree", null);

        existingLocator.addShapeTreeLocation(locationThree);

        updatedLocator.addShapeTreeLocation(locationThreeUpdated);
        updatedLocator.addShapeTreeLocation(locationFour);

        ShapeTreeLocatorDelta delta = ShapeTreeLocatorDelta.evaluate(existingLocator, updatedLocator);

        Assertions.assertTrue(delta.isUpdated());
        Assertions.assertFalse(delta.wasReduced());
        Assertions.assertFalse(delta.allRemoved());
        Assertions.assertEquals(2, delta.getUpdatedLocations().size());
        Assertions.assertEquals(0, delta.getRemovedLocations().size());
        Assertions.assertTrue(delta.getUpdatedLocations().contains(locationThreeUpdated));
        Assertions.assertTrue(delta.getUpdatedLocations().contains(locationFour));

    }

    @SneakyThrows
    @Test
    @Label("Delete location and update another")
    void DeleteLocationAndUpdateAnother() {

        ShapeTreeLocation locationThreeUpdated = duplicateLocation(locationThree, "http://shapetrees.pub/appleTree", null);

        existingLocator.addShapeTreeLocation(locationTwo);
        existingLocator.addShapeTreeLocation(locationThree);

        updatedLocator.addShapeTreeLocation(locationThreeUpdated);

        ShapeTreeLocatorDelta delta = ShapeTreeLocatorDelta.evaluate(existingLocator, updatedLocator);

        Assertions.assertTrue(delta.isUpdated());
        Assertions.assertTrue(delta.wasReduced());
        Assertions.assertFalse(delta.allRemoved());
        Assertions.assertEquals(1, delta.getUpdatedLocations().size());
        Assertions.assertEquals(1, delta.getRemovedLocations().size());
        Assertions.assertTrue(delta.getUpdatedLocations().contains(locationThreeUpdated));
        Assertions.assertTrue(delta.getRemovedLocations().contains(locationTwo));

    }

    @SneakyThrows
    @Test
    @Label("Add a new locations to an empty set")
    void AddNewLocationToEmptySet() {

        updatedLocator.addShapeTreeLocation(locationOne);
        updatedLocator.addShapeTreeLocation(locationTwo);

        ShapeTreeLocatorDelta delta = ShapeTreeLocatorDelta.evaluate(existingLocator, updatedLocator);

        Assertions.assertTrue(delta.isUpdated());
        Assertions.assertFalse(delta.wasReduced());
        Assertions.assertFalse(delta.allRemoved());
        Assertions.assertEquals(2, delta.getUpdatedLocations().size());
        Assertions.assertEquals(0, delta.getRemovedLocations().size());
        Assertions.assertTrue(delta.getUpdatedLocations().contains(locationOne));
        Assertions.assertTrue(delta.getUpdatedLocations().contains(locationTwo));

    }

    @SneakyThrows
    @Test
    @Label("Update existing locations")
    void UpdateExistingLocation() {

        ShapeTreeLocation locationOneUpdated = duplicateLocation(locationOne, null, "http://data.example/resourceOne#Otherfocus");

        ShapeTreeLocation locationTwoUpdated = duplicateLocation(locationTwo, null, "http://data.example/resourceTwo#Otherfocus");

        existingLocator.addShapeTreeLocation(locationOne);
        existingLocator.addShapeTreeLocation(locationTwo);

        updatedLocator.addShapeTreeLocation(locationOneUpdated);
        updatedLocator.addShapeTreeLocation(locationTwoUpdated);

        ShapeTreeLocatorDelta delta = ShapeTreeLocatorDelta.evaluate(existingLocator, updatedLocator);

        Assertions.assertTrue(delta.isUpdated());
        Assertions.assertFalse(delta.wasReduced());
        Assertions.assertFalse(delta.allRemoved());
        Assertions.assertEquals(2, delta.getUpdatedLocations().size());
        Assertions.assertEquals(0, delta.getRemovedLocations().size());
        Assertions.assertTrue(delta.getUpdatedLocations().contains(locationOneUpdated));
        Assertions.assertTrue(delta.getUpdatedLocations().contains(locationTwoUpdated));
        
    }

    @Test
    @Label("Compare two null locators")
    void compareTwoNullLocators() {
        Assertions.assertThrows(ShapeTreeException.class, () -> ShapeTreeLocatorDelta.evaluate(null, null));
    }

    @SneakyThrows
    @Test
    @Label("Check null values on updated locator")
    void checkNullsOnUpdatedLocator() {

        existingLocator.addShapeTreeLocation(locationOne);
        existingLocator.addShapeTreeLocation(locationTwo);
        ShapeTreeLocatorDelta delta = ShapeTreeLocatorDelta.evaluate(existingLocator, null);
        Assertions.assertTrue(delta.allRemoved());

        updatedLocator.getLocations().clear();
        delta = ShapeTreeLocatorDelta.evaluate(existingLocator, updatedLocator);
        Assertions.assertTrue(delta.allRemoved());

        updatedLocator.setLocations(null);
        delta = ShapeTreeLocatorDelta.evaluate(existingLocator, updatedLocator);
        Assertions.assertTrue(delta.allRemoved());

    }

    @SneakyThrows
    @Test
    @Label("Check null values on existing locator")
    void checkNullsOnExistingLocator() {

        updatedLocator.addShapeTreeLocation(locationOne);
        updatedLocator.addShapeTreeLocation(locationTwo);
        ShapeTreeLocatorDelta delta = ShapeTreeLocatorDelta.evaluate(null, updatedLocator);
        Assertions.assertTrue(delta.isUpdated());

        existingLocator.getLocations().clear();
        delta = ShapeTreeLocatorDelta.evaluate(existingLocator, updatedLocator);
        Assertions.assertTrue(delta.isUpdated());

        existingLocator.setLocations(null);
        delta = ShapeTreeLocatorDelta.evaluate(existingLocator, updatedLocator);
        Assertions.assertTrue(delta.isUpdated());

    }

    private ShapeTreeLocation duplicateLocation(ShapeTreeLocation location, final String shapeTree, final String focusNode) {

        ShapeTreeLocation duplicateLocation = new ShapeTreeLocation(
                shapeTree != null ? shapeTree : location.getShapeTree(),
                location.getManagedResource(),
                location.getRootShapeTreeLocation(),
                focusNode != null ? focusNode : location.getFocusNode(),
                location.getShape(),
                location.getUri()
        );
        return duplicateLocation;

    }

}
