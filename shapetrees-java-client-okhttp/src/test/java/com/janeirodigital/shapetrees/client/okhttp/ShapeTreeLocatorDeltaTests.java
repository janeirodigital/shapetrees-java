package com.janeirodigital.shapetrees.client.okhttp;

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
class ShapeTreeLocatorDeltaTests extends BaseShapeTreeTest {

    private static ShapeTreeLocator existingLocator = null;
    private static ShapeTreeLocator updatedLocator = null;
    private static ShapeTreeLocation locationOne = null;
    private static ShapeTreeLocation locationTwo = null;
    private static ShapeTreeLocation locationThree = null;
    private static ShapeTreeLocation locationFour = null;
    private static ShapeTreeLocation locationFive = null;

    public ShapeTreeLocatorDeltaTests() {
        // Call BaseShapeTreeTest constructor
        super();
    }

    @BeforeEach
    void beforeEach() throws ShapeTreeException, URISyntaxException {

        existingLocator = new ShapeTreeLocator();
        updatedLocator = new ShapeTreeLocator();
        
        existingLocator.setId("https://locator.example/#existing");
        updatedLocator.setId("https://locator.example/#updated");
        
        locationOne = new ShapeTreeLocation();
        locationOne.setShapeTree("http://shapetrees.example/#firstTree");
        locationOne.setManagedResource("http://data.example/resourceOne");
        locationOne.setRootShapeTreeLocation(URI.create("http://data.example/resourceOne.shapetree#locationOne"));
        locationOne.setFocusNode("http://data.example/resourceOne#focus");
        locationOne.setShape("http://shapes.example/#firstShape");
        locationOne.setUri(URI.create("http://data.example/resourceOne.shapetree#locationOne"));

        locationTwo = new ShapeTreeLocation();
        locationTwo.setShapeTree("http://shapetrees.example/#secondTree");
        locationTwo.setManagedResource("http://data.example/resourceTwo");
        locationTwo.setRootShapeTreeLocation(URI.create("http://data.example/resourceTwo.shapetree#locationTwo"));
        locationTwo.setFocusNode("http://data.example/resourceTwo#focus");
        locationTwo.setShape("http://shapes.example/#secondShape");
        locationTwo.setUri(URI.create("http://data.example/resourceTwo.shapetree#locationTwo"));

        locationThree = new ShapeTreeLocation();
        locationThree.setShapeTree("http://shapetrees.example/#thirdTree");
        locationThree.setManagedResource("http://data.example/resourceThree");
        locationThree.setRootShapeTreeLocation(URI.create("http://data.example/resourceThree.shapetree#locationThree"));
        locationThree.setFocusNode("http://data.example/resourceThree#focus");
        locationThree.setShape("http://shapes.example/#thirdShape");
        locationThree.setUri(URI.create("http://data.example/resourceThree.shapetree#locationThree"));

        locationFour = new ShapeTreeLocation();
        locationFour.setShapeTree("http://shapetrees.example/#fourthTree");
        locationFour.setManagedResource("http://data.example/resourceFour");
        locationFour.setRootShapeTreeLocation(URI.create("http://data.example/resourceFour.shapetree#locationFour"));
        locationFour.setFocusNode("http://data.example/resourceFour#focus");
        locationFour.setShape("http://shapes.example/#fourthShape");
        locationFour.setUri(URI.create("http://data.example/resourceFour.shapetree#locationFour"));

        locationFive = new ShapeTreeLocation();
        locationFive.setShapeTree("http://shapetrees.example/#fifthTree");
        locationFive.setManagedResource("http://data.example/resourceFive");
        locationFive.setRootShapeTreeLocation(URI.create("http://data.example/resourceFive.shapetree#locationFive"));
        locationFive.setFocusNode("http://data.example/resourceFive#focus");
        locationFive.setShape("http://shapes.example/#fifthShape");
        locationFive.setUri(URI.create("http://data.example/resourceFive.shapetree#locationFive"));

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

        ShapeTreeLocation locationThreeUpdated = duplicateLocation(locationThree);
        locationThreeUpdated.setShapeTree("http://shapetrees.pub/appleTree");

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

        ShapeTreeLocation locationThreeUpdated = duplicateLocation(locationThree);
        locationThreeUpdated.setShapeTree("http://shapetrees.pub/appleTree");

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

        ShapeTreeLocation locationThreeUpdated = duplicateLocation(locationThree);
        locationThreeUpdated.setShapeTree("http://shapetrees.pub/appleTree");

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

        ShapeTreeLocation locationOneUpdated = duplicateLocation(locationOne);
        locationOneUpdated.setFocusNode("http://data.example/resourceOne#Otherfocus");

        ShapeTreeLocation locationTwoUpdated = duplicateLocation(locationTwo);
        locationTwoUpdated.setFocusNode("http://data.example/resourceTwo#Otherfocus");

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

    private ShapeTreeLocation duplicateLocation(ShapeTreeLocation location) {

        ShapeTreeLocation duplicateLocation = new ShapeTreeLocation();
        duplicateLocation.setShapeTree(location.getShapeTree());
        duplicateLocation.setManagedResource(location.getManagedResource());
        duplicateLocation.setRootShapeTreeLocation(location.getRootShapeTreeLocation());
        duplicateLocation.setFocusNode(location.getFocusNode());
        duplicateLocation.setShape(location.getShape());
        duplicateLocation.setUri(location.getUri());
        return duplicateLocation;

    }

}
