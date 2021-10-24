package com.janeirodigital.shapetrees.core.models;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import lombok.Getter;

import java.util.ArrayList;
import java.util.List;

@Getter
public class ShapeTreeLocatorDelta {

    ShapeTreeLocator existingLocator;
    ShapeTreeLocator updatedLocator;
    List<ShapeTreeLocation> updatedLocations;
    List<ShapeTreeLocation> removedLocations;

    /**
     * Compares an updated ShapeTreeLocator (updatedLocator) with an existing one (existingLocator). Neither may
     * be null, locators with no locations are acceptable for the purposes of comparison.
     * @param existingLocator
     * @param updatedLocator
     * @return ShapeTreeLocatorDelta
     */
    public static ShapeTreeLocatorDelta evaluate(ShapeTreeLocator existingLocator, ShapeTreeLocator updatedLocator) throws ShapeTreeException {

        if (existingLocator == null && updatedLocator == null) {
            throw new ShapeTreeException(422, "Cannot compare two null locators");
        }

        ShapeTreeLocatorDelta delta = new ShapeTreeLocatorDelta();

        delta.existingLocator = existingLocator;
        delta.updatedLocator = updatedLocator;
        delta.updatedLocations = new ArrayList<>();
        delta.removedLocations = new ArrayList<>();


        if (updatedLocator == null || updatedLocator.getLocations() == null || updatedLocator.getLocations().isEmpty()) {
            // All locations have been removed in the updated locator, so any existing locations should
            // similarly be removed. No need for further comparison.
            delta.removedLocations = existingLocator.getLocations();
            return delta;
        }

        if (existingLocator == null || existingLocator.getLocations() == null || existingLocator.getLocations().isEmpty()) {
            // This existing locator doesn't have any locations (which means it shouldn't exist)
            // Anything in the updated locator is being added as new. No need for further comparison.
            delta.updatedLocations = updatedLocator.getLocations();
            return delta;
        }

        for (ShapeTreeLocation existingLocation : existingLocator.getLocations()) {

            // Locations match, and are unchanged, so continue
            if (updatedLocator.getLocations().contains(existingLocation)) { continue; }

            // Locations have the same URL but are different, so update
            ShapeTreeLocation updatedLocation = containsSameUri(existingLocation, updatedLocator.getLocations());

            if (updatedLocation != null) {
                delta.updatedLocations.add(updatedLocation);
                continue;
            }

            // existing location isn't in the updated location, so remove
            delta.removedLocations.add(existingLocation);

        }

        for (ShapeTreeLocation updatedLocation : updatedLocator.getLocations()) {

            // Locations match, and are unchanged, so continue
            if (existingLocator.getLocations().contains(updatedLocation)) { continue; }

            // If this was already processed and marked as updated continue
            if (delta.updatedLocations.contains(updatedLocation)) { continue; }

            // updated location isn't in the existing locations, so it is new, add it
            delta.updatedLocations.add(updatedLocation);
        }

        return delta;

    }

    public static ShapeTreeLocation containsSameUri(ShapeTreeLocation location, List<ShapeTreeLocation> targetLocations) {
        for (ShapeTreeLocation targetLocation : targetLocations) {
            if (location.getUrl().equals(targetLocation.getUrl())) { return targetLocation; }
        }
        return null;
    }

    public boolean allRemoved() {
        return (!this.isUpdated() && this.removedLocations.size() == this.existingLocator.getLocations().size());
    }

    public boolean isUpdated() {
        return !this.updatedLocations.isEmpty();
    }

    public boolean wasReduced() {
        return !this.removedLocations.isEmpty();
    }

}
