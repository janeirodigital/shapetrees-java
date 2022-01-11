package com.janeirodigital.shapetrees.tests.fixtures;

import java.util.List;
import java.util.Map;

public class DispatcherHelper {

    public static boolean addFixture(RequestMatchingFixtureDispatcher dispatcher, List<String> fixtures, String method, String path, Map<String, List<String>> headers) {
        return dispatcher.getConfiguredFixtures().add(new DispatcherEntry(fixtures, method, path, headers));
    }

    public static boolean mockOnGet(RequestMatchingFixtureDispatcher dispatcher, String path, List<String> fixtures) {
        return dispatcher.getConfiguredFixtures().add(new DispatcherEntry(fixtures, "GET", path, null));
    }

    public static boolean mockOnGet(RequestMatchingFixtureDispatcher dispatcher, String path, String fixture) {
        return dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of(fixture), "GET", path, null));
    }

    public static boolean mockOnPut(RequestMatchingFixtureDispatcher dispatcher, String path, List<String> fixtures) {
        return dispatcher.getConfiguredFixtures().add(new DispatcherEntry(fixtures, "PUT", path, null));
    }

    public static boolean mockOnPut(RequestMatchingFixtureDispatcher dispatcher, String path, String fixture) {
        return dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of(fixture), "PUT", path, null));
    }

    public static boolean mockOnPatch(RequestMatchingFixtureDispatcher dispatcher, String path, List<String> fixtures) {
        return dispatcher.getConfiguredFixtures().add(new DispatcherEntry(fixtures, "PATCH", path, null));
    }

    public static boolean mockOnPatch(RequestMatchingFixtureDispatcher dispatcher, String path, String fixture) {
        return dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of(fixture), "PATCH", path, null));
    }

    public static boolean mockOnDelete(RequestMatchingFixtureDispatcher dispatcher, String path, List<String> fixtures) {
        return dispatcher.getConfiguredFixtures().add(new DispatcherEntry(fixtures, "DELETE", path, null));
    }

    public static boolean mockOnDelete(RequestMatchingFixtureDispatcher dispatcher, String path, String fixture) {
        return dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of(fixture), "DELETE", path, null));
    }

    // Note: make headers optional with override
    // addFileFixture (current behavior)
    // addFixture (non-file, body of resource is optional but must be provided as string if desired)
    // addManagedResource (should automatically include content-type and link header for managed-by
    // addManagerResource (should automatically include content-type and link header for manages)
    // Real question - should any more than this be automated? specifically, creating the manager with assignments
    // addManagerResource -> could take ShapeTreeManager as parameter
    // add assignments to the shapetree manager - assign to

}
