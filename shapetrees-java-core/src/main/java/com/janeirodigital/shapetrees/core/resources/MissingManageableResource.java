package com.janeirodigital.shapetrees.core.resources;

import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;

import java.net.URL;
import java.util.Optional;

/**
 * A MissingManageableResource represents a state where a given
 * ManageableResource at a URL does not exist.
 */
public class MissingManageableResource extends ManageableResource {

    /**
     * Construct a missing manageable resource.
     * @param url URL of the resource
     * @param resourceType Identified shape tree resource type
     * @param attributes Associated resource attributes
     * @param body Body of the resource
     * @param name Name of the resource
     * @param managerResourceUrl URL of the shape tree manager resource
     */
    public MissingManageableResource(URL url, ShapeTreeResourceType resourceType, ResourceAttributes attributes, String body, String name, Optional<URL> managerResourceUrl) {
        super(url, resourceType, attributes, body, name, false, managerResourceUrl);
    }
}
