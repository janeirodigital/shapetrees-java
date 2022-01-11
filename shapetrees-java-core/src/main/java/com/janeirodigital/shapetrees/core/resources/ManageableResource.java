package com.janeirodigital.shapetrees.core.resources;

import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;
import lombok.Getter;

import java.net.URL;
import java.util.Optional;

import static com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType.CONTAINER;

/**
 * A ManageableResource represents a regular resource that could be managed by
 * one or more shape trees. Each possible state is represented by typed
 * subclasses; ManagedResource, UnmanagedResource, and
 * MissingManageableResource. When the state is known, the appropriate
 * typed subclass should be used.
 */
@Getter
public class ManageableResource extends InstanceResource {

    private final Optional<URL> managerResourceUrl;

    /**
     * Construct a manageable resource.
     * @param url URL of the resource
     * @param resourceType Identified shape tree resource type
     * @param attributes Associated resource attributes
     * @param body Body of the resource
     * @param name Name of the resource
     * @param exists Whether the resource exists
     * @param managerResourceUrl URL of the shape tree manager resource
     */
    public ManageableResource(URL url, ShapeTreeResourceType resourceType, ResourceAttributes attributes, String body, String name, boolean exists, Optional<URL> managerResourceUrl) {
        super(url, resourceType, attributes, body, name, exists);
        this.managerResourceUrl = managerResourceUrl;
    }

    public boolean isContainer() {
        return this.getResourceType() == CONTAINER;
    }

}
