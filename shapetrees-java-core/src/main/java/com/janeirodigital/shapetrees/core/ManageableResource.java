package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import lombok.Getter;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Optional;

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
    private final boolean isContainer;

    /**
     * Construct a manageable resource.
     * @param url URL of the resource
     * @param resourceType Identified shape tree resource type
     * @param attributes Associated resource attributes
     * @param body Body of the resource
     * @param name Name of the resource
     * @param exists Whether the resource exists
     * @param managerResourceUrl URL of the shape tree manager resource
     * @param isContainer Whether the resource is a container
     */
    public ManageableResource(URL url, ShapeTreeResourceType resourceType, ResourceAttributes attributes, String body, String name, boolean exists, Optional<URL> managerResourceUrl, boolean isContainer) {
        super(url, resourceType, attributes, body, name, exists);
        this.managerResourceUrl = managerResourceUrl;
        this.isContainer = isContainer;
    }

    /**
     * Get the URL of the resource's parent container
     * @return URL of the parent container
     * @throws ShapeTreeException
     */
    public URL getParentContainerUrl() throws ShapeTreeException {
        final String rel = this.isContainer() ? ".." : ".";
        try {
            return new URL(this.getUrl(), rel);
        } catch (MalformedURLException e) {
            throw new ShapeTreeException(500, "Malformed focus node when resolving <" + rel + "> against <" + this.getUrl() + ">");
        }
    }

}
