package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;

import java.net.URL;

/**
 * A MissingManagerResource represents a state where a given
 * ManagerResource at a URL does not exist.
 */
public class MissingManagerResource extends ManagerResource {

    /**
     * Construct a missing manager resource based on a MissingManageableResource
     * @param managedResourceUrl Corresponding URL of the resource that would be managed
     * @param manageable Missing manageable resource
     */
    public MissingManagerResource(URL managedResourceUrl, MissingManageableResource manageable) {
        super(manageable.getUrl(), manageable.getResourceType(), manageable.getAttributes(), manageable.getBody(), manageable.getName(), manageable.isExists(), managedResourceUrl);
    }

    /**
     * Construct a missing manager resource.
     * @param managedResourceUrl URL of the resource that would be managed
     * @param url URL of the resource
     * @param resourceType Identified shape tree resource type
     * @param attributes Associated resource attributes
     * @param body Body of the resource
     * @param name Name of the resource
     */
    public MissingManagerResource(URL managedResourceUrl, URL url, ShapeTreeResourceType resourceType, ResourceAttributes attributes, String body, String name) {
        super(url, resourceType, attributes, body, name, false, managedResourceUrl);
    }

}
