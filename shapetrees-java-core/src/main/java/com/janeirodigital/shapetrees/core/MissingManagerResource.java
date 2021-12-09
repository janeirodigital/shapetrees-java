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
     * @param manageable Missing manageable resource
     * @param managedUrl Corresponding URL of the resource that would be managed
     */
    public MissingManagerResource(MissingManageableResource manageable, URL managedUrl) {
        super(manageable.getUrl(), manageable.getResourceType(), manageable.getAttributes(), manageable.getBody(), manageable.getName(), manageable.isExists(), managedUrl);
    }

    /**
     * Construct a missing manager resource.
     * @param url URL of the resource
     * @param resourceType Identified shape tree resource type
     * @param attributes Associated resource attributes
     * @param body Body of the resource
     * @param name Name of the resource
     * @param managedResourceUrl URL of the resource that would be managed
     */
    public MissingManagerResource(URL url, ShapeTreeResourceType resourceType, ResourceAttributes attributes, String body, String name, URL managedResourceUrl) {
        super(url, resourceType, attributes, body, name, false, managedResourceUrl);
    }

}
