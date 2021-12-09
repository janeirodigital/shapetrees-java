package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import lombok.Getter;
import org.apache.jena.graph.Graph;

import java.net.URL;

/**
 * A ManagerResource represents a resource that is associated with
 * a regular MangeableResource, and contains metadata in the form
 * of a ShapeTreeManager that assigns one or more shape trees to the
 * associated ManageableResource. When it exists, the associated
 * resource is considered to be managed. When it doesn't, the associated
 * resource is considered to be unmanaged.
 */
@Getter
public class ManagerResource extends InstanceResource {

    private final URL managedResourceUrl;

    /**
     * Construct a manager resource
     * @param url URL of the resource
     * @param resourceType Identified shape tree resource type
     * @param attributes Associated resource attributes
     * @param body Body of the resource
     * @param name Name of the resource
     * @param exists Whether the resource exists
     * @param managedResourceUrl URL of the associated managed resource
     */
    public ManagerResource(URL url, ShapeTreeResourceType resourceType, ResourceAttributes attributes, String body, String name, boolean exists, URL managedResourceUrl) {
        super(url, resourceType, attributes, body, name, exists);
        this.managedResourceUrl = managedResourceUrl;
    }

    /**
     * Get a ShapeTreeManager from the body of the ManagerResource
     * @return Shape tree manager
     * @throws ShapeTreeException
     */
    public ShapeTreeManager getManager() throws ShapeTreeException {
        if (!this.isExists()) { return null; }
        Graph managerGraph = this.getGraph(this.getUrl());
        if (managerGraph == null) {
            return null;
        }
        return ShapeTreeManager.getFromGraph(this.getUrl(), managerGraph);
    }

}
