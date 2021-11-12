package com.janeirodigital.shapetrees.core;

import java.net.URL;
import java.util.Optional;

/**
 * A ManagedResource indicates that a given ManageableResource
 * is managed by a shape tree. This means that is has an associated
 * ManagerResource that exists and contains a valid ShapeTreeManager.
 */
public class ManagedResource extends ManageableResource {

    /**
     * Construct a ManagedResource based on a provided ManageableResource
     * <code>manageable</code> and <code>managerUrl</code>
     * @param manageable ManageableResource to construct the ManagedResource from
     * @param managerUrl URL of the associated shape tree manager resource
     */
    public ManagedResource(ManageableResource manageable, Optional<URL> managerUrl) {
        super(manageable.getUrl(), manageable.getResourceType(), manageable.getAttributes(), manageable.getBody(), manageable.getName(), manageable.isExists(), managerUrl, manageable.isContainer());
    }

}
