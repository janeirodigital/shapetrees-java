package com.janeirodigital.shapetrees.core;

import java.net.URL;
import java.util.Optional;

/**
 * An UnmanagedResource indicates that a given ManageableResource
 * is not managed by a shape tree. This means that there is not an
 * associated ManagerResource that exists.
 */
public class UnmanagedResource extends ManageableResource {

    /**
     * Construct an UnmanagedResource based on a provided ManageableResource
     * <code>manageable</code> and <code>managerUrl</code>
     * @param manageable ManageableResource to construct the UnmanagedResource from
     * @param managerUrl URL of the associated shape tree manager resource
     */
    public UnmanagedResource(ManageableResource manageable, Optional<URL> managerUrl) {
        super(manageable.getUrl(), manageable.getResourceType(), manageable.getAttributes(), manageable.getBody(), manageable.getName(), manageable.isExists(), managerUrl);
    }

}
