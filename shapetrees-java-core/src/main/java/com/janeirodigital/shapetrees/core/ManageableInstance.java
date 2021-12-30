package com.janeirodigital.shapetrees.core;

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import java.net.URL;
import java.util.Objects;

/**
 * A ManageableInstance represents a pairing of a shape tree ManagerResource
 * and a ManageableResource.
 *
 * The ManageableInstance may represent a managed
 * state, where the ManageableResource is a ManagedResource that is
 * managed by one or more shape trees assigned by the ShapeTreeManager
 * in the ManagedResource.Conversely, it could represent an unmanaged
 * state, where the ManageableResource is an UnmanagedResource and the
 * ManagedResource is a MissingManagedResource. Lastly, it may
 * represent other state combinations where one or both of the
 * ManageableResource or ManagedResource are missing.
 *
 * Both ManageableResource and ManagedResource are looked up and loaded
 * upon construction of the ManageableInstance, which should be done
 * through a ResourceAccessor. Once constructed, the ManageableInstance
 * is immutable.
 */
@Slf4j @Getter
public class  ManageableInstance {
    public static final String TEXT_TURTLE = "text/turtle";

    private final ResourceAccessor resourceAccessor;
    private final ShapeTreeContext shapeTreeContext;
    private final boolean wasRequestForManager;
    private final ManageableResource manageableResource;
    private final ManagerResource managerResource;

    /**
     * Indicates whether the HTTP request that triggered the initialization of the
     * ManageableInstance was targeted towards the ManagerResource or the
     * ManageableResource.
     * @return True when the request targeted the ManagerResource
     */
    public boolean wasRequestForManager() { return isWasRequestForManager(); }

    /**
     * Indicates whether the ManageableInstance represents an unmanaged state, with a
     * UnmanagedResource and a MissingManagerResource
     * @return True when the instance is in an unmanaged state
     */
    public boolean isUnmanaged() { return managerResource instanceof MissingManagerResource; }

    /**
     * Indicates whether the ManageableInstance represents a managed state, with a
     * ManagedResource assigned one or more shape trees by a ShapeTreeManager in
     * a ManagerResource
     * @return True when the instance is in an managed state
     */
    public boolean isManaged() { return !isUnmanaged(); }

    /**
     * Constructor for a ManageableInstance. Since a ManageableInstance is immutable, all
     * elements must be provided, and cannot be null. ManageableInstances should be
     * constructed through a ResourceAccessor:
     * {@link ResourceAccessor#createInstance(ShapeTreeContext, String, URL, ResourceAttributes, String, String)}
     * {@link ResourceAccessor#getInstance(ShapeTreeContext, URL)}
     * @param context Shape tree context
     * @param resourceAccessor Resource accessor in use
     * @param wasRequestForManager True if the manager resource was the target of the associated request
     * @param manageableResource Initialized manageable resource, which may be a typed sub-class
     * @param managerResource Initialized manager resource, which may be a typed sub-class
     */
    public ManageableInstance(ShapeTreeContext context,
                              ResourceAccessor resourceAccessor,
                              boolean wasRequestForManager,
                              ManageableResource manageableResource,
                              ManagerResource managerResource) {

        this.shapeTreeContext = Objects.requireNonNull(context, "Must provide a shape tree context");
        this.resourceAccessor = Objects.requireNonNull(resourceAccessor, "Must provide a resource accessor");
        this.wasRequestForManager = wasRequestForManager;
        this.manageableResource = Objects.requireNonNull(manageableResource, "Must provide a manageable resource");
        this.managerResource = Objects.requireNonNull(managerResource, "Must provide a manager resource");

    }

}
