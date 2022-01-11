package com.janeirodigital.shapetrees.core.resources;

import com.janeirodigital.shapetrees.core.validation.ShapeTreeContext;
import com.janeirodigital.shapetrees.core.validation.ShapeTreeManager;
import com.janeirodigital.shapetrees.core.enums.LinkRelation;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.vocabularies.LdpVocabulary;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.apache.jena.graph.Graph;
import org.apache.jena.graph.Node;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Triple;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.*;

import static com.janeirodigital.shapetrees.core.enums.ContentType.OCTET_STREAM;
import static com.janeirodigital.shapetrees.core.enums.HttpHeader.CONTENT_TYPE;
import static com.janeirodigital.shapetrees.core.helpers.DocumentResponseHelper.getHeader;
import static com.janeirodigital.shapetrees.core.helpers.GraphHelper.readStringIntoGraph;
import static com.janeirodigital.shapetrees.core.helpers.GraphHelper.urlToUri;

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
public class ManageableInstance {
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
     * {@link #createInstanceResource(ResourceAccessor, ShapeTreeContext, URL, ResourceAttributes, String, String)}
     * {@link #getInstance(ResourceAccessor, ShapeTreeContext, URL)}
     * @param context Shape tree context
     * @param resourceAccessor Resource accessor in use
     * @param wasRequestForManager True if the manager resource was the target of the associated request
     * @param manageableResource Initialized manageable resource, which may be a typed sub-class
     * @param managerResource Initialized manager resource, which may be a typed sub-class
     */
    private ManageableInstance(ShapeTreeContext context,
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

    /**
     * Gets a list of {@link ManageableInstance}s contained in a managed resource that is a container.
     * @return List of contained {@link ManageableInstance}s
     * @throws ShapeTreeException
     */
    public List<ManageableInstance>
    getContainedInstances() throws ShapeTreeException {
        ManageableResource resource = this.getManageableResource();
        ResourceAttributes attributes = resource.getAttributes();
        if (!resource.isContainer()) {
            throw new ShapeTreeException(500, "Cannot get contained resources for a non-container <" + resource.getUrl() + ">");
        }
        try {
            Graph containerGraph = readStringIntoGraph(urlToUri(resource.getUrl()), resource.getBody(), getHeader(attributes, CONTENT_TYPE).orElse(null));

            if (containerGraph.isEmpty()) { return Collections.emptyList(); }

            List<Triple> containerTriples = containerGraph.find(NodeFactory.createURI(resource.getUrl().toString()),
                    NodeFactory.createURI(LdpVocabulary.CONTAINS),
                    Node.ANY).toList();

            if (containerTriples.isEmpty()) { return Collections.emptyList(); }

            ArrayList<ManageableInstance> containedInstances = new ArrayList<>();

            for (Triple containerTriple : containerTriples) {
                ManageableInstance containedInstance = getInstance(this.resourceAccessor, this.shapeTreeContext, new URL(containerTriple.getObject().getURI()));
                containedInstances.add(containedInstance);
            }

            return containedInstances;
        } catch (Exception ex) {
            throw new ShapeTreeException(500, ex.getMessage());
        }
    }

    /**
     * Return a new version of provided {@link ManageableInstance} <code>originalInstance</code>
     * @param originalInstance Original instance to get a new version of
     * @return Reloaded version of <code>originalInstance</code>
     * @throws ShapeTreeException
     */
    public static ManageableInstance
    reloadInstance(ManageableInstance originalInstance) throws ShapeTreeException {
        return getInstance(originalInstance.getResourceAccessor(), originalInstance.getShapeTreeContext(), originalInstance.getManageableResource().getUrl());
    }

    /**
     * Return a {@link ManageableInstance} constructed starting with the resource identified by the provided
     * <code>resourceUrl</code>. The <code>resourceUrl</code> may target either a {@link ManageableResource},
     * or a {@link ManagerResource}.
     *
     * <p>Both the {@link ManageableResource} and {@link ManagerResource} are retrieved and loaded as specifically
     * typed sub-classes that indicate whether they exist, or (in the case of {@link ManageableResource})
     * whether they are managed.</p>
     * @param accessor {@link ResourceAccessor} to use to get {@link InstanceResource}s
     * @param context {@link ShapeTreeContext}
     * @param resourceUrl URL of the resource to get
     * @return {@link ManageableInstance} including {@link ManageableResource} and {@link ManagerResource}
     * @throws ShapeTreeException
     */
    public static ManageableInstance
    getInstance(ResourceAccessor accessor, ShapeTreeContext context, URL resourceUrl) throws ShapeTreeException {

        Objects.requireNonNull(accessor, "Must provide a non-null resource accessor");
        Objects.requireNonNull(accessor, "Must provide a non-null shape tree context");
        Objects.requireNonNull(accessor, "Must provide a non-null resource URL");

        log.debug("Getting manageable instance for {}", resourceUrl);

        InstanceResource resource = accessor.getResource(context, resourceUrl);

        if (resource instanceof MissingManageableResource) {
            // Get is for a manageable resource that doesn't exist
            return getInstanceFromMissingManageableResource(accessor, context, (MissingManageableResource)resource);
        } else if (resource instanceof MissingManagerResource) {
            // Get is for a manager resource that doesn't exist
            return getInstanceFromMissingManagerResource(accessor, context, (MissingManagerResource)resource);
        } else if (resource instanceof ManageableResource) {
            // Get is for an existing manageable resource
            return getInstanceFromManageableResource(accessor, context, (ManageableResource)resource);
        } else if (resource instanceof ManagerResource) {
            // Get is for an existing manager resource
            return getInstanceFromManagerResource(accessor, context, (ManagerResource)resource);
        }

        throw new ShapeTreeException(500, "Can get instance from resource of unsupported type: " + resource.getUrl());

    }

    /**
     * Gets a {@link ManageableInstance} given a {@link MissingManageableResource}, which means that
     * a corresponding {@link ManagerResource} cannot exist, so a {@link MissingManagerResource} is
     * constructed and included as part of instance construction.
     * @param context {@link ShapeTreeContext}
     * @param missing {@link MissingManageableResource}
     * @return {@link ManageableInstance} including {@link MissingManageableResource} and {@link MissingManagerResource}
     */
    private static ManageableInstance
    getInstanceFromMissingManageableResource(ResourceAccessor accessor, ShapeTreeContext context, MissingManageableResource missing) {

        MissingManagerResource missingManager = new MissingManagerResource(missing, null);
        log.debug("Got manageable instance for missing resource {}", missing.getUrl());
        return new ManageableInstance(context, accessor, false, missing, missingManager);

    }

    /**
     * Gets a {@link ManageableInstance} given a {@link MissingManagerResource}, which means that
     * a {@link ManagerResource} doesn't exist, but an {@link UnmanagedResource} that would be associated
     * with it may, so it is looked up over HTTP and populated with the appropriate resulting type
     * based on its existence.
     * @param context {@link ShapeTreeContext}
     * @param missing {@link MissingManagerResource}
     * @return {@link ManageableInstance} including {@link UnmanagedResource}|{@link MissingManageableResource} and {@link MissingManagerResource}
     * @throws ShapeTreeException
     */
    private static ManageableInstance
    getInstanceFromMissingManagerResource(ResourceAccessor accessor, ShapeTreeContext context, MissingManagerResource missing) throws ShapeTreeException {

        InstanceResource manageable = accessor.getResource(context, calculateManagedUrl(missing.getUrl(), missing.getAttributes()));

        if (manageable.isExists()) {
            UnmanagedResource unmanaged = new UnmanagedResource((ManageableResource)manageable, Optional.of(missing.getUrl()));
            log.debug("Got manageable instance for unmanaged resource {}, from request to missing manager resource {}", unmanaged.getUrl(), missing.getUrl());
            return new ManageableInstance(context, accessor, true, unmanaged, missing);
        } else {
            throw new ShapeTreeException(500, "Cannot get a shape tree manager " + missing.getUrl() + " for a missing manageable resource " + manageable.getUrl());
        }
    }

    /**
     * Gets a {@link ManageableInstance} given a {@link ManageableResource}, which could be a
     * {@link ManagedResource} or an {@link UnmanagedResource}. Which type is determined by
     * the presence of the {@link ManagerResource}, which is looked up and the instance is
     * populated with the appropriate resulting types.*
     * @param context {@link ShapeTreeContext}
     * @param manageable {@link ManagedResource} or {@link UnmanagedResource}
     * @return {@link ManageableInstance} including {@link UnmanagedResource}|{@link ManagedResource} and {@link ManagerResource}|{@link MissingManagerResource}
     * @throws ShapeTreeException
     */
    private static ManageableInstance
    getInstanceFromManageableResource(ResourceAccessor accessor, ShapeTreeContext context, ManageableResource manageable) throws ShapeTreeException {

        URL managerResourceUrl = manageable.getManagerResourceUrl().orElseThrow(() -> new ShapeTreeException(500, "Cannot discover shape tree manager for " + manageable.getUrl()));

        InstanceResource manager = accessor.getResource(context, managerResourceUrl);

        if (manager instanceof MissingManagerResource) {
            // If the manager does exist it is unmanaged - Get and store both in instance
            UnmanagedResource unmanaged = new UnmanagedResource(manageable, Optional.of(manager.getUrl()));
            log.debug("Got manageable instance for unmanaged resource {}", unmanaged.getUrl());
            return new ManageableInstance(context, accessor, false, unmanaged, (ManagerResource) manager);
        } else if (manager instanceof ManagerResource) {
            // If the manager exists then it is managed - get and store manager and managed resource in instance
            ManagedResource managed = new ManagedResource(manageable, Optional.of(manager.getUrl()));
            log.debug("Got manageable instance for managed resource {}", managed.getUrl());
            return new ManageableInstance(context, accessor, false, managed, (ManagerResource)manager);
        } else {
            throw new ShapeTreeException(500, "Error looking up corresponding shape tree manager for " + manageable.getUrl());
        }

    }

    /**
     * Gets a {@link ManageableInstance} given a {@link ManagerResource}. The corresponding
     * {@link ManagedResource} is looked up and the instance is populated with it.
     * @param context {@link ShapeTreeContext}
     * @param manager Existing {@link ManagerResource}
     * @return {@link ManageableInstance} including {@link ManagerResource} and {@link ManagedResource}
     * @throws ShapeTreeException
     */
    private static ManageableInstance
    getInstanceFromManagerResource(ResourceAccessor accessor, ShapeTreeContext context, ManagerResource manager) throws ShapeTreeException {
        InstanceResource manageable = accessor.getResource(context, manager.getManagedResourceUrl());
        if (manageable instanceof MissingManageableResource) {
            throw new ShapeTreeException(500, "Cannot have a shape tree manager at " + manager.getUrl() + " without a corresponding managed resource");
        }
        ManagedResource managed = new ManagedResource((ManageableResource)manageable, Optional.of(manager.getUrl()));
        log.debug("Got manageable instance for managed resource {}, based on request to manager {}", managed.getUrl(), manager.getUrl());
        return new ManageableInstance(context, accessor, true, managed, manager);
    }

    /**
     * Looks for the presence of the http://www.w3.org/ns/shapetrees#manages HTTP Link Relation in the
     * provided <code>attributes</code>, with a valid target URL of a {@link ManagedResource}. Falls
     * back to a relatively crude inference when the more reliable header isn't available
     * @param managerUrl URL of the {@link ShapeTreeManager}
     * @param attributes Parsed link headers from {@link ManagerResource} response
     * @return URL of {@link ManagedResource}
     * @throws ShapeTreeException
     */
    public static URL
    calculateManagedUrl(URL managerUrl, ResourceAttributes attributes) throws ShapeTreeException {

        String managedUrlString;
        URL managedResourceUrl;

        final Optional<String> optManagedString = attributes.firstValue(LinkRelation.MANAGES.getValue());
        if (!optManagedString.isEmpty()) {
            managedUrlString = optManagedString.get();
        } else {
            // Attempt to (crudely) infer based on path calculation
            // If this implementation uses a dot notation for meta, trim it from the path
            // Rebuild without the query string in case that was employed
            managedUrlString = managerUrl.getPath().replaceAll("\\.shapetree$", "");
        }

        try {
            managedResourceUrl = new URL(managerUrl, managedUrlString);
        } catch (MalformedURLException e) {
            throw new ShapeTreeException(500, "Can't calculate managed resource for shape tree manager <" + managerUrl + ">");
        }

        return managedResourceUrl;
    }

    /**
     * Creates the resource identified by the provided <code>resourceUrl</code> and gets a corresponding
     * {@link ManageableInstance}. This may create either a {@link ManageableResource} or a {@link ManagerResource}.
     * The newly created resource is loaded into the instance, and the corresponding {@link ManageableResource} or
     * {@link ManagerResource} is looked up and loaded into the instance alongside it. They are loaded as specifically
     * typed sub-classes that indicate whether they exist, or (in the case of {@link ManageableResource}),
     * whether they are managed.
     * @param context {@link ShapeTreeContext}
     * @param resourceUrl URL of the resource to create
     * @param headers Incoming HTTP headers
     * @param body Body of the resource to create
     * @param contentType Content-type of the resource to create
     * @return {@link ManageableInstance} including {@link ManageableResource} and {@link ManagerResource}
     * @throws ShapeTreeException
     */
    public static ManageableInstance
    createInstanceResource(ResourceAccessor accessor, ShapeTreeContext context, URL resourceUrl, ResourceAttributes headers, String body, String contentType) throws ShapeTreeException {

        Objects.requireNonNull(accessor, "Must provide a non-null resource accessor");
        Objects.requireNonNull(accessor, "Must provide a non-null shape tree context");
        Objects.requireNonNull(accessor, "Must provide a non-null resource URL");

        if (headers == null) { headers = new ResourceAttributes(); }
        if (body == null) { body = ""; }
        if (contentType == null) { contentType = OCTET_STREAM.getValue(); }

        log.debug("Creating manageable instance for {}", resourceUrl);

        InstanceResource resource = accessor.createResource(context, resourceUrl, headers, body, contentType);

        if (resource instanceof ManageableResource) {
            // Managed or unmanaged resource was created
            return createInstanceFromManageableResource(accessor, context, (ManageableResource)resource);
        } else if (resource instanceof ManagerResource) {
            // Manager resource was created
            return createInstanceFromManagerResource(accessor, context, (ManagerResource)resource);
        }

        throw new ShapeTreeException(500, "Invalid resource type returned from resource creation");

    }

    /**
     * Gets a {@link ManageableInstance} given a newly created {@link ManageableResource}. A corresponding
     * {@link ManagerResource} is looked up. If it exists, a {@link ManagedResource} is initialized and loaded
     * into the instance. If it doesn't, an {@link UnmanagedResource} is initialized and loaded instead.
     * @param context {@link ShapeTreeContext}
     * @param manageable Newly created {@link ManageableResource}
     * @return {@link ManageableInstance} including {@link ManagedResource}|{@link UnmanagedResource} and {@link ManagerResource}|{@link MissingManagerResource}
     * @throws ShapeTreeException
     */
    private static ManageableInstance
    createInstanceFromManageableResource(ResourceAccessor accessor, ShapeTreeContext context, ManageableResource manageable) throws ShapeTreeException {

        // Lookup the corresponding ManagerResource for the ManageableResource
        URL managerResourceUrl = manageable.getManagerResourceUrl().orElseThrow(() -> new ShapeTreeException(500, "Cannot discover shape tree manager for " + manageable.getUrl()));
        InstanceResource manager = accessor.getResource(context, managerResourceUrl);

        if (manager instanceof MissingManagerResource) {
            // Create and store an UnmanagedResource in instance - if the create was a resource in an unmanaged container
            UnmanagedResource unmanaged = new UnmanagedResource(manageable, Optional.of(manager.getUrl()));
            log.debug("Got manageable instance for newly created unmanaged resource {}", unmanaged.getUrl());
            return new ManageableInstance(context, accessor, false, unmanaged, (ManagerResource)manager);
        } else if (manager instanceof ManagerResource) {
            // Create and store a ManagedResource in instance - if the create was a resource in a managed container
            ManagedResource managed = new ManagedResource(manageable, Optional.of(manager.getUrl()));
            log.debug("Got manageable instance for newly created managed resource {}", managed.getUrl());
            return new ManageableInstance(context, accessor, false, managed, (ManagerResource)manager);
        }

        throw new ShapeTreeException(500, "Error looking up corresponding shape tree manager for " + manageable.getUrl());

    }

    /**
     * Gets a {@link ManageableInstance} given a newly created {@link ManagerResource}. A corresponding
     * {@link ManagedResource} is looked up (and which must exist and be associated with this
     * manager).
     * @param context {@link ShapeTreeContext}
     * @param manager Newly created {@link ManagerResource}
     * @return {@link ManageableInstance} including {@link ManagerResource} and {@link ManagedResource}
     * @throws ShapeTreeException
     */
    private static ManageableInstance
    createInstanceFromManagerResource(ResourceAccessor accessor, ShapeTreeContext context, ManagerResource manager) throws ShapeTreeException {

        // Lookup the corresponding ManagedResource for the ManagerResource
        InstanceResource resource = accessor.getResource(context, manager.getManagedResourceUrl());

        if (resource instanceof MissingManageableResource) {
            throw new ShapeTreeException(500, "Cannot have an existing manager resource " + manager.getUrl() + " with a non-existing managed resource " + resource.getUrl());
        } else if (resource instanceof ManagerResource) {
            throw new ShapeTreeException(500, "Invalid manager resource " + resource.getUrl() + " seems to be associated with another manager resource " + manager.getUrl());
        }

        ManagedResource managed = new ManagedResource((ManageableResource)resource, Optional.of(manager.getUrl()));
        log.debug("Got manageable instance for managed resource {}, from newly created manager resource {}", managed.getUrl(), manager.getUrl());
        return new ManageableInstance(context, accessor, true, managed, manager);

    }
}
