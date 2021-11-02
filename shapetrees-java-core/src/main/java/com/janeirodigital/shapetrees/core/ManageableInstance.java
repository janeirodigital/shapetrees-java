package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.core.enums.LinkRelations;
import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.helpers.GraphHelper;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;
import com.janeirodigital.shapetrees.core.models.ShapeTreeManager;
import lombok.extern.slf4j.Slf4j;
import org.apache.jena.graph.Graph;

import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.function.Supplier;

import static com.janeirodigital.shapetrees.core.helpers.GraphHelper.urlToUri;

@Slf4j
public class ManageableInstance {
    public static final String TEXT_TURTLE = "text/turtle";

    // access parameters
    final protected ResourceAccessor _resourceAccessor;
    final protected ShapeTreeContext _shapeTreeContext;
    final protected boolean _wasCreateFromManager;  // TODO - Why?

    // components
    // TODO - Is there always one or the other? Both?
    private Optional<ManageableResource> manageableResource = Optional.empty();
    private Optional<ManagerResource> managerResource = Optional.empty();

    // simple getters
    public boolean wasCreatedFromManager() { return this._wasCreateFromManager; }    // TODO - why is this important?
    public ShapeTreeContext getShapeTreeContext() { return this._shapeTreeContext; }

    // constructors
    private ManageableInstance(URL url, ResourceAccessor resourceAccessor, ShapeTreeContext shapeTreeContext, Resource str) {
        this._resourceAccessor = resourceAccessor;
        this._shapeTreeContext = shapeTreeContext;
        if (str instanceof ManagerResource) {
            // If the resource is being created from a manager resource, keep track of that, and assign it
            this._wasCreateFromManager = true;
            this.managerResource = Optional.of((ManagerResource) str);
        } else {
            // If the resource is being created from a manageable resource, let them know it wasn't from a manager, and assign it
            this._wasCreateFromManager = false;
            this.manageableResource = Optional.of((ManageableResource) str);
        }
    }
    public ManageableInstance(URL url, ResourceAccessor resourceAccessor, ShapeTreeContext shapeTreeContext) throws ShapeTreeException {
        this(url, resourceAccessor, shapeTreeContext, resourceAccessor.getResource(shapeTreeContext, url));
    }
    public ManageableInstance(URL url, ResourceAccessor resourceAccessor, ShapeTreeContext shapeTreeContext, ShapeTreeRequest shapeTreeRequest) throws ShapeTreeException {
        this(url, resourceAccessor, shapeTreeContext, resourceAccessor.createResource(shapeTreeContext, shapeTreeRequest.getMethod(), url, shapeTreeRequest.getHeaders(), shapeTreeRequest.getBody(), shapeTreeRequest.getContentType()));
    }

    // Get specific types of individual resource
    public ManageableResource getManageableResource() throws ShapeTreeException {
        ManageableResource manageableResource;
        if (this.manageableResource.isEmpty()) {
            ManagerResource managerResource = this.managerResource.orElseThrow(unintialized_instanceResource);
            /* TODO: #86 @see https://github.com/xformativ/shapetrees-java/issues/86            */
//            if (... no managedResourceUrl ...) {
//                throw new ShapeTreeException(500, "No link headers in manager resource <" + managerResource.url + ">");
//            }
            URL url = managerResource.getManagedResourceUrl();
            Resource instanceResource = this._resourceAccessor.getResource(this._shapeTreeContext, url);
            if (instanceResource instanceof ManageableResource) {
                this.manageableResource = Optional.of(manageableResource = (ManageableResource) instanceResource);
            } else {
                throw new IllegalStateException("Dereferencing <" + url + "> did not yield a ManageableResource");
            }
        } else {
            manageableResource = this.manageableResource.get();
        }
        return manageableResource;
    }

    public ManagerResource getManagerResource() throws ShapeTreeException {
        ManagerResource managerResource;
        if (this.managerResource.isEmpty()) {
//            ManageableResource managedResource = this.managedResource.orElseThrow(unintialized_instanceResource);
//            if (... no shapeTreeManagerUrlForResource ...) {
//                throw new ShapeTreeException(500, "No link headers in user-owned resource <" + managedResource.url + ">");
//            }
            final URL url = this.getManagerUrlForManagedResource();
            Resource instanceResource = this._resourceAccessor.getResource(this._shapeTreeContext, url);
            if (instanceResource instanceof ManagerResource) {
                this.managerResource = Optional.of(managerResource = (ManagerResource) instanceResource);
            } else {
                throw new IllegalStateException("Dereferencing <" + url + "> did not yield a ManageableResource");
            }
        } else {
            managerResource = this.managerResource.get();
        }
        return managerResource;
    }

    protected URL getManagerUrlForManagedResource() throws ShapeTreeException {
        ManageableResource manageableResource = this.manageableResource.orElseThrow(unintialized_instanceResource);
        final List<String> linkHeaderValues = manageableResource.attributes.allValues(HttpHeaders.LINK.getValue());
        ResourceAttributes linkHeaders = ResourceAttributes.parseLinkHeaders(linkHeaderValues);

        final URL base = manageableResource.url;
        if (linkHeaders.firstValue(LinkRelations.MANAGED_BY.getValue()).isEmpty()) {
            log.error("The resource {} does not contain a link header of {}", base, LinkRelations.MANAGED_BY.getValue());
            throw new ShapeTreeException(500, "The resource <" + base + "> has no Link header with relation of " + LinkRelations.MANAGED_BY.getValue() + " found");
        }
        String managerUrlString = linkHeaders.firstValue(LinkRelations.MANAGED_BY.getValue()).orElseThrow(
                () -> new ShapeTreeException(500, "No Link header with relation of " + LinkRelations.MANAGED_BY.getValue() + " found")
        );
        try {
            return new URL(base, managerUrlString);
        } catch (MalformedURLException e) {
            // If we can't do relative URL resolution, assume that the manager is a URL and we have some other means of resolving it.
            try {
                return new URL(managerUrlString);
            } catch (MalformedURLException ex) {
                throw new IllegalStateException("Malformed relative URL <" + managerUrlString + "> (resolved from <" + base + ">)");
            }
        }
    }

    public void createOrUpdateManagerResource(ShapeTreeManager manager) throws ShapeTreeException {
        ManagerResource managerResource = this.getManagerResource();
        if (!managerResource.wasSuccessful()) {
            // create manager resource if it doesn't exist
            ResourceAttributes headers = new ResourceAttributes();
            headers.setAll(HttpHeaders.CONTENT_TYPE.getValue(), Collections.singletonList(TEXT_TURTLE));
            this._resourceAccessor.createResource(this._shapeTreeContext,"POST", managerResource.url, headers, manager.getGraph().toString(), TEXT_TURTLE);
        } else {
            // Update the existing manager resource for the managed resource
            this._resourceAccessor.updateResource(this._shapeTreeContext, "PUT", managerResource, manager.getGraph().toString());
        }
    }

    static final Supplier<IllegalStateException> unintialized_instanceResource = () -> new IllegalStateException("Unintialized shape tree instance resource");

    static public class Resource {
        final protected URL url;
        final protected ShapeTreeResourceType resourceType;
        final protected ResourceAttributes attributes;
        final protected String body;
        final protected String name;
        final protected boolean exists;

        Resource(URL url, ShapeTreeResourceType resourceType, ResourceAttributes attributes, String body, String name, boolean exists) {
            this.url = url;
            this.resourceType = resourceType;
            this.attributes = attributes;
            this.body = body;
            this.name = name;
            this.exists = exists;
        }

        public URL getUrl() {
            return this.url;
        }
        public ShapeTreeResourceType getResourceType() {
            return this.resourceType;
        }
        public ResourceAttributes getAttributes() {
            return this.attributes;
        }
        public String getBody() {
            return this.body;
        }
        public String getName() {
            return this.name;
        }
        public boolean wasSuccessful() {
            return this.exists;
        }
        public Graph getGraph(URL baseUrl) throws ShapeTreeException {
            if (!this.wasSuccessful()) { return null; }
            final URI baseUri = urlToUri(baseUrl);
            return GraphHelper.readStringIntoGraph(baseUri, this.getBody(), this.getAttributes().firstValue(HttpHeaders.CONTENT_TYPE.getValue()).orElse(null));
        }
    }

    static public class ManageableResource extends Resource {
        final protected Optional<URL> managerResourceUrl;
        final protected boolean _container;

        public ManageableResource(URL url, ShapeTreeResourceType resourceType, ResourceAttributes attributes, String body, String name, boolean exists, Optional<URL> managerResourceUrl, boolean isContainer) {
            super(url, resourceType, attributes, body, name, exists);
            this.managerResourceUrl = managerResourceUrl;
            this._container = isContainer;
        }

        public Optional<URL> getManagerResourceUrl() {
            return this.managerResourceUrl;
        }

        public URL getParentContainerUrl() throws ShapeTreeException {
            final String rel = this.isContainer() ? ".." : ".";
            try {
                return new URL(this.getUrl(), rel);
            } catch (MalformedURLException e) {
                throw new ShapeTreeException(500, "Malformed focus node when resolving <" + rel + "> against <" + this.getUrl() + ">");
            }
        }

        public boolean isContainer() {
            return this._container;
        }
    }

    static public class ManagerResource extends Resource {
        final protected URL managedResourceUrl;

        public ManagerResource(URL url, ShapeTreeResourceType resourceType, ResourceAttributes attributes, String body, String name, boolean exists, URL managedResourceUrl) {
            super(url, resourceType, attributes, body, name, exists);
            this.managedResourceUrl = managedResourceUrl;
        }

        public URL getManagedResourceUrl() {
            return this.managedResourceUrl;
        }

        public ShapeTreeManager getManager() throws ShapeTreeException {
            if (!this.wasSuccessful()) { return null; }
            Graph managerResourceGraph = this.getGraph(this.getUrl());
            if (managerResourceGraph == null) { return null; }
            return ShapeTreeManager.getFromGraph(this.getUrl(), managerResourceGraph);
        }

    }
}
