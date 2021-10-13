package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.core.enums.LinkRelations;
import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;
import com.janeirodigital.shapetrees.core.models.ShapeTreeLocator;
import org.apache.jena.graph.Graph;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.function.Supplier;

/* migration strategy:
   0. resolve https://github.com/xformativ/shapetrees-java/issues/86
   1. make ResourceFork API emulate ShapeTreeResource
   2. atomic(delete ShapeTreeResource, s/ResourceFork/ShapeTreeResource/g)
 */
public class ResourceConstellation {
    public static final String TEXT_TURTLE = "text/turtle";

    // access parameters
    protected ResourceAccessor _resourceAccessor;
    protected ShapeTreeContext _shapeTreeContext;

    // components
    private Optional<UserOwnedResource> userOwnedResource = Optional.empty();
    private Optional<MetadataResource> metadataResource = Optional.empty();

    // discovered attributes
    protected boolean _isMetadata;
    protected boolean _isManaged;
    protected boolean _isContainer999;

    public ResourceConstellation(URI uri, ResourceAccessor resourceAccessor, ShapeTreeContext shapeTreeContext) throws ShapeTreeException {
        this._resourceAccessor = resourceAccessor;
        this._shapeTreeContext = shapeTreeContext;
        ShapeTreeResource res = resourceAccessor.getResource(shapeTreeContext, uri);
        _init(uri, res);
    }

    public ResourceConstellation(URI uri, ResourceAccessor resourceAccessor, ShapeTreeContext shapeTreeContext, ShapeTreeRequest shapeTreeRequest) throws ShapeTreeException {
        this._resourceAccessor = resourceAccessor;
        this._shapeTreeContext = shapeTreeContext;
        DocumentResponse res = resourceAccessor.createResource(shapeTreeContext, shapeTreeRequest.getMethod(), uri, shapeTreeRequest.getHeaders(), shapeTreeRequest.getBody(), shapeTreeRequest.getContentType());
        _init(uri, new ShapeTreeResource(uri, res));
    }

    protected void _init(URI uri, ShapeTreeResource res) {
        if (res.isMetadata()) {
            this._isMetadata = true;
            this._isManaged = true;
            this._isContainer999 = false; // TODO @@ I guess it's not known to be a Container...
            MetadataResource mr = new MetadataResource();
            this.metadataResource = Optional.of(mr);
            _setMetadataResource(uri, res);
        } else {
            this._isMetadata = false;
            final UserOwnedResource uor = new UserOwnedResource();
            this.userOwnedResource = Optional.of(uor);
            _setUserOwnedResource(uri, res);
        }
    }

    protected void _setResourceFork(ResourceFork fork, URI uri, ShapeTreeResource res) {
        fork.sTResource = res;
        fork.uri = uri;
        fork.body = res.getBody();
        fork.attributes = res.getAttributes();
        fork._isExists = res.isExists();
        fork._resourceType = res.getResourceType();
        fork.name = res.getName();
    }

    protected void _setUserOwnedResource(URI uri, ShapeTreeResource res) {
        final UserOwnedResource uor = userOwnedResource.orElseThrow(unintialized_resourceFork);
        _setResourceFork(uor, uri, res);
        uor.metadataResourceUri = res.getAssociatedUri();
        this._isManaged = uor._isManaged = res.isManaged(); // TODO test !isManaged.
        this._isContainer999 = uor._isContainer  = res.isContainer();
        final List<String> linkHeaderValues = res.getAttributes().allValues(HttpHeaders.LINK.getValue());
        uor.linkHeaders = linkHeaderValues.size() > 0
                ? Optional.of(ResourceAttributes.parseLinkHeaders(linkHeaderValues))
                : Optional.empty();
    }

    protected void _setMetadataResource(URI uri, ShapeTreeResource res) {
        final MetadataResource mr = metadataResource.orElseThrow(unintialized_resourceFork);
        _setResourceFork(mr, uri, res);
        mr.userOwnedResourceUri = res.getAssociatedUri();
        mr.graph = res.getGraph();
    }

    public boolean isManaged() { return this._isManaged; }
    public boolean isMetadata() { return this._isMetadata; }
    public boolean isContainer999() { return this._isContainer999; }
    public ShapeTreeContext getShapeTreeContext() { return this._shapeTreeContext; }

    public UserOwnedResource getUserOwnedResourceFork() throws ShapeTreeException {
        UserOwnedResource uor;
        if (this.userOwnedResource.isEmpty()) {
            MetadataResource mr = this.metadataResource.orElseThrow(unintialized_resourceFork);
            // @see https://github.com/xformativ/shapetrees-java/issues/86
//            if (mr.linkHeaders.isEmpty()) {
//                throw new ShapeTreeException(500, "No link headers in metadata resource <" + mr.uri + ">");
//            }
            uor = new UserOwnedResource();
            this.userOwnedResource = Optional.of(uor);
            URI uri = mr.getUserOwnedResourceUri().get();
            ShapeTreeResource userRes = this._resourceAccessor.getResource(this._shapeTreeContext, uri);
            _setUserOwnedResource(uri, userRes);
        } else {
            uor = this.userOwnedResource.get();
        }
        return uor;
    }

    public MetadataResource getMetadataResourceFork() throws ShapeTreeException {
        MetadataResource mr;
        if (this.metadataResource.isEmpty()) {
            UserOwnedResource uor = this.userOwnedResource.orElseThrow(unintialized_resourceFork);
            if (uor.linkHeaders.isEmpty()) {
                throw new ShapeTreeException(500, "No link headers in user-owned resource <" + uor.uri + ">");
            }
            mr = new MetadataResource();
            this.metadataResource = Optional.of(mr);
            final URI mDUri = this.getShapeTreeMetadataURIForResource();
            final ShapeTreeResource mdRes = this._resourceAccessor.getResource(this._shapeTreeContext, mDUri);
            _setMetadataResource(mDUri, mdRes);
        } else {
            mr = this.metadataResource.get();
        }
        return mr;
    }

    protected URI getShapeTreeMetadataURIForResource() throws ShapeTreeException {
        UserOwnedResource uor = this.userOwnedResource.orElseThrow(unintialized_resourceFork);
        ResourceAttributes linkHeaders = uor.linkHeaders.get();

        if (linkHeaders.firstValue(LinkRelations.SHAPETREE_LOCATOR.getValue()).isEmpty()) {
            // TODO: log.error("The resource {} does not contain a link header of {}", this.userOwnedResource.uri, LinkRelations.SHAPETREE_LOCATOR.getValue());
            throw new ShapeTreeException(500, "The resource <" + uor.uri + "> has no Link header with relation of " + LinkRelations.SHAPETREE_LOCATOR.getValue() + " found");
        }
        String metaDataURIString = linkHeaders.firstValue(LinkRelations.SHAPETREE_LOCATOR.getValue()).orElse(null);
        if (metaDataURIString != null && metaDataURIString.startsWith("/")) {
            // If the header value doesn't include scheme/host, prefix it with the scheme & host from container
            URI shapeTreeContainerURI = uor.uri;
            String portFragment;
            if (shapeTreeContainerURI.getPort() > 0) {
                portFragment = ":" + shapeTreeContainerURI.getPort();
            } else {
                portFragment = "";
            }
            metaDataURIString = shapeTreeContainerURI.getScheme() + "://" + shapeTreeContainerURI.getHost() + portFragment + metaDataURIString;
        }

        if (metaDataURIString == null) {
            throw new ShapeTreeException(500, "No Link header with relation of " + LinkRelations.SHAPETREE_LOCATOR.getValue() + " found");
        }

        return URI.create(metaDataURIString);
    }

    public void createOrUpdateMetadataResource(ShapeTreeLocator primaryResourceLocator) throws ShapeTreeException, URISyntaxException {
        MetadataResource mr = this.getMetadataResourceFork();
        DocumentResponse res;
        if (!mr.sTResource.isExists()) {
            // create primary metadata resource if it doesn't exist
            ResourceAttributes headers = new ResourceAttributes();
            headers.setAll(HttpHeaders.CONTENT_TYPE.getValue(), Collections.singletonList(TEXT_TURTLE));
            res = this._resourceAccessor.createResource(this._shapeTreeContext,"POST", mr.uri, headers, primaryResourceLocator.getGraph().toString(), TEXT_TURTLE);
        } else {
            // Update the existing metadata resource for the primary resource
            mr.sTResource.setBody(primaryResourceLocator.getGraph().toString());
            res = this._resourceAccessor.updateResource(this._shapeTreeContext, "PUT", mr);
        }
        this._init(mr.uri, new ShapeTreeResource(mr.uri, res));
    }

    static final Supplier<IllegalStateException> unintialized_resourceFork = () -> new IllegalStateException("unintialized ResourceFork");
    public class ResourceFork { // TODO: abstract with helpful toString() for error messages
        protected URI uri;
        protected ShapeTreeResource sTResource;
        protected String body;
        protected ResourceAttributes attributes;
        protected String name;

        public String getName() {
            return this.name;
        }

        public ShapeTreeResourceType getResourceType() {
            return this._resourceType;
        }


        protected ShapeTreeResourceType _resourceType;

        protected boolean _isExists;

        public String getBody() {
            return this.body;
        }

        public void setBody(String body) {
            this.body = body;
            this.sTResource.setBody(body);
        }

        public ResourceAttributes getAttributes() {
            return this.attributes;
        }

        public URI getUri() {
            return this.uri;
        }

        public boolean isExists() {
            return this.sTResource.isExists();
        }
    }

    public class UserOwnedResource extends ResourceFork {
        protected Optional<ResourceAttributes> linkHeaders;

        protected boolean _isManaged;
        protected boolean _isContainer;
        public Optional<URI> getMetadataResourceUri() {
            return this.metadataResourceUri;
        }

        protected Optional<URI> metadataResourceUri;

        public boolean isManaged() {
            return this._isManaged;
        }
        public Optional<ResourceAttributes> getLinkHeaders() {
            return this.linkHeaders;
        }

        public boolean isContainer() {
            return this._isContainer;
        }


    }

    public class MetadataResource extends ResourceFork {
        protected Optional<URI> userOwnedResourceUri;

        protected Optional<Graph> graph;
        public Optional<URI> getUserOwnedResourceUri() {
            return this.userOwnedResourceUri;
        }

        public Optional<Graph> getGraph() {
            return this.graph;
        }
    }
}
