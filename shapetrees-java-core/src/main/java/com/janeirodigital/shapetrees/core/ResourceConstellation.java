package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.core.enums.LinkRelations;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;

import java.net.URI;
import java.util.List;
import java.util.Optional;

public class ResourceConstellation {
    // access parameters
    protected ResourceAccessor _resourceAccessor;
    protected ShapeTreeContext _shapeTreeContext;

    // components
    private UserOwnedResource userOwnedResource = new UserOwnedResource();
    private MetadataResource metadataResource = new MetadataResource();

    // discovered attributes
    protected boolean _isMetadata;

    public ResourceConstellation(URI uri, ResourceAccessor resourceAccessor, ShapeTreeContext shapeTreeContext) throws ShapeTreeException {
        this.userOwnedResource.uri = uri;
        this._resourceAccessor = resourceAccessor;
        this._shapeTreeContext = shapeTreeContext;
        ShapeTreeResource res = resourceAccessor.getResource(shapeTreeContext, uri);
        if (res.isMetadata()) {
            this._isMetadata = true;
            this.userOwnedResource.shapeTreeResource = Optional.empty();
            this.userOwnedResource.uri = null;
            this.userOwnedResource._isManaged = true;
            this.userOwnedResource.linkHeaders = Optional.empty();
            this.metadataResource.shapeTreeResource = Optional.of(res);
            this.metadataResource.uri = uri;
        } else {
            this._isMetadata = false;
            this.metadataResource.shapeTreeResource = Optional.empty();
            this.metadataResource.uri = null;
            _setUserOwnedResource(uri, res);
        }
    }
    protected void _setUserOwnedResource(URI uri, ShapeTreeResource res) {
        this.userOwnedResource.shapeTreeResource = Optional.of(res);
        this.userOwnedResource.uri = uri;
        this.userOwnedResource._isManaged = res.isManaged(); // TODO test !isManaged.
        final List<String> linkHeaderValues = res.getAttributes().allValues(HttpHeaders.LINK.getValue());
        this.userOwnedResource.linkHeaders = linkHeaderValues.size() > 0
                ? Optional.of(ResourceAttributes.parseLinkHeaders(linkHeaderValues))
                : Optional.empty();
    }
    public boolean isManaged() { return this.userOwnedResource._isManaged; }
    public boolean isMetadata() { return this._isMetadata; }
    public ShapeTreeContext getShapeTreeContext() { return this._shapeTreeContext; }

    public ShapeTreeResource getUserOwnedResource() throws ShapeTreeException {
        if (this.userOwnedResource.shapeTreeResource.isEmpty()) {
            final ShapeTreeResource str = this.metadataResource.shapeTreeResource.orElseThrow(
                    () -> new Error("ResourceConstellation for <" + metadataResource.uri + ">")
            );
            URI uri = str.getAssociatedUri().orElseThrow(
                    () -> new ShapeTreeException(500, "Can't find resource managed by <" + metadataResource.uri + ">")
            );
            ShapeTreeResource userRes = this._resourceAccessor.getResource(this._shapeTreeContext, uri);
            _setUserOwnedResource(uri, userRes);
            this.userOwnedResource.shapeTreeResource = Optional.of(userRes);
        }
        return this.userOwnedResource.shapeTreeResource.get();
    }
    public ShapeTreeResource getMetadataResource() throws ShapeTreeException {
        if (this.metadataResource.shapeTreeResource.isEmpty()) {
            if (this.userOwnedResource.linkHeaders.isEmpty()) {
                throw new ShapeTreeException(500, "No link headers in user-owned resource <" + this.userOwnedResource.uri + ">");
            }

            ResourceAttributes linkHeaders = this.userOwnedResource.linkHeaders.get();
            // moved from AbstractValidatingMethodHandler.getShapeTreeMetadataURIForResource999()
            if (linkHeaders.firstValue(LinkRelations.SHAPETREE_LOCATOR.getValue()).isEmpty()) {
                // TODO: log.error("The resource {} does not contain a link header of {}", this.userOwnedResource.uri, LinkRelations.SHAPETREE_LOCATOR.getValue());
                throw new ShapeTreeException(500, "The resource <" + this.userOwnedResource.uri + "> has no Link header with relation of " + LinkRelations.SHAPETREE_LOCATOR.getValue() + " found");
            }
            String metaDataURIString = linkHeaders.firstValue(LinkRelations.SHAPETREE_LOCATOR.getValue()).orElse(null);
            if (metaDataURIString != null && metaDataURIString.startsWith("/")) {
                // If the header value doesn't include scheme/host, prefix it with the scheme & host from container
                URI shapeTreeContainerURI = this.userOwnedResource.uri;
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

            this.metadataResource.uri = URI.create(metaDataURIString);

            this.metadataResource.shapeTreeResource = Optional.of(this._resourceAccessor.getResource(this._shapeTreeContext, this.metadataResource.uri));
        }
        return this.metadataResource.shapeTreeResource.get();
    }

    public class UserOwnedResource {
        private URI uri;
        protected Optional<ShapeTreeResource> shapeTreeResource;
        protected Optional<ResourceAttributes> linkHeaders;
        protected boolean _isManaged;
    }

    public class MetadataResource {
        private URI uri;
        protected Optional<ShapeTreeResource> shapeTreeResource;
    }
}
