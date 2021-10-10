package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;

import java.net.URI;
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
            this.metadataResource.shapeTreeResource = Optional.of(res);
            this.metadataResource.uri = uri;
            this.userOwnedResource._isManaged = true;
        } else {
            this._isMetadata = false;
            this.userOwnedResource.shapeTreeResource = Optional.of(res);
            this.userOwnedResource.uri = uri;
            this.metadataResource.shapeTreeResource = Optional.empty();
            this.metadataResource.uri = null;
            this.userOwnedResource._isManaged = res.isManaged(); // TODO test !isManaged.
        }
    }
    public boolean isManaged() { return this.userOwnedResource._isManaged; }
    public boolean isMetadata() { return this._isMetadata; }
    public ShapeTreeContext getShapeTreeContext() { return this._shapeTreeContext; }

    public ShapeTreeResource getUserOwnedResource() throws ShapeTreeException {
        if (this.userOwnedResource.shapeTreeResource.isEmpty()) {
            this.userOwnedResource.shapeTreeResource = Optional.of(this._resourceAccessor.getResource(this._shapeTreeContext, this.userOwnedResource.uri));
        }
        return this.userOwnedResource.shapeTreeResource.get();
    }
    public ShapeTreeResource getMetadataResource() throws ShapeTreeException {
        if (this.metadataResource.shapeTreeResource.isEmpty()) {
            this.metadataResource.shapeTreeResource = Optional.of(this._resourceAccessor.getResource(this._shapeTreeContext, this.metadataResource.uri));
        }
        return this.metadataResource.shapeTreeResource.get();
    }

    public class UserOwnedResource {
        private URI uri;
        protected Optional<ShapeTreeResource> shapeTreeResource;
        protected boolean _isManaged;
    }

    public class MetadataResource {
        private URI uri;
        protected Optional<ShapeTreeResource> shapeTreeResource;
    }
}
