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
    private UserResource userResource = new UserResource();
    private MetadataResource metadataResource = new MetadataResource();

    // discovered attributes
    protected boolean _isMetadata;

    public ResourceConstellation(URI uri, ResourceAccessor resourceAccessor, ShapeTreeContext shapeTreeContext) throws ShapeTreeException {
        this.userResource.uri = uri;
        this._resourceAccessor = resourceAccessor;
        this._shapeTreeContext = shapeTreeContext;
        ShapeTreeResource res = resourceAccessor.getResource(shapeTreeContext, uri);
        if (res.isMetadata()) {
            this._isMetadata = true;
            this.userResource.resource = Optional.empty();
            this.userResource.uri = null;
            this.metadataResource.resource = Optional.of(res);
            this.metadataResource.uri = uri;
            this.userResource._isManaged = true;
        } else {
            this._isMetadata = false;
            this.userResource.resource = Optional.of(res);
            this.userResource.uri = uri;
            this.metadataResource.resource = Optional.empty();
            this.metadataResource.uri = null;
            this.userResource._isManaged = res.isManaged(); // TODO test !isManaged.
        }
    }
    public boolean isManaged() { return this.userResource._isManaged; }
    public boolean isMetadata() { return this._isMetadata; }
    public ShapeTreeContext getShapeTreeContext() { return this._shapeTreeContext; }
    public ShapeTreeResource getUserResource() throws ShapeTreeException {
        if (this.userResource.resource.isEmpty()) {
            this.userResource.resource = Optional.of(this._resourceAccessor.getResource(this._shapeTreeContext, this.userResource.uri));
        }
        return this.userResource.resource.get();
    }
    public ShapeTreeResource getMetadataResource() throws ShapeTreeException {
        if (this.metadataResource.resource.isEmpty()) {
            this.metadataResource.resource = Optional.of(this._resourceAccessor.getResource(this._shapeTreeContext, this.metadataResource.uri));
        }
        return this.metadataResource.resource.get();
    }

    public class UserResource {
        private URI uri;
        protected Optional<ShapeTreeResource> resource;
        protected boolean _isManaged;
    }

    public class MetadataResource {
        private URI uri;
        protected Optional<ShapeTreeResource> resource;
    }
}
