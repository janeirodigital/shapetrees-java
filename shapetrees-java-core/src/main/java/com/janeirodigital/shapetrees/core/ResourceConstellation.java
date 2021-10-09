package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;

import java.net.URI;
import java.util.Optional;

public class ResourceConstellation {
    protected ResourceAccessor _resourceAccessor;
    protected ShapeTreeContext _shapeTreeContext;

    private URI _userResourceUri;
    protected Optional<ShapeTreeResource> _userResource;

    private URI _metadataResourceUri;
    protected Optional<ShapeTreeResource> _metadataResource;

    protected boolean _isManaged;
    protected boolean _isMetadata;

    public ResourceConstellation(URI uri, ResourceAccessor resourceAccessor, ShapeTreeContext shapeTreeContext) throws ShapeTreeException {
        this._userResourceUri = uri;
        this._resourceAccessor = resourceAccessor;
        this._shapeTreeContext = shapeTreeContext;
        ShapeTreeResource res = resourceAccessor.getResource(shapeTreeContext, uri);
        if (res.isMetadata()) {
            this._isMetadata = true;
            this._userResource = Optional.empty();
            this._userResourceUri = null;
            this._metadataResource = Optional.of(res);
            this._metadataResourceUri = uri;
            this._isManaged = true;
        } else {
            this._isMetadata = false;
            this._userResource = Optional.of(res);
            this._userResourceUri = uri;
            this._metadataResource = Optional.empty();
            this._metadataResourceUri = null;
            this._isManaged = res.isManaged(); // TODO test !isManaged.
        }
    }
    public boolean isManaged() { return this._isManaged; }
    public boolean isMetadata() { return this._isMetadata; }
    public ShapeTreeContext getShapeTreeContext() { return this._shapeTreeContext; }
    public ShapeTreeResource getUserResource() throws ShapeTreeException {
        if (this._userResource.isEmpty()) {
            this._userResource = Optional.of(this._resourceAccessor.getResource(this._shapeTreeContext, this._userResourceUri));
        }
        return this._userResource.get();
    }
    public ShapeTreeResource getMetadataResource() throws ShapeTreeException {
        if (this._metadataResource.isEmpty()) {
            this._metadataResource = Optional.of(this._resourceAccessor.getResource(this._shapeTreeContext, this._metadataResourceUri));
        }
        return this._metadataResource.get();
    }

    public class UserResource {
    }

    public class MetadataResource {

    }
}
