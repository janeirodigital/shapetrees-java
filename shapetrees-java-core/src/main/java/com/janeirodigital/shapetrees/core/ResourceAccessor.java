package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;

import java.net.URI;
import java.util.List;

public interface ResourceAccessor {
    ShapeTreeResource.ResourceFork getResource(ShapeTreeContext context, URI resourceURI) throws ShapeTreeException;
    ShapeTreeResource.ResourceFork createResource(ShapeTreeContext context, String method, URI resourceURI, ResourceAttributes headers, String body, String contentType) throws ShapeTreeException;
    DocumentResponse updateResource(ShapeTreeContext context, String method, ShapeTreeResource.ResourceFork updatedResource, String body) throws ShapeTreeException;
    DocumentResponse deleteResource(ShapeTreeContext context, ShapeTreeResource.MetadataResource updatedResource) throws ShapeTreeException;
    List<ShapeTreeResource> getContainedResources(ShapeTreeContext context, URI containerResourceURI) throws ShapeTreeException;
}
