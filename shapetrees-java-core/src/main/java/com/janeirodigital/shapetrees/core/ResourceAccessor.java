package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;

import java.net.URI;
import java.util.List;

public interface ResourceAccessor {
    ShapeTreeResource getResource(ShapeTreeContext context, URI resourceURI) throws ShapeTreeException;
    ShapeTreeResource createResource(ShapeTreeContext context, String method, URI resourceURI, ResourceAttributes headers, String body, String contentType) throws ShapeTreeException;
    ShapeTreeResource updateResource(ShapeTreeContext context, String method, ResourceConstellation.ResourceFork updatedResource) throws ShapeTreeException;
    DocumentResponse deleteResource(ShapeTreeContext context, ResourceConstellation.MetadataResource updatedResource) throws ShapeTreeException;
    List<ResourceConstellation> getContainedResources(ShapeTreeContext context, URI containerResourceURI) throws ShapeTreeException;
}
