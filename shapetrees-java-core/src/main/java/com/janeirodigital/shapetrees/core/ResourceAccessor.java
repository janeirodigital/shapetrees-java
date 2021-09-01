package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;

import java.net.URI;
import java.util.List;

public interface ResourceAccessor {
    ShapeTreeResource getResource(ShapeTreeContext context, URI resourceURI) throws ShapeTreeException;
    ShapeTreeResource createResource(ShapeTreeContext context, String method, URI resourceURI, ResourceAttributes headers, String body, String contentType) throws ShapeTreeException;
    ShapeTreeResource updateResource(ShapeTreeContext context, String method, ShapeTreeResource updatedResource) throws ShapeTreeException;
    ShapeTreeResponse deleteResource(ShapeTreeContext context, ShapeTreeResource updatedResource) throws ShapeTreeException;
    List<ShapeTreeResource> getContainedResources(ShapeTreeContext context, URI containerResourceURI) throws ShapeTreeException;
}
