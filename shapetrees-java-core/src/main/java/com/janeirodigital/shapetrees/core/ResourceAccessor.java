package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;
import org.jetbrains.annotations.NotNull;

import java.net.URI;
import java.util.List;

public interface ResourceAccessor {
    ShapeTreeResource.Fork getResource(ShapeTreeContext context, URI resourceURI) throws ShapeTreeException;
    ShapeTreeResource.Fork createResource(ShapeTreeContext context, String method, URI resourceURI, ResourceAttributes headers, @NotNull String body, String contentType) throws ShapeTreeException;
    DocumentResponse updateResource(ShapeTreeContext context, String method, ShapeTreeResource.Fork updatedResource, String body) throws ShapeTreeException;
    DocumentResponse deleteResource(ShapeTreeContext context, ShapeTreeResource.Metadata updatedResource) throws ShapeTreeException;
    List<ShapeTreeResource> getContainedResources(ShapeTreeContext context, URI containerResourceURI) throws ShapeTreeException;
}
