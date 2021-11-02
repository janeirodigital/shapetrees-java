package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;

import java.net.URL;
import java.util.List;

public interface ResourceAccessor {
    ShapeTreeInstance.Resource getResource(ShapeTreeContext context, URL resourceUrl) throws ShapeTreeException;
    ShapeTreeInstance.Resource createResource(ShapeTreeContext context, String method, URL resourceUrl, ResourceAttributes headers, String body, String contentType) throws ShapeTreeException;
    DocumentResponse updateResource(ShapeTreeContext context, String method, ShapeTreeInstance.Resource updatedResource, String body) throws ShapeTreeException;
    DocumentResponse deleteResource(ShapeTreeContext context, ShapeTreeInstance.ManagerResource updatedResource) throws ShapeTreeException;
    List<ShapeTreeInstance> getContainedResources(ShapeTreeContext context, URL containerResourceUrl) throws ShapeTreeException;
}
