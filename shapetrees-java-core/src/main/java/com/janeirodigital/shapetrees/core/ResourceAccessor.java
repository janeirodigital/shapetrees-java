package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;

import java.net.URL;
import java.util.List;

public interface ResourceAccessor {
    ManageableInstance.Resource getResource(ShapeTreeContext context, URL resourceUrl) throws ShapeTreeException;
    ManageableInstance.Resource createResource(ShapeTreeContext context, String method, URL resourceUrl, ResourceAttributes headers, String body, String contentType) throws ShapeTreeException;
    DocumentResponse updateResource(ShapeTreeContext context, String method, ManageableInstance.Resource updatedResource, String body) throws ShapeTreeException;
    DocumentResponse deleteResource(ShapeTreeContext context, ManageableInstance.ManagerResource updatedResource) throws ShapeTreeException;
    List<ManageableInstance> getContainedResources(ShapeTreeContext context, URL containerResourceUrl) throws ShapeTreeException;
}
