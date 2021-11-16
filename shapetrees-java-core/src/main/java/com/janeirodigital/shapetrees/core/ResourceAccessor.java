package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;

import java.net.URL;
import java.util.List;

/**
 * Interface used by the shape trees core for accessing manageable instances and resources.
 * Depending upon the context, this could be implemented by an accessor working with resources
 * in a database or filesystem (typical of server-side processing), or by an accessor that
 * is working with remote resources over http (typical of client-side processing).
 *
 * Note that create and update make the assumption that create and update requests are
 * originating from HTTP requests regardless of context (hence the inclusion of method,
 * headers, and contentType).
 *
 * Deletion and Update of ManageableInstances aren't supported, as both should be targeted
 * specifically to either a ManageableResource or ManagerResource with <code>deleteResource</code>
 * or <code>updateResource</code>.
 */
public interface ResourceAccessor {
    ManageableInstance getInstance(ShapeTreeContext context, URL resourceUrl) throws ShapeTreeException;
    ManageableInstance createInstance(ShapeTreeContext context, String method, URL resourceUrl, ResourceAttributes headers, String body, String contentType) throws ShapeTreeException;
    List<ManageableInstance> getContainedInstances(ShapeTreeContext context, URL containerResourceUrl) throws ShapeTreeException;
    InstanceResource getResource(ShapeTreeContext context, URL resourceUrl) throws ShapeTreeException;
    InstanceResource createResource(ShapeTreeContext context, String method, URL resourceUrl, ResourceAttributes headers, String body, String contentType) throws ShapeTreeException;
    DocumentResponse updateResource(ShapeTreeContext context, String method, InstanceResource updatedResource, String body) throws ShapeTreeException;
    DocumentResponse deleteResource(ShapeTreeContext context, ManagerResource updatedResource) throws ShapeTreeException;
}
