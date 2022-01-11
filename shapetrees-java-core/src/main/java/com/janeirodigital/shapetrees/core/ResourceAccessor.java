package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;

import java.net.URL;

/**
 * Interface used by the shape trees core for accessing individual {@link InstanceResource}s.
 *
 * <p>Depending upon the context, this could be implemented by a <code>ResourceAccessor</code> implementation
 * accessing a database or filesystem (typical of server-side processing), or by a <code>ResourceAccessor</code>
 * implementation that is working with remote resources over http (typical of client-side processing).</p>
 */
public interface ResourceAccessor {

    /**
     * Gets a specific {@link InstanceResource} identified by the provided <code>resourceUrl</code>.
     * @param context {@link ShapeTreeContext}
     * @param resourceUrl URL of the target resource to get
     * @return {@link InstanceResource}
     * @throws ShapeTreeException
     */
    InstanceResource getResource(ShapeTreeContext context, URL resourceUrl) throws ShapeTreeException;

    /**
     * Creates a specific {@link InstanceResource} identified by the provided <code>resourceUrl</code>.
     * @param context {@link ShapeTreeContext}
     * @param resourceUrl URL of the resource to create
     * @param headers Incoming HTTP headers
     * @param body Body of the resource to create
     * @param contentType Content-type of the resource to create
     * @return {@link InstanceResource}
     * @throws ShapeTreeException
     */
    InstanceResource createResource(ShapeTreeContext context, URL resourceUrl, ResourceAttributes headers, String body, String contentType) throws ShapeTreeException;

    /**
     * Updates a specific {@link InstanceResource} identified by the provided <code>updatedResource</code>
     * @param context {@link ShapeTreeContext}
     * @param updatedResource {@link InstanceResource} to update
     * @param body Updated body of the {@link InstanceResource}
     * @return Updated {@link InstanceResource}
     * @throws ShapeTreeException
     */
    DocumentResponse updateResource(ShapeTreeContext context, InstanceResource updatedResource, String body) throws ShapeTreeException;

    /**
     * Deletes a specific {@link InstanceResource} identified by the provided <code>updatedResource</code>
     * @param context {@link ShapeTreeContext}
     * @param deleteResource {@link InstanceResource} to delete
     * @return Resultant {@link DocumentResponse}
     * @throws ShapeTreeException
     */
    DocumentResponse deleteResource(ShapeTreeContext context, InstanceResource deleteResource) throws ShapeTreeException;

}
