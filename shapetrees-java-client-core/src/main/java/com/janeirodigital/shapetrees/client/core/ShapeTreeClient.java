package com.janeirodigital.shapetrees.client.core;

import com.janeirodigital.shapetrees.core.ShapeTreeResponse;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;
import com.janeirodigital.shapetrees.core.models.ShapeTreeLocator;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;

/**
 * This interface defines a proposed API to be used for any client-side implementations of
 * a shape tree client
 */
public interface ShapeTreeClient {

    /**
     * Shape Trees, §4.1: This operation is used by a client-side agent to discover any shape trees associated
     * with a given resource. If URI is a managed resource, the associated Shape Tree Locator will be returned.
     *
     * https://shapetrees.org/TR/specification/#discover
     *
     * @param context ShapeTreeContext that would be used for authentication purposes
     * @param targetResource The URI of the target resource for shape tree discovery
     * @return A ShapeTreeLocator associated with targetResource
     * @throws IOException IOException
     */
    ShapeTreeLocator discoverShapeTree(ShapeTreeContext context, URI targetResource) throws IOException;

    /**
     * Shape Trees, §4.2: This operation marks an existing resource as being managed by one or more shape trees,
     * by associating a shape tree locator with the resource, and turning it into a managed resource.
     *
     * If the resource is already managed, the associated shape tree locator will be updated with another
     * shape tree location for the planted shape tree.
     *
     * If the resource is a container that already contains existing resources, and a recursive plant is requested,
     * this operation will perform a depth first traversal through the containment hierarchy, validating
     * and assigning as it works its way back up to the target root resource of this operation.
     *
     * https://shapetrees.org/TR/specification/#plant-shapetree
     *
     * Plants one or more shape trees at a given container
     * @param context ShapeTreeContext that would be used for authentication purposes
     * @param targetResource The URI of the resource to plant on
     * @param targetShapeTree A URI representing the shape tree to plant for targetResource
     * @param focusNode An optional URI representing the target subject within targetResource used for shape validation
     * @return ShapeTreeResponse containing status and response headers/attributes
     * @throws IOException IOException
     * @throws URISyntaxException URISyntaxException
     */
    ShapeTreeResponse plantShapeTree(ShapeTreeContext context, URI targetResource, URI targetShapeTree, String focusNode) throws IOException, URISyntaxException;

    /**
     * Shape Trees, §4.3: This operation unassigns a planted root shape tree from a root shape tree instance. If
     * the root shape tree instance is a managed container, it will also unassign contained resources.
     * If there are no remaining shape trees managing the resource, it would no longer be considered as managed.
     *
     * https://shapetrees.org/TR/specification/#unplant-shapetree
     *
     * @param context ShapeTreeContext that would be used for authentication purposes
     * @param targetResource Container to remove shape tree from
     * @param targetShapeTree URI of shape tree being removed
     */
    ShapeTreeResponse unplantShapeTree(ShapeTreeContext context, URI targetResource, URI targetShapeTree) throws IOException, URISyntaxException;

    /**
     * Creates a resource via HTTP POST that has been validated against the provided shape tree
     * @param context ShapeTreeContext that would be used for authentication purposes
     * @param parentContainer The container the created resource should be created within
     * @param focusNode The node/subject to use for validation purposes
     * @param targetShapeTree The shape tree the body should be validated by
     * @param proposedName Proposed resource name (aka Slug) for the resulting resource
     * @param isContainer Specifies whether the newly created resource should be created as a container or not
     * @param bodyString String representation of body of the created resource
     * @param contentType Content type to parse the bodyString parameter as
     * @return ShapeTreeResponse containing status and response headers/attributes
     * @throws IOException IOException
     * @throws URISyntaxException URISyntaxException
     */
    ShapeTreeResponse postShapeTreeInstance(ShapeTreeContext context, URI parentContainer, URI focusNode, URI targetShapeTree, String proposedName, Boolean isContainer, String bodyString, String contentType) throws IOException, URISyntaxException;

    /**
     * Creates a resource via HTTP PUT that has been validated against the provided target shape tree
     * @param context ShapeTreeContext that would be used for authentication purposes
     * @param targetResource The target resource to be created or updated
     * @param focusNode The node/subject to use for validation purposes
     * @param targetShapeTree The shape tree that a proposed resource to be created should be validated against
     * @param isContainer Specifies whether a newly created resource should be created as a container or not
     * @param bodyString String representation of the body of the resource to create or update
     * @param contentType Content type to parse the bodyString parameter as
     * @return ShapeTreeResponse containing status and response header / attributes
     * @throws IOException
     * @throws URISyntaxException
     */
    ShapeTreeResponse putShapeTreeInstance(ShapeTreeContext context, URI targetResource, URI focusNode, URI targetShapeTree, Boolean isContainer, String bodyString, String contentType) throws IOException, URISyntaxException;

    /**
     * Updates a resource via HTTP PUT that has been validated against an associated shape tree
     * @param context ShapeTreeContext that would be used for authentication purposes
     * @param targetResource The target resource to be created or updated
     * @param focusNode The node/subject to use for validation purposes
     * @param bodyString String representation of the body of the resource to create or update
     * @param contentType Content type to parse the bodyString parameter as
     * @return ShapeTreeResponse containing status and response header / attributes
     * @throws IOException
     * @throws URISyntaxException
     */
    ShapeTreeResponse putShapeTreeInstance(ShapeTreeContext context, URI targetResource, URI focusNode, String bodyString, String contentType) throws IOException, URISyntaxException;

    /**
     * Updates a resource via HTTP PATCH that has been validated against an associated shape tree
     * @param context ShapeTreeContext that would be used for authentication purposes
     * @param targetResource The target resource to be created or updated
     * @param focusNode The node/subject to use for validation purposes
     * @param patchString SPARQL Update statement to use in patching the resource
     * @return ShapeTreeResponse containing status and response header / attributes
     * @throws IOException
     * @throws URISyntaxException
     */
    ShapeTreeResponse patchShapeTreeInstance(ShapeTreeContext context, URI targetResource, URI focusNode, String patchString) throws IOException, URISyntaxException;

    /**
     * Deletes an existing resource.  Provided as a convenience - no validation is performed
     * @param context ShapeTreeContext that would be used for authentication purposes
     * @param resourceURI The URI of the resource being updated
     * @return ShapeTreeResponse containing status and response headers/attributes
     * @throws IOException IOException
     */
    ShapeTreeResponse deleteShapeTreeInstance(ShapeTreeContext context, URI resourceURI) throws IOException;

    /**
     * Indicates whether validation is currently being applied on the client
     * @return boolean of whether client-side validation is being performed
     */
    boolean isValidationSkipped();

    /**
     * Determines whether validation should be performed on the client
     * @param skipValidation boolean indicating whether validation should be performed on the client
     */
    void skipValidation(boolean skipValidation);
}
