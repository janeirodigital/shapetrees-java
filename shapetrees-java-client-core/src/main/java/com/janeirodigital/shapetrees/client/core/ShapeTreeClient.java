package com.janeirodigital.shapetrees.client.core;

import com.janeirodigital.shapetrees.core.ShapeTreeResponse;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;
import com.janeirodigital.shapetrees.core.models.ShapeTreeLocator;
import org.apache.jena.graph.Graph;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;

/**
 * This interface defines a proposed API to be used for any client-side implementations of
 * a shape tree client
 */
public interface ShapeTreeClient {
    /**
     * Discovering a shape tree entails determining which shape tree(s) manage a given resource
     * @param context ShapeTreeContext that would be used for authentication purposes
     * @param targetContainer The URI of the container being discovered
     * @return A list of ShapeTreeLocator classes which describe what shape tree(s) are managing
     * a container
     * @throws IOException IOException
     */
    //TODO: Return a single locator
    //TODO: Take any target resource type, not just a container
    List<ShapeTreeLocator> discoverShapeTree(ShapeTreeContext context, URI targetContainer) throws IOException;

    /**
     * Plants one or more shape trees at a given container
     * @param context ShapeTreeContext that would be used for authentication purposes
     * @param parentContainer The container the shape trees will be planted within
     * @param shapeTreeURIs A list of shape tree URIs to be planted
     * @param focusNode The node/subject to use for validation purposes (optional)
     * @param shapeTreeHint The shape tree the body should be validated by (required if body is populated)
     * @param proposedResourceName Proposed resource name (aka Slug) for the resulting container
     * @param body Graph representation of body to be included in the container graph of the resulting planted container
     * @return The URI of the resulting container
     * @throws IOException IOException
     * @throws URISyntaxException URISyntaxException
     */
    URI plantShapeTree(ShapeTreeContext context, URI parentContainer, List<URI> shapeTreeURIs, String focusNode, URI shapeTreeHint, String proposedResourceName, Graph body) throws IOException, URISyntaxException;

    /**
     * Plants one or more shape trees at a given container
     * @param context ShapeTreeContext that would be used for authentication purposes
     * @param parentContainer The container the shape trees will be planted within
     * @param shapeTreeURIs A list of shape tree URIs to be planted
     * @param focusNode The node/subject to use for validation purposes (optional)
     * @param shapeTreeHint The shape tree the body should be validated by (required if body is populated)
     * @param proposedResourceName Proposed resource name (aka Slug) for the resulting container
     * @param bodyString String representation of body to be included in the container graph of the resulting planted container
     * @param contentType Content type to parse the bodyString parameter as
     * @return The URI of the resulting container
     * @throws IOException IOException
     * @throws URISyntaxException URISyntaxException
     */
    URI plantShapeTree(ShapeTreeContext context, URI parentContainer, List<URI> shapeTreeURIs, String focusNode, URI shapeTreeHint, String proposedResourceName, String bodyString, String contentType) throws IOException, URISyntaxException;

    /**
     * Creates a resource that has been validated against the provided shape tree
     * @param context ShapeTreeContext that would be used for authentication purposes
     * @param parentContainer The container the created resource should be created within
     * @param focusNode The node/subject to use for validation purposes
     * @param shapeTreeHint The shape tree the body should be validated by
     * @param proposedResourceName Proposed resource name (aka Slug) for the resulting resource
     * @param isContainer Specifies whether the newly created resource should be created as a container or not
     * @param bodyString String representation of body of the created resource
     * @param contentType Content type to parse the bodyString parameter as
     * @return ShapeTreeResponse containing status and response headers/attributes
     * @throws IOException IOException
     * @throws URISyntaxException URISyntaxException
     */
    ShapeTreeResponse createDataInstance(ShapeTreeContext context, URI parentContainer, String focusNode, URI shapeTreeHint, String proposedResourceName, Boolean isContainer, String bodyString, String contentType) throws IOException, URISyntaxException;

    /**
     * Updates an existing resource, validating it against the provided shape tree
     * @param context ShapeTreeContext that would be used for authentication purposes
     * @param resourceURI The URI of the resource being updated
     * @param focusNode The node/subject to use for validation purposes
     * @param shapeTreeHint The shape tree the body should be validated by
     * @param bodyString String representation of body of the created resource
     * @param contentType Content type to parse the bodyString parameter as
     * @return ShapeTreeResponse containing status and response headers/attributes
     * @throws IOException IOException
     * @throws URISyntaxException URISyntaxException
     */
    ShapeTreeResponse updateDataInstance(ShapeTreeContext context, URI resourceURI, String focusNode, URI shapeTreeHint, String bodyString, String contentType) throws IOException, URISyntaxException;

    /**
     * Updates an existing resource using a SPARQL Update, validating it against the provided shape tree
     * @param context ShapeTreeContext that would be used for authentication purposes
     * @param resourceURI The URI of the resource being updated
     * @param focusNode The node/subject to use for validation purposes
     * @param shapeTreeHint The shape tree the body should be validated by
     * @param bodyString SPARQL update query that is updating the resource
     * @return ShapeTreeResponse containing status and response headers/attributes
     * @throws IOException IOException
     * @throws URISyntaxException URISyntaxException
     */
    ShapeTreeResponse updateDataInstanceWithPatch(ShapeTreeContext context, URI resourceURI, String focusNode, URI shapeTreeHint, String bodyString) throws IOException, URISyntaxException;

    /**
     * Deletes an existing resource.  Provided as a convenience - no validation is performed
     * @param context ShapeTreeContext that would be used for authentication purposes
     * @param resourceURI The URI of the resource being updated
     * @param shapeTreeURI URI of shape tree associated with resource being deleted
     * @return ShapeTreeResponse containing status and response headers/attributes
     * @throws IOException IOException
     */
    ShapeTreeResponse deleteDataInstance(ShapeTreeContext context, URI resourceURI, URI shapeTreeURI) throws IOException;

    /**
     * Removes a shape tree from a given container
     * @param context ShapeTreeContext that would be used for authentication purposes
     * @param containerURI Container to remove shape tree from
     * @param shapeTreeURI URI of shape tree being removed
     */
    void unplantShapeTree(ShapeTreeContext context, URI containerURI, URI shapeTreeURI);

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
