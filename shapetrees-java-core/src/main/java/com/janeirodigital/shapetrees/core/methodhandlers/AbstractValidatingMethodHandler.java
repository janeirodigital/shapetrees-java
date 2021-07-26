package com.janeirodigital.shapetrees.core.methodhandlers;

import com.janeirodigital.shapetrees.core.*;
import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.core.enums.LinkRelations;
import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.helpers.GraphHelper;
import com.janeirodigital.shapetrees.core.helpers.HttpHeaderHelper;
import com.janeirodigital.shapetrees.core.models.*;
import com.janeirodigital.shapetrees.core.vocabularies.LdpVocabulary;
import lombok.extern.slf4j.Slf4j;
import org.apache.jena.graph.Graph;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.*;

/**
 * Abstract class providing reusable functionality to different method handlers
 */
@Slf4j
public abstract class AbstractValidatingMethodHandler {
    public static final String TEXT_TURTLE = "text/turtle";
    protected final ResourceAccessor resourceAccessor;
    protected final Set<String> supportedRDFContentTypes = Set.of(TEXT_TURTLE, "application/rdf+xml", "application/n-triples", "application/ld+json");
    private static final String REL_TYPE_CONTAINER = "<" + LdpVocabulary.CONTAINER + ">; rel=\"" + LinkRelations.TYPE.getValue() + "\"";

    protected AbstractValidatingMethodHandler(ResourceAccessor resourceAccessor) {
        this.resourceAccessor = resourceAccessor;
    }

    /**
     * Builds a ShapeTreeContext from the incoming request.  Specifically it retrieves
     * the incoming Authorization header and stashes that value for use on any additional requests made during
     * validation.
     * @param shapeTreeRequest Incoming request
     * @return ShapeTreeContext object populated with authentication details, if present
     */
    protected ShapeTreeContext buildContextFromRequest(ShapeTreeRequest<?> shapeTreeRequest) {
        ShapeTreeContext context = new ShapeTreeContext();
        context.setAuthorizationHeaderValue(shapeTreeRequest.getHeaderValue(HttpHeaders.AUTHORIZATION.getValue()));
        return context;
    }

    /**
     * Retrieves a representation of the resource present at the URI of the incoming ShapeTreeRequest
     * @param context ShapeTreeContext used for authentication
     * @param shapeTreeRequest Incoming request to retrieve resource URI from
     * @return ShapeTreeResource representation of that URI
     * @throws ShapeTreeException ShapeTreeException
     */
    protected ShapeTreeResource getRequestResource(ShapeTreeContext context, ShapeTreeRequest<?> shapeTreeRequest) throws ShapeTreeException {
        return this.resourceAccessor.getResource(context, shapeTreeRequest.getURI());
    }

    /**
     * This determines the type of resource being processed.
     *
     * Initial test is based on the incoming request headers, specifically the Content-Type header.
     * If the content type is not one of the accepted RDF types, it will be treated as a NON-RDF source.
     *
     * Then the determination becomes whether or not the resource is a container.
     *
     * If it is a PATCH or PUT and the URI provided already exists, then the existing resource's Link header(s)
     * are used to determine if it is a container or not.
     *
     * If it is a POST or if the resource does not already exist, the incoming request Link header(s) are relied
     * upon.
     *
     * @param shapeTreeRequest The current incoming request
     * @param existingResource The resource located at the incoming request's URI
     * @return ShapeTreeResourceType aligning to current request
     * @throws ShapeTreeException ShapeTreeException throw, specifically if Content-Type is not included on request
     */
    protected ShapeTreeResourceType determineResourceType(ShapeTreeRequest<?> shapeTreeRequest, ShapeTreeResource existingResource) throws ShapeTreeException {
        boolean isNonRdf;
        if (!shapeTreeRequest.getMethod().equals("DELETE")) {
            String incomingRequestContentType = shapeTreeRequest.getContentType();
            // Ensure a content-type is present
            if (incomingRequestContentType == null) {
                throw new ShapeTreeException(400, "Content-Type is required");
            }

            isNonRdf = determineIsNonRdfSource(incomingRequestContentType);

        } else {
            isNonRdf = false;
        }

        if (isNonRdf) {
            return ShapeTreeResourceType.NON_RDF;
        }

        boolean resourceAlreadyExists = existingResource.isExists();
        boolean isContainer = false;
        if ((shapeTreeRequest.getMethod().equals("PUT") || shapeTreeRequest.getMethod().equals("PATCH")) && resourceAlreadyExists) {
            isContainer = existingResource.isContainer();
        } else if (shapeTreeRequest.getLinkHeaders() != null) {
            isContainer = getIsContainerFromIncomingLinkHeaders(shapeTreeRequest);
        }

        return isContainer ? ShapeTreeResourceType.CONTAINER : ShapeTreeResourceType.RESOURCE;
    }

    /**
     * Normalizes the BaseURI to use for a request based on the incoming request.
     * @param uri URI of request
     * @param requestedName Requested name of resource (provided on created resources via POST)
     * @param resourceType Description of resource (Container, NonRDF, Resource)
     * @return BaseURI to use for RDF Graphs
     * @throws URISyntaxException URISyntaxException
     */
    protected URI normalizeBaseURI(URI uri, String requestedName, ShapeTreeResourceType resourceType) throws URISyntaxException {
        String uriString = uri.toString();
        if (requestedName != null) {
            uriString += requestedName;
        }
        if (resourceType == ShapeTreeResourceType.CONTAINER && !uriString.endsWith("/")) {
            uriString += "/";
        }
        return new URI(uriString);
    }

    /**
     * Loads body of request into graph
     * @param shapeTreeRequest Request
     * @param baseURI BaseURI to use for graph
     * @return Graph representation of request body
     * @throws ShapeTreeException ShapeTreeException
     */
    protected Graph getIncomingBodyGraph(ShapeTreeRequest<?> shapeTreeRequest, URI baseURI) throws ShapeTreeException {
        log.debug("Reading request body into graph with baseURI {}", baseURI);

        if (shapeTreeRequest.getResourceType() != ShapeTreeResourceType.NON_RDF &&
                shapeTreeRequest.getBody() != null &&
                shapeTreeRequest.getBody().length() > 0) {
            return GraphHelper.readStringIntoGraph(baseURI, shapeTreeRequest.getBody(), shapeTreeRequest.getContentType());
        }
        return null;
    }

    /**
     * Gets focus node from request header
     * @param shapeTreeRequest Request
     * @param baseURI Base URI for use on relative focus nodes
     * @return URI of focus node
     * @throws IOException IOException
     */
    protected URI getIncomingResolvedFocusNode(ShapeTreeRequest<?> shapeTreeRequest, URI baseURI) throws IOException {
        if (shapeTreeRequest.getLinkHeaders().get(LinkRelations.FOCUS_NODE.getValue()) != null) {
            String focusNode = shapeTreeRequest.getLinkHeaders().get(LinkRelations.FOCUS_NODE.getValue()).get(0);
            return baseURI.resolve(focusNode);
        } else {
            throw new ShapeTreeException(400, "No Link header with relation " + LinkRelations.FOCUS_NODE.getValue() + " supplied, unable to perform Shape validation");
        }
    }

    /**
     * Gets target shape tree / hint from request header
     * @param shapeTreeRequest Request
     * @return URI value of target shape tree
     * @throws URISyntaxException URISyntaxException
     */
    protected URI getIncomingTargetShapeTreeHint(ShapeTreeRequest<?> shapeTreeRequest) throws URISyntaxException {
        if (shapeTreeRequest.getLinkHeaders().get(LinkRelations.TARGET_SHAPETREE.getValue()) != null) {
            return new URI(shapeTreeRequest.getLinkHeaders().get(LinkRelations.TARGET_SHAPETREE.getValue()).get(0));
        }
        return null;
    }

    /**
     * Determines if a resource should be treated as a container based on its request Link headers
     * @param shapeTreeRequest Request
     * @return Is the resource a container?
     */
    protected Boolean getIsContainerFromIncomingLinkHeaders(ShapeTreeRequest<?> shapeTreeRequest) {
        if (shapeTreeRequest.getLinkHeaders() != null && shapeTreeRequest.getLinkHeaders().get(LinkRelations.TYPE.getValue()) != null) {
            return (shapeTreeRequest.getLinkHeaders().get(LinkRelations.TYPE.getValue()).contains(LdpVocabulary.CONTAINER) ||
                    shapeTreeRequest.getLinkHeaders().get(LinkRelations.TYPE.getValue()).contains(LdpVocabulary.BASIC_CONTAINER));
        }
        return false;
    }

    /**
     * Determines whether a content type is a supported RDF type
     * @param incomingRequestContentType Content type to test
     * @return Boolean indicating whether it is RDF or not
     */
    protected boolean determineIsNonRdfSource(String incomingRequestContentType) {
        return !this.supportedRDFContentTypes.contains(incomingRequestContentType.toLowerCase());
    }

    /**
     * Returns parent container URI for a given resource
     * @param shapeTreeResource Resource
     * @return URI to the resource's parent container
     */
    protected URI getParentContainerURI(ShapeTreeResource shapeTreeResource) {
        return shapeTreeResource.getUri().resolve(shapeTreeResource.isContainer() ? ".." : ".");
    }

    /**
     * Returns resource name from a resource URI
     * @param shapeTreeResource Resource
     * @return Resource name
     */
    protected String getRequestResourceName(ShapeTreeResource shapeTreeResource) {
        return shapeTreeResource.getUri().toString().replace(getParentContainerURI(shapeTreeResource).toString(), "");
    }

    /**
     * Returns URI of shape tree auxiliary resource for a given resource
     * @param shapeTreeResource resource
     * @return URI to shape tree auxiliary resource
     * @throws ShapeTreeException ShapeTreeException
     */
    protected URI getShapeTreeMetadataURIForResource(ShapeTreeResource shapeTreeResource) throws ShapeTreeException {
        Map<String, List<String>> linkHeaders = HttpHeaderHelper.parseLinkHeadersToMap(shapeTreeResource.getAttributes().get(HttpHeaders.LINK.getValue()));

        if (!linkHeaders.containsKey(LinkRelations.SHAPETREE_LOCATOR.getValue())) {
            log.error("The resource {} does not contain a link header of {}", shapeTreeResource.getUri(), LinkRelations.SHAPETREE_LOCATOR.getValue());
            throw new ShapeTreeException(500, "No Link header with relation of " + LinkRelations.SHAPETREE_LOCATOR.getValue() + " found");
        }
        String metaDataURIString = linkHeaders.get(LinkRelations.SHAPETREE_LOCATOR.getValue()).stream().findFirst().orElse(null);
        if (metaDataURIString != null && metaDataURIString.startsWith("/")) {
            // If the header value doesn't include scheme/host, prefix it with the scheme & host from container
            URI shapeTreeContainerURI = shapeTreeResource.getUri();
            String portFragment;
            if (shapeTreeContainerURI.getPort() > 0) {
                portFragment = ":" + shapeTreeContainerURI.getPort();
            } else {
                portFragment = "";
            }
            metaDataURIString = shapeTreeContainerURI.getScheme() + "://" + shapeTreeContainerURI.getHost() + portFragment + metaDataURIString;
        }

        if (metaDataURIString == null) {
            throw new ShapeTreeException(500, "No Link header with relation of " + LinkRelations.SHAPETREE_LOCATOR.getValue() + " found");
        }

        return URI.create(metaDataURIString);
    }

    /**
     * Returns shape tree auxiliary resource for a given resource
     * @param shapeTreeResource resource
     * @return shape tree auxiliary resource
     * @throws ShapeTreeException ShapeTreeException
     */
    protected ShapeTreeResource getShapeTreeMetadataResourceForResource(ShapeTreeContext shapeTreeContext, ShapeTreeResource shapeTreeResource) throws ShapeTreeException {
        return this.resourceAccessor.getResource(shapeTreeContext, getShapeTreeMetadataURIForResource(shapeTreeResource));
    }

    /**
     * Returns a graph representation of a resource
     * @param resource Resource to get graph of
     * @param baseURI BaseURI to use for triples
     * @return Graph representation of resource
     * @throws ShapeTreeException ShapeTreeException
     */
    protected Graph getGraphForResource(ShapeTreeResource resource, URI baseURI) throws ShapeTreeException {
        if (!resource.isExists()) return null;

        return GraphHelper.readStringIntoGraph(baseURI, resource.getBody(), resource.getFirstAttributeValue(HttpHeaders.CONTENT_TYPE.getValue()));
    }

    /**
     * Plants a shape tree on an existing resource
     * @param shapeTreeContext
     * @param shapeTreeRequest
     * @param metadataResource
     * @return
     * @throws ShapeTreeException
     */
    protected ShapeTreeValidationResponse plantShapeTree(ShapeTreeContext shapeTreeContext, ShapeTreeRequest shapeTreeRequest, ShapeTreeResource metadataResource) throws IOException, URISyntaxException {

        // Determine the resource directly related to this one
        ShapeTreeResource primaryResource = this.resourceAccessor.getResource(shapeTreeContext, metadataResource.getAssociatedUri());

        if (!getShapeTreeMetadataURIForResource(primaryResource).equals(metadataResource.getUri())) {
            throw new ShapeTreeException(500, "Primary resource is not associated with metadata resource");
        }

        // Extract the ShapeTreeLocator from the ShapeTreeRequest body
        ShapeTreeLocator rootLocator = ShapeTreeLocator.getShapeTreeLocatorFromGraph(shapeTreeRequest.getURI().toString(),
                                                    GraphHelper.readStringIntoGraph(shapeTreeRequest.getURI(),
                                                            shapeTreeRequest.getBody(),
                                                            shapeTreeRequest.getHeaderValue(HttpHeaders.CONTENT_TYPE.getValue())));

        List<ShapeTreeLocation> updatedLocations = null;
        if (metadataResource.isExists()) {
            ShapeTreeLocator existingLocator = ShapeTreeLocator.getShapeTreeLocatorFromGraph(metadataResource.getUri().toString(),
                    GraphHelper.readStringIntoGraph(metadataResource.getUri(),
                            metadataResource.getBody(),
                            metadataResource.getFirstAttributeValue(HttpHeaders.CONTENT_TYPE.getValue())));

            updatedLocations = existingLocator.getUpdatedShapeTreeLocations(rootLocator);
        } else {
            updatedLocations = rootLocator.getLocations();
        }

        // Call shape tree assigned for each associated shape tree location. There must be at least one.
        for (ShapeTreeLocation rootLocation : updatedLocations) {
            return assignShapeTreeToResource(shapeTreeContext,
                    rootLocator,
                    rootLocation,
                    rootLocation,
                    primaryResource);
        }

        throw new ShapeTreeException(500, "Failed to plant ShapeTreeLocator with no ShapeTreeLocations");

    }

    protected ShapeTreeValidationResponse assignShapeTreeToResource(ShapeTreeContext shapeTreeContext,
                                                                    ShapeTreeLocator rootLocator,
                                                                    ShapeTreeLocation rootLocation,
                                                                    ShapeTreeLocation parentLocation,
                                                                    ShapeTreeResource primaryResource) throws IOException, URISyntaxException {

        ShapeTree primaryResourceShapeTree = null;
        ShapeTreeLocator primaryResourceLocator = null;
        ShapeTreeResource primaryMetadataResource = null;
        URI primaryResourceMatchingNode = null;
        ShapeTreeValidationResponse validationResponse = null;

        // 1. Determine the shape tree to manage the target container (first one has to come from the provided location)

        if (atRootOfPlantHierarchy(rootLocation, primaryResource)) {

            // If we are at the root of the plant hierarchy we don't need to validate the primary resource against
            // a shape tree managing a parent container. We only need to validate the primary resource against
            // the shape tree that is being planted at the root to ensure it conforms.

            primaryResourceShapeTree = ShapeTreeFactory.getShapeTree(URI.create(rootLocation.getShapeTree()));
            // Validate the root primary resource conforms with the root shape tree
            ValidationResult validationResult = primaryResourceShapeTree.validateResource(primaryResource);
            // Return failure if validation fails
            if (!validationResult.isValid()) { return new ShapeTreeValidationResponse(validationResult); }
            // If the match with primaryResourceShapeTree included some shape validation, include the matching focus node
            primaryResourceMatchingNode = validationResult.getMatchingFocusNode();
            // If we are at the root of the plant hierarchy, use the root locator from the initial plant request body
            // We do not need to add the location to the locator, because it already exists (from the initial plant)
            primaryResourceLocator = rootLocator;
            // Lookup the associated metadata resource (which may not exist)
            primaryMetadataResource = getShapeTreeMetadataResourceForResource(shapeTreeContext, primaryResource);

        } else {

            // Get the shape tree managing the parent
            ShapeTree parentShapeTree = ShapeTreeFactory.getShapeTree(URI.create(parentLocation.getShapeTree()));
            // Validate proposed primary resource against the shape tree managing its parent container
            ValidationResult validationResult = parentShapeTree.validateContainedResource(primaryResource);
            // If the proposed primary resource won't validate against the parent container shape tree, return failure
            if (!validationResult.isValid()) { return new ShapeTreeValidationResponse(validationResult); }
            // Extract the shape tree from parentShapeTree's st:contains that the primary resource matched with
            primaryResourceShapeTree = validationResult.getMatchingShapeTree();
            // If the match with primaryResourceShapeTree included shape validation, include the matching focus node
            primaryResourceMatchingNode = validationResult.getMatchingFocusNode();

            // Not at the root of the plant hierarchy. Check to see if the primary resource has an existing shape
            // tree locator assigned, or create a new one. Existing locators will be updated with a new location

            // First check for an existing metadata resource
            primaryMetadataResource = getShapeTreeMetadataResourceForResource(shapeTreeContext, primaryResource);

            if (!primaryMetadataResource.isExists()) {
                // If the existing metadata resource doesn't exist make a new shape tree locator
                primaryResourceLocator = new ShapeTreeLocator(primaryMetadataResource.getUri().toString());
            } else {
                // Get the existing shape tree locator from the metadata resource graph
                primaryResourceLocator = ShapeTreeLocator.getShapeTreeLocatorFromGraph(primaryMetadataResource.getUri().toString(),
                        GraphHelper.readStringIntoGraph(primaryMetadataResource.getUri(),
                                primaryMetadataResource.getBody(),
                                primaryMetadataResource.getFirstAttributeValue(HttpHeaders.CONTENT_TYPE.getValue())));
            }

        }

        // Build the primary resource location
        String matchingNode = primaryResourceMatchingNode == null ? null : primaryResourceMatchingNode.toString();
        ShapeTreeLocation primaryResourceLocation = new ShapeTreeLocation(primaryResourceShapeTree.getId(),
                                                                          rootLocation.getRootShapeTree(),
                                                                          rootLocation.getRootShapeTreeInstance(),
                                                                          matchingNode,
                                                                          primaryResourceShapeTree.getShape(),
                                                                          primaryResourceLocator.mintLocation());

        if (!atRootOfPlantHierarchy(rootLocation, primaryResource)) {
            // Add the shape tree location to the shape tree locator for the primary resource
            primaryResourceLocator.addShapeTreeLocation(primaryResourceLocation);
        }

        // If the primary resource is a container, and its shape tree specifies its contents with st:contains
        // Recursively traverse the hierarchy and perform shape tree assignment
        if (primaryResource.isContainer() && primaryResourceShapeTree.getContains() != null && !primaryResourceShapeTree.getContains().isEmpty()) {

            // TODO - Provide a configurable maximum limit on contained resources for a recursive plant, generate ShapeTreeException
            List<ShapeTreeResource> containedResources = this.resourceAccessor.getContainedResources(shapeTreeContext, primaryResource.getUri());
            // If the container is not empty
            if (containedResources != null && !containedResources.isEmpty()) {
                // Sort contained resources so that containers are evaluated first, then resources
                Collections.sort(containedResources, new SortByShapeTreeResourceType());
                // Perform a depth first validation and assignment for each contained resource
                for (ShapeTreeResource containedResource : containedResources) {
                    // Recursively call this function on the contained resource
                    validationResponse = assignShapeTreeToResource(shapeTreeContext, rootLocator, rootLocation, primaryResourceLocation, containedResource);
                    if (!validationResponse.isValidRequest()) { return validationResponse; }
                }
            }
        }

        if (!primaryMetadataResource.isExists()) {
            // create primary metadata resource if it doesn't exist
            Map<String, List<String>> headers = new HashMap<>();
            headers.put(HttpHeaders.CONTENT_TYPE.getValue(), Collections.singletonList("text/turtle"));
            ShapeTreeResource createdMetadataResource = this.resourceAccessor.createResource(shapeTreeContext,
                                                                                             getShapeTreeMetadataURIForResource(primaryResource),
                                                                                             headers,
                                                                                             primaryResourceLocator.getGraph().toString(),
                                                                                             "text/turtle");
        } else {
            // Update the existing metadata resource for the primary resource
            primaryMetadataResource.setBody(primaryResourceLocator.getGraph().toString());
            primaryMetadataResource = this.resourceAccessor.updateResource(shapeTreeContext, primaryMetadataResource);
        }

        return new ShapeTreeValidationResponse(true, true);

    }

    private boolean atRootOfPlantHierarchy(ShapeTreeLocation rootLocation, ShapeTreeResource primaryResource) {
        if (rootLocation.getRootShapeTreeInstance().equals(primaryResource.getUri().toString())) {
            return true;
        }
        return false;
    }

    protected ShapeTreeValidationResponse createShapeTreeInstance(ShapeTreeContext shapeTreeContext, ShapeTreeRequest shapeTreeRequest, String proposedName) throws IOException, URISyntaxException {

        // Lookup the target container where the resource will be created
        ShapeTreeResource targetContainer = getRequestResource(shapeTreeContext, shapeTreeRequest);
        ensureRequestResourceExists(targetContainer,"Target container for POST not found");
        ensureRequestResourceIsNotMetadata(targetContainer,"Cannot create a shape tree instance in a metadata resource");

        // Check the resource type, and error if it isn't a container
        shapeTreeRequest.setResourceType(determineResourceType(shapeTreeRequest, targetContainer));
        ensureRequestResourceIsContainer(targetContainer,"Cannot create a shape tree instance in a non-container resource");

        // Prepare the target resource for validation and creation
        URI targetResourceURI = normalizeBaseURI(targetContainer.getUri(), proposedName, shapeTreeRequest.getResourceType());
        ensureTargetResourceDoesNotExist(shapeTreeContext, targetResourceURI,"Cannot create a shape tree instance in a non-container resource");

        ShapeTreeResource containerMetadataResource = getShapeTreeMetadataResourceForResource(shapeTreeContext, targetContainer);
        ensureRequestResourceExists(containerMetadataResource, "Should not be creating a shape tree instance on an unmanaged target container");

        ShapeTreeLocator containerLocator = ShapeTreeLocator.getShapeTreeLocatorFromGraph(containerMetadataResource.getUri().toString(),
                                GraphHelper.readStringIntoGraph(containerMetadataResource.getUri(),
                                containerMetadataResource.getBody(),
                                containerMetadataResource.getFirstAttributeValue(HttpHeaders.CONTENT_TYPE.getValue())));

        ensureShapeTreeLocatorExists(containerLocator, "Cannot have a shape tree metadata resource without a shape tree locator with at least one shape tree location");

        // Get the shape tree associated that specifies what resources can be contained by the target container (st:contains)
        ShapeTreeLocation containingLocation = containerLocator.getContainingShapeTreeLocation();

        if (containingLocation == null) {
            // If there is no containing shape tree for the target container, then the request is valid and can
            // be passed straight through
            return ShapeTreeValidationResponse.passThroughResponse();
        }

        URI containerShapeTreeURI = URI.create(containingLocation.getShapeTree());
        ShapeTree containerShapeTree = ShapeTreeFactory.getShapeTree(containerShapeTreeURI);

        ValidationResult validationResult = containerShapeTree.validateContainedResource(proposedName, shapeTreeRequest.getResourceType(), getIncomingTargetShapeTreeHint(shapeTreeRequest), getIncomingBodyGraph(shapeTreeRequest, targetResourceURI), getIncomingResolvedFocusNode(shapeTreeRequest, targetResourceURI).toString());

        if (!validationResult.isValid()) {
            return new ShapeTreeValidationResponse(validationResult);
        }

        // get the root location from the containingLocation

        // assignShapeTreeToResource(shapeTreeContext, rootLocation, parentLocation(containing?), primaryResource)

        // Validation was successful, assign a locator to the newly created resource

        // Validate the proposed resource against the parent
        // If it is allowed
        //   1. create the resource
        //   2. assign the matched shape tree to it
        //   3. Provide a response

        log.debug("Creating shape tree instance at {}", targetResourceURI.toString());

        return new ShapeTreeValidationResponse();

    }

    protected ShapeTreeValidationResponse updateShapeTreeInstance(ShapeTreeContext shapeTreeContext, ShapeTreeRequest shapeTreeRequest) {

        ShapeTreeValidationResponse validationResponse = new ShapeTreeValidationResponse();

        return validationResponse;

    }

    protected ShapeTreeValidationResponse deleteShapeTreeInstance() {

        ShapeTreeValidationResponse validationResponse = new ShapeTreeValidationResponse();

        return validationResponse;

    }

    private void ensureRequestResourceIsNotMetadata(ShapeTreeResource shapeTreeResource, String message) throws ShapeTreeException {
        if (shapeTreeResource.isMetadata()) {
            throw new ShapeTreeException(400, message);
        }
    }

    private void ensureRequestResourceExists(ShapeTreeResource shapeTreeResource, String message) throws ShapeTreeException {
        if (!shapeTreeResource.isExists()) {
            throw new ShapeTreeException(404, message);
        }
    }

    private void ensureRequestResourceIsContainer(ShapeTreeResource shapeTreeResource, String message) throws ShapeTreeException {
        if (!shapeTreeResource.isContainer()) {
            throw new ShapeTreeException(400, message);
        }
    }

    private void ensureTargetResourceDoesNotExist(ShapeTreeContext shapeTreeContext, URI targetResourceURI, String message) throws ShapeTreeException {
        ShapeTreeResource targetResource = this.resourceAccessor.getResource(shapeTreeContext, targetResourceURI);
        if (targetResource.isExists()) {
            throw new ShapeTreeException(409, message);
        }
    }

    private void ensureShapeTreeLocatorExists(ShapeTreeLocator locator, String message) throws ShapeTreeException {
        if (locator == null || locator.getLocations() == null || locator.getLocations().isEmpty()) {
            throw new ShapeTreeException(400, message);
        }
    }
}



class SortByShapeTreeResourceType implements Comparator<ShapeTreeResource>
{

    // Used for sorting by shape tree resource type with the following order
    // 1. Containers
    // 2. Resources
    // 3. Non-RDF Resources

    public int compare (ShapeTreeResource a, ShapeTreeResource b) {
        return a.getType().compareTo(b.getType());
    }

}