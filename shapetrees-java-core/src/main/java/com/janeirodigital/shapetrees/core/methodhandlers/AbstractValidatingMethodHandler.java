package com.janeirodigital.shapetrees.core.methodhandlers;

import com.janeirodigital.shapetrees.core.*;
import com.janeirodigital.shapetrees.core.enums.LinkRelations;
import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.helpers.GraphHelper;
import com.janeirodigital.shapetrees.core.models.*;
import com.janeirodigital.shapetrees.core.vocabularies.LdpVocabulary;
import lombok.extern.slf4j.Slf4j;
import org.apache.jena.graph.Graph;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.update.UpdateAction;
import org.apache.jena.update.UpdateFactory;
import org.apache.jena.update.UpdateRequest;

import java.io.IOException;
import java.io.Serializable;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.*;

/**
 * Abstract class providing reusable functionality to different method handlers
 */
@Slf4j
public abstract class AbstractValidatingMethodHandler {
    public static final String TEXT_TURTLE = "text/turtle";
    private static final String POST = "POST";
    private static final String PUT = "PUT";
    private static final String PATCH = "PATCH";
    private static final String DELETE = "DELETE";
    protected final ResourceAccessor resourceAccessor;
    protected final Set<String> supportedRDFContentTypes = Set.of(TEXT_TURTLE, "application/rdf+xml", "application/n-triples", "application/ld+json");
    protected final Set<String> supportedSPARQLContentTypes = Set.of("application/sparql-update");

    protected AbstractValidatingMethodHandler(ResourceAccessor resourceAccessor) {
        this.resourceAccessor = resourceAccessor;
    }

    protected ShapeTreeValidationResponse manageShapeTree(ShapeTreeContext shapeTreeContext, ShapeTreeRequest shapeTreeRequest, ShapeTreeResource metadataResource) throws IOException, URISyntaxException {

        ShapeTreeValidationResponse validationResponse = null;
        ShapeTreeLocator updatedRootLocator = getShapeTreeLocatorFromRequest(shapeTreeRequest, metadataResource);
        ShapeTreeLocator existingRootLocator = getShapeTreeLocatorFromResource(metadataResource);
        ShapeTreeResource primaryResource = getResourceForShapeTreeMetadataResource(shapeTreeContext, metadataResource);

        // Determine ShapeTreeLocations that have been removed, added, and/or updated
        ShapeTreeLocatorDelta delta = ShapeTreeLocatorDelta.evaluate(existingRootLocator, updatedRootLocator);

        // It is invalid for a locator resource to be left with no locations.
        // Shape Trees, §3: A shape tree locator includes one or more shape tree locations via st:location.
        if (delta.allRemoved()) { ensureAllRemovedFromLocatorByDelete(shapeTreeRequest); }

        if (delta.wasReduced()) {
            // An existing location has been removed from the locator for the primary resource.
            validationResponse = unplantShapeTree(shapeTreeContext, delta, primaryResource);
            if (!validationResponse.isValidRequest()) { return validationResponse; }
        }

        if (delta.isUpdated()) {
            // An existing location has been updated, or new locations have been added
            validationResponse = plantShapeTree(shapeTreeContext, updatedRootLocator, delta, primaryResource);
            if (!validationResponse.isValidRequest()) { return validationResponse; }
        }

        return new ShapeTreeValidationResponse(true, true);

    }

    /**
     * Plants a shape tree on an existing resource
     * @param shapeTreeContext
     * @param updatedRootLocator
     * @param delta
     * @param primaryResource
     * @return ShapeTreeValidationResponse
     * @throws IOException
     * @throws URISyntaxException
     */
    protected ShapeTreeValidationResponse plantShapeTree(ShapeTreeContext shapeTreeContext, ShapeTreeLocator updatedRootLocator, ShapeTreeLocatorDelta delta, ShapeTreeResource primaryResource) throws IOException, URISyntaxException {

        // Cannot directly update locations that are not root locations
        ensureUpdatedLocationsAreRootLocations(delta);

        // Run recursive assignment for each updated location in the root locator
        for (ShapeTreeLocation rootLocation : delta.getUpdatedLocations()) {
            ShapeTreeValidationResponse validationResponse = assignShapeTreeToResource(shapeTreeContext, updatedRootLocator, rootLocation, rootLocation, primaryResource, null);
            if (!validationResponse.isValidRequest()) { return validationResponse; }
        }

        return new ShapeTreeValidationResponse(true, true);

    }

    protected  ShapeTreeValidationResponse unplantShapeTree(ShapeTreeContext shapeTreeContext, ShapeTreeLocatorDelta delta, ShapeTreeResource primaryResource) throws ShapeTreeException, URISyntaxException {

        ensureRemovedLocationsAreRootLocations(delta); // Cannot unplant a non-root location

        // Run recursive unassignment for each removed location in the updated root locator
        for (ShapeTreeLocation rootLocation : delta.getRemovedLocations()) {
            ShapeTreeValidationResponse validationResponse = unassignShapeTreeFromResource(shapeTreeContext, rootLocation, primaryResource);
            if (!validationResponse.isValidRequest()) { return validationResponse; }
        }

        return new ShapeTreeValidationResponse(true, true);

    }

    protected ShapeTreeValidationResponse createShapeTreeInstance(ShapeTreeContext shapeTreeContext, ShapeTreeRequest shapeTreeRequest, String proposedName) throws IOException, URISyntaxException {

        ShapeTreeValidationResponse validationResponse = null;

        shapeTreeRequest.setResourceType(determineResourceType(shapeTreeRequest, null));

        // Lookup the target container where the resource will be created
        ShapeTreeResource targetContainer = getRequestTargetContainer(shapeTreeContext, shapeTreeRequest);

        ensureShapeTreeResourceExists(targetContainer,"Target container for resource creation not found");
        ensureRequestResourceIsNotMetadata(targetContainer,"Cannot create a shape tree instance in a metadata resource");
        ensureRequestResourceIsContainer(targetContainer,"Cannot create a shape tree instance in a non-container resource");

        // Prepare the target resource for validation and creation
        URI targetResourceURI = normalizeBaseURI(targetContainer.getUri(), proposedName, shapeTreeRequest.getResourceType());
        ensureTargetResourceDoesNotExist(shapeTreeContext, targetResourceURI,"Cannot create a shape tree instance in a non-container resource");

        ShapeTreeResource containerMetadataResource = getShapeTreeMetadataResourceForResource(shapeTreeContext, targetContainer);
        ensureShapeTreeResourceExists(containerMetadataResource, "Should not be creating a shape tree instance on an unmanaged target container");

        ShapeTreeLocator containerLocator = getShapeTreeLocatorFromResource(containerMetadataResource);
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

        URI targetShapeTree = getIncomingTargetShapeTreeHint(shapeTreeRequest);
        URI incomingFocusNode = getIncomingResolvedFocusNode(shapeTreeRequest, targetResourceURI);
        Graph incomingBodyGraph = getIncomingBodyGraph(shapeTreeRequest, targetResourceURI, null);

        ValidationResult validationResult = containerShapeTree.validateContainedResource(proposedName, shapeTreeRequest.getResourceType(), targetShapeTree, incomingBodyGraph, incomingFocusNode);

        if (Boolean.FALSE.equals(validationResult.isValid())) {
            return new ShapeTreeValidationResponse(validationResult);
        }

        log.debug("Creating shape tree instance at {}", targetResourceURI);

        ShapeTreeResource createdResource = this.resourceAccessor.createResource(shapeTreeContext, shapeTreeRequest.getMethod(), targetResourceURI, shapeTreeRequest.getHeaders(), shapeTreeRequest.getBody(), shapeTreeRequest.getContentType());

        ShapeTreeLocation rootShapeTreeLocation = getRootShapeTreeLocation(shapeTreeContext, containingLocation);
        ensureShapeTreeLocationExists(rootShapeTreeLocation, "Unable to find root shape tree location at " + containingLocation.getRootShapeTreeLocation());

        log.debug("Assigning shape tree to created resource: {}", createdResource.getUri());
        // Note: By providing the positive advance validationResult, we let the assignment operation know that validation
        // has already been performed with a positive result, and avoid having it perform the validation a second time
        validationResponse = assignShapeTreeToResource(shapeTreeContext, null, rootShapeTreeLocation, containingLocation, createdResource, validationResult);

        return validationResponse;

    }

    protected ShapeTreeValidationResponse updateShapeTreeInstance(ShapeTreeContext shapeTreeContext, ShapeTreeRequest shapeTreeRequest) throws IOException, URISyntaxException {


        ShapeTreeResource targetResource = getRequestResource(shapeTreeContext, shapeTreeRequest);

        shapeTreeRequest.setResourceType(determineResourceType(shapeTreeRequest, targetResource));

        ensureShapeTreeResourceExists(targetResource,"Target resource to update not found");
        ensureRequestResourceIsNotMetadata(targetResource,"Cannot update a metadata resource as a shape tree instance");

        ShapeTreeResource metadataResource = getShapeTreeMetadataResourceForResource(shapeTreeContext, targetResource);
        ensureShapeTreeResourceExists(metadataResource, "Should not be updating an unmanaged resource as a shape tree instance");

        ShapeTreeLocator locator = getShapeTreeLocatorFromResource(metadataResource);
        ensureShapeTreeLocatorExists(locator, "Cannot have a shape tree metadata resource without a shape tree locator with at least one shape tree location");

        for (ShapeTreeLocation location : locator.getLocations()) {

            // Evaluate the update against each ShapeTreeLocation managing the resource.
            // All must pass for the update to validate
            ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(URI.create(location.getShapeTree()));
            ValidationResult validationResult = shapeTree.validateResource(null, shapeTreeRequest.getResourceType(), getIncomingBodyGraph(shapeTreeRequest, targetResource.getUri(), targetResource), getIncomingResolvedFocusNode(shapeTreeRequest, targetResource.getUri()));
            if (Boolean.FALSE.equals(validationResult.isValid())) { return new ShapeTreeValidationResponse(validationResult); }

        }

        // No issues with validation, so the request is passed along
        return ShapeTreeValidationResponse.passThroughResponse();

    }

    protected ShapeTreeValidationResponse deleteShapeTreeInstance() {
        // Nothing to validate in a delete request, so the request is passed along
        return ShapeTreeValidationResponse.passThroughResponse();
    }

    protected ShapeTreeValidationResponse assignShapeTreeToResource(ShapeTreeContext shapeTreeContext,
                                                                    ShapeTreeLocator rootLocator,
                                                                    ShapeTreeLocation rootLocation,
                                                                    ShapeTreeLocation parentLocation,
                                                                    ShapeTreeResource primaryResource,
                                                                    ValidationResult advanceValidationResult)
            throws IOException, URISyntaxException {

        ShapeTree primaryResourceShapeTree = null;
        ShapeTreeLocator primaryResourceLocator = null;
        URI primaryResourceMatchingNode = null;
        ShapeTreeLocation primaryResourceLocation = null;
        ShapeTreeValidationResponse validationResponse = null;
        ShapeTreeResource primaryMetadataResource = getShapeTreeMetadataResourceForResource(shapeTreeContext, primaryResource);

        ensureValidationResultIsUsableForAssignment(advanceValidationResult, "Invalid advance validation result provided for resource assignment");
        if (advanceValidationResult != null) { primaryResourceShapeTree = advanceValidationResult.getMatchingShapeTree(); }
        if (advanceValidationResult != null) { primaryResourceMatchingNode = advanceValidationResult.getMatchingFocusNode(); }

        if (atRootOfPlantHierarchy(rootLocation, primaryResource)) {

            // If we are at the root of the plant hierarchy we don't need to validate the primary resource against
            // a shape tree managing a parent container. We only need to validate the primary resource against
            // the shape tree that is being planted at the root to ensure it conforms.
            primaryResourceShapeTree = ShapeTreeFactory.getShapeTree(URI.create(rootLocation.getShapeTree()));
            if (advanceValidationResult == null) {    // If this validation wasn't performed in advance
                ValidationResult validationResult = primaryResourceShapeTree.validateResource(primaryResource);
                if (Boolean.FALSE.equals(validationResult.isValid())) { return new ShapeTreeValidationResponse(validationResult); }
                primaryResourceMatchingNode = validationResult.getMatchingFocusNode();
            }

        } else {

            // Not at the root of the plant hierarchy. Validate proposed resource against the shape tree
            // managing the parent container, then extract the matching shape tree and focus node on success
            if (advanceValidationResult == null) {    // If this validation wasn't performed in advance
                ShapeTree parentShapeTree = ShapeTreeFactory.getShapeTree(URI.create(parentLocation.getShapeTree()));
                ValidationResult validationResult = parentShapeTree.validateContainedResource(primaryResource);
                if (Boolean.FALSE.equals(validationResult.isValid())) { return new ShapeTreeValidationResponse(validationResult); }
                primaryResourceShapeTree = validationResult.getMatchingShapeTree();
                primaryResourceMatchingNode = validationResult.getMatchingFocusNode();
            }

        }

        primaryResourceLocator = getPrimaryResourceLocatorForAssignment(primaryResource, primaryMetadataResource, rootLocator, rootLocation);
        primaryResourceLocation = getPrimaryResourceLocationForAssignment(primaryResource, primaryResourceLocator, rootLocation, primaryResourceShapeTree, primaryResourceMatchingNode);

        // If the primary resource is a container, and its shape tree specifies its contents with st:contains
        // Recursively traverse the hierarchy and perform shape tree assignment
        if (primaryResource.isContainer() && primaryResourceShapeTree.getContains() != null && !primaryResourceShapeTree.getContains().isEmpty()) {

            // If the container is not empty, perform a recursive, depth first validation and assignment for each
            // contained resource by recursively calling this method (assignShapeTreeToResource)
            // TODO - Provide a configurable maximum limit on contained resources for a recursive plant, generate ShapeTreeException
            List<ShapeTreeResource> containedResources = this.resourceAccessor.getContainedResources(shapeTreeContext, primaryResource.getUri());
            if (!containedResources.isEmpty()) {
                Collections.sort(containedResources, new SortByShapeTreeResourceType());  // Evaluate containers, then resources
                for (ShapeTreeResource containedResource : containedResources) {
                    validationResponse = assignShapeTreeToResource(shapeTreeContext, null, rootLocation, primaryResourceLocation, containedResource, null);
                    if (!validationResponse.isValidRequest()) { return validationResponse; }
                }
            }
        }

        createOrUpdateMetadataResource(shapeTreeContext, primaryMetadataResource, primaryResource, primaryResourceLocator);

        return new ShapeTreeValidationResponse(true, true);

    }

    protected ShapeTreeValidationResponse unassignShapeTreeFromResource(ShapeTreeContext shapeTreeContext,
                                                                        ShapeTreeLocation rootLocation,
                                                                        ShapeTreeResource primaryResource) throws ShapeTreeException, URISyntaxException {


        ensureShapeTreeResourceExists(primaryResource, "Cannot unassign location from non-existent primary resource");
        ShapeTreeResource primaryMetadataResource = getShapeTreeMetadataResourceForResource(shapeTreeContext, primaryResource);
        ensureShapeTreeResourceExists(primaryMetadataResource, "Cannot unassign location from non-existent metadata resource");

        ShapeTreeLocator primaryResourceLocator = getShapeTreeLocatorFromResource(primaryMetadataResource);
        ShapeTreeLocation removeLocation = getShapeTreeLocationForRoot(primaryResourceLocator, rootLocation);
        ShapeTree primaryResourceShapeTree = ShapeTreeFactory.getShapeTree(URI.create(removeLocation.getShapeTree()));

        ShapeTreeValidationResponse validationResponse = null;

        // If the primary resource is a container, and its shape tree specifies its contents with st:contains
        // Recursively traverse the hierarchy and perform shape tree unassignment
        if (primaryResource.isContainer() && primaryResourceShapeTree.getContains() != null && !primaryResourceShapeTree.getContains().isEmpty()) {

            // TODO - Should there also be a configurable maximum limit on unplanting?
            List<ShapeTreeResource> containedResources = this.resourceAccessor.getContainedResources(shapeTreeContext, primaryResource.getUri());
            // If the container is not empty
            if (!containedResources.isEmpty()) {
                // Sort contained resources so that containers are evaluated first, then resources
                Collections.sort(containedResources, new SortByShapeTreeResourceType());
                // Perform a depth first unassignment for each contained resource
                for (ShapeTreeResource containedResource : containedResources) {
                    // Recursively call this function on the contained resource
                    validationResponse = unassignShapeTreeFromResource(shapeTreeContext, rootLocation, containedResource);
                    if (!validationResponse.isValidRequest()) { return validationResponse; }
                }
            }
        }

        primaryResourceLocator.removeShapeTreeLocation(removeLocation);

        deleteOrUpdateMetadataResource(shapeTreeContext, primaryMetadataResource, primaryResourceLocator);

        return new ShapeTreeValidationResponse(true, true);

    }

    /**
     * Builds a ShapeTreeContext from the incoming request.  Specifically it retrieves
     * the incoming Authorization header and stashes that value for use on any additional requests made during
     * validation.
     * @param shapeTreeRequest Incoming request
     * @return ShapeTreeContext object populated with authentication details, if present
     */
    protected ShapeTreeContext buildContextFromRequest(ShapeTreeRequest shapeTreeRequest) {
        ShapeTreeContext context = new ShapeTreeContext();
        context.setAuthorizationHeaderValue(shapeTreeRequest.getHeaderValue(com.janeirodigital.shapetrees.core.enums.HttpHeaders.AUTHORIZATION.getValue()));
        return context;
    }

    /**
     * Retrieves a representation of the resource present at the URI of the incoming ShapeTreeRequest
     * @param context ShapeTreeContext used for authentication
     * @param shapeTreeRequest Incoming request to retrieve resource URI from
     * @return ShapeTreeResource representation of that URI
     * @throws ShapeTreeException ShapeTreeException
     */
    protected ShapeTreeResource getRequestResource(ShapeTreeContext context, ShapeTreeRequest shapeTreeRequest) throws ShapeTreeException {
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
    protected ShapeTreeResourceType determineResourceType(ShapeTreeRequest shapeTreeRequest, ShapeTreeResource existingResource) throws ShapeTreeException {
        boolean isNonRdf;
        if (!shapeTreeRequest.getMethod().equals(DELETE)) {
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

        boolean isContainer = false;
        boolean resourceAlreadyExists = false;
        if (existingResource != null) {
            resourceAlreadyExists = existingResource.isExists();
        }
        if ((shapeTreeRequest.getMethod().equals(PUT) || shapeTreeRequest.getMethod().equals(PATCH)) && resourceAlreadyExists) {
            isContainer = existingResource.isContainer();
        } else if (shapeTreeRequest.getLinkHeaders() != null) {
            isContainer = getIsContainerFromRequest(shapeTreeRequest);
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
    protected Graph getIncomingBodyGraph(ShapeTreeRequest shapeTreeRequest, URI baseURI, ShapeTreeResource targetResource) throws ShapeTreeException {
        log.debug("Reading request body into graph with baseURI {}", baseURI);

        if ((shapeTreeRequest.getResourceType() == ShapeTreeResourceType.NON_RDF
                && !shapeTreeRequest.getContentType().equalsIgnoreCase("application/sparql-update"))
                || shapeTreeRequest.getBody() == null
                || shapeTreeRequest.getBody().length() == 0) {
            return null;
        }

        Graph targetResourceGraph = null;

        if (shapeTreeRequest.getMethod().equals(PATCH)) {

            // In the event of a SPARQL PATCH, we get the SPARQL query and evaluate it, passing the
            // resultant graph back to the caller

            if (targetResource != null) {
                targetResourceGraph = getGraphForResource(targetResource, baseURI);
            }

            if (targetResourceGraph == null) {   // if the target resource doesn't exist or has no content
                log.debug("Existing target resource graph to patch does not exist.  Creating an empty graph.");
                targetResourceGraph = ModelFactory.createDefaultModel().getGraph();
            }

            // Perform a SPARQL update locally to ensure that resulting graph validates against ShapeTree
            UpdateRequest updateRequest = UpdateFactory.create(shapeTreeRequest.getBody(), baseURI.toString());
            UpdateAction.execute(updateRequest, targetResourceGraph);

            if (targetResourceGraph == null) {
                throw new ShapeTreeException(400, "No graph after update");
            }

        } else {
            targetResourceGraph = GraphHelper.readStringIntoGraph(baseURI, shapeTreeRequest.getBody(), shapeTreeRequest.getContentType());
        }

        return targetResourceGraph;
    }

    /**
     * Gets focus node from request header
     * @param shapeTreeRequest Request
     * @param baseURI Base URI for use on relative focus nodes
     * @return URI of focus node
     * @throws IOException IOException
     */
    protected URI getIncomingResolvedFocusNode(ShapeTreeRequest shapeTreeRequest, URI baseURI) throws IOException {
        if (shapeTreeRequest.getLinkHeaders().get(LinkRelations.FOCUS_NODE.getValue()) != null) {
            String focusNode = shapeTreeRequest.getLinkHeaders().get(LinkRelations.FOCUS_NODE.getValue()).get(0);
            return baseURI.resolve(focusNode);
        }
        return null;
    }

    /**
     * Gets target shape tree / hint from request header
     * @param shapeTreeRequest Request
     * @return URI value of target shape tree
     * @throws URISyntaxException URISyntaxException
     */
    protected URI getIncomingTargetShapeTreeHint(ShapeTreeRequest shapeTreeRequest) throws URISyntaxException {
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
    protected Boolean getIsContainerFromRequest(ShapeTreeRequest shapeTreeRequest) {
        // First try to determine based on link headers
        if (shapeTreeRequest.getLinkHeaders() != null && shapeTreeRequest.getLinkHeaders().get(LinkRelations.TYPE.getValue()) != null) {
            return (shapeTreeRequest.getLinkHeaders().get(LinkRelations.TYPE.getValue()).contains(LdpVocabulary.CONTAINER) ||
                    shapeTreeRequest.getLinkHeaders().get(LinkRelations.TYPE.getValue()).contains(LdpVocabulary.BASIC_CONTAINER));
        }
        // As a secondary attempt, use slash path semantics
        return shapeTreeRequest.getURI().getPath().endsWith("/");
    }

    /**
     * Determines whether a content type is a supported RDF type
     * @param incomingRequestContentType Content type to test
     * @return Boolean indicating whether it is RDF or not
     */
    protected boolean determineIsNonRdfSource(String incomingRequestContentType) {
        return (!this.supportedRDFContentTypes.contains(incomingRequestContentType.toLowerCase()) &&
                !this.supportedSPARQLContentTypes.contains(incomingRequestContentType.toLowerCase()));
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

        String resourceName = shapeTreeResource.getUri().toString().replace(getParentContainerURI(shapeTreeResource).toString(), "");

        if (resourceName.equals("/")) { return "/"; }

        // if this is a container, trim the trailing slash
        if (resourceName.endsWith("/")) {
            resourceName = resourceName.substring(0, resourceName.length() - 1);
        }
        return resourceName;

    }

    /**
     * Returns URI of shape tree auxiliary resource for a given resource
     * @param shapeTreeResource resource
     * @return URI to shape tree auxiliary resource
     * @throws ShapeTreeException ShapeTreeException
     */
    protected URI getShapeTreeMetadataURIForResource(ShapeTreeResource shapeTreeResource) throws ShapeTreeException {

        ensureShapeTreeResourceHasLinkHeaders(shapeTreeResource);

        HttpHeaders linkHeaders = HttpHeaders.parseLinkHeaders(shapeTreeResource.getAttributes().get(com.janeirodigital.shapetrees.core.enums.HttpHeaders.LINK.getValue()));

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
        return GraphHelper.readStringIntoGraph(baseURI, resource.getBody(), resource.getFirstAttributeValue(com.janeirodigital.shapetrees.core.enums.HttpHeaders.CONTENT_TYPE.getValue()));
    }

    protected ShapeTreeLocator getShapeTreeLocatorFromRequest(ShapeTreeRequest shapeTreeRequest, ShapeTreeResource metadataResource) throws URISyntaxException, ShapeTreeException {

        Graph incomingBodyGraph = getIncomingBodyGraph(shapeTreeRequest, normalizeBaseURI(shapeTreeRequest.getURI(), null, ShapeTreeResourceType.RESOURCE), metadataResource);
        if (incomingBodyGraph == null) { return null; }
        return ShapeTreeLocator.getShapeTreeLocatorFromGraph(shapeTreeRequest.getURI().toString(), incomingBodyGraph);
    }

    protected ShapeTreeLocator getShapeTreeLocatorFromResource(ShapeTreeResource metadataResource) throws URISyntaxException, ShapeTreeException {

        if (!metadataResource.isExists()) { return null; }
        Graph metadataResourceGraph = getGraphForResource(metadataResource, normalizeBaseURI(metadataResource.getUri(), null, metadataResource.getType()));
        if (metadataResourceGraph == null) { return null; }
        return ShapeTreeLocator.getShapeTreeLocatorFromGraph(metadataResource.getUri().toString(), metadataResourceGraph);

    }

    // Given a root location, lookup the corresponding location in a shape tree locator that has the same root location
    protected ShapeTreeLocation getShapeTreeLocationForRoot(ShapeTreeLocator locator, ShapeTreeLocation rootLocation) {

        if (locator.getLocations() == null || locator.getLocations().isEmpty()) { return null; }

        for (ShapeTreeLocation location : locator.getLocations()) {
            if (rootLocation.getUri().equals(location.getRootShapeTreeLocation())) {
                return location;
            }
        }
        return null;
    }

    protected ShapeTreeResource getResourceForShapeTreeMetadataResource(ShapeTreeContext shapeTreeContext, ShapeTreeResource metadataResource) throws ShapeTreeException {

        ShapeTreeResource primaryResource = this.resourceAccessor.getResource(shapeTreeContext, metadataResource.getAssociatedUri());
        ensureResourceIsAssociatedWithMetadataResource(primaryResource, metadataResource, "Primary resource is not associated with metadata resource");
        return primaryResource;

    }



    private void createOrUpdateMetadataResource(ShapeTreeContext shapeTreeContext,
                                        ShapeTreeResource primaryMetadataResource,
                                        ShapeTreeResource primaryResource,
                                        ShapeTreeLocator primaryResourceLocator) throws ShapeTreeException, URISyntaxException {

        if (!primaryMetadataResource.isExists()) {
            // create primary metadata resource if it doesn't exist
            HttpHeaders headers = new HttpHeaders();
            headers.setAll(com.janeirodigital.shapetrees.core.enums.HttpHeaders.CONTENT_TYPE.getValue(), Collections.singletonList(TEXT_TURTLE));
            this.resourceAccessor.createResource(shapeTreeContext,"POST", getShapeTreeMetadataURIForResource(primaryResource), headers, primaryResourceLocator.getGraph().toString(), TEXT_TURTLE);
        } else {
            // Update the existing metadata resource for the primary resource
            primaryMetadataResource.setBody(primaryResourceLocator.getGraph().toString());
            this.resourceAccessor.updateResource(shapeTreeContext, "PUT", primaryMetadataResource);
        }

    }

    private void deleteOrUpdateMetadataResource(ShapeTreeContext shapeTreeContext,
                                                ShapeTreeResource primaryMetadataResource,
                                                ShapeTreeLocator primaryResourceLocator) throws ShapeTreeException, URISyntaxException {

        if (primaryResourceLocator.getLocations().isEmpty()) {
            ShapeTreeResponse response = this.resourceAccessor.deleteResource(shapeTreeContext, primaryMetadataResource);
            ensureDeleteIsSuccessful(response);
        } else {
            // Update the existing metadata resource for the primary resource
            primaryMetadataResource.setBody(primaryResourceLocator.getGraph().toString());
            this.resourceAccessor.updateResource(shapeTreeContext, "PUT", primaryMetadataResource);
        }

    }

    private ShapeTreeLocator getPrimaryResourceLocatorForAssignment(ShapeTreeResource primaryResource,
                                                                    ShapeTreeResource primaryMetadataResource,
                                                                    ShapeTreeLocator rootLocator,
                                                                    ShapeTreeLocation rootLocation) throws ShapeTreeException {

        ShapeTreeLocator primaryResourceLocator = null;

        // When at the top of the plant hierarchy, use the root locator from the initial plant request body
        if (atRootOfPlantHierarchy(rootLocation, primaryResource)) { return rootLocator; }

        if (!primaryMetadataResource.isExists()) {
            // If the existing metadata resource doesn't exist make a new shape tree locator
            primaryResourceLocator = new ShapeTreeLocator(primaryMetadataResource.getUri().toString());
        } else {
            // Get the existing shape tree locator from the metadata resource graph
            Graph primaryMetadataGraph = getGraphForResource(primaryMetadataResource, primaryMetadataResource.getUri());
            primaryResourceLocator = ShapeTreeLocator.getShapeTreeLocatorFromGraph(primaryMetadataResource.getUri().toString(), primaryMetadataGraph);
        }

        return primaryResourceLocator;

    }

    private ShapeTreeLocation getPrimaryResourceLocationForAssignment(ShapeTreeResource primaryResource,
                                                                      ShapeTreeLocator primaryResourceLocator,
                                                                      ShapeTreeLocation rootLocation,
                                                                      ShapeTree primaryResourceShapeTree,
                                                                      URI primaryResourceMatchingNode) throws URISyntaxException, ShapeTreeException {

        URI primaryResourceLocationUri = null;

        if (!atRootOfPlantHierarchy(rootLocation, primaryResource)) {
            // Mint a new location URI, since it wouldn't have been passed in the initial request body
            primaryResourceLocationUri = primaryResourceLocator.mintLocation();
        }

        // Build the primary resource location
        String matchingNode = primaryResourceMatchingNode == null ? null : primaryResourceMatchingNode.toString();
        ShapeTreeLocation primaryResourceLocation = new ShapeTreeLocation(primaryResourceShapeTree.getId(),
                primaryResource.getUri().toString(),
                rootLocation.getUri(),
                matchingNode,
                primaryResourceShapeTree.getShape(),
                primaryResourceLocationUri);

        if (!atRootOfPlantHierarchy(rootLocation, primaryResource)) {
            // Add the shape tree location to the shape tree locator for the primary resource
            primaryResourceLocator.addShapeTreeLocation(primaryResourceLocation);
        }

        return primaryResourceLocation;

    }

    private boolean atRootOfPlantHierarchy(ShapeTreeLocation rootLocation, ShapeTreeResource primaryResource) {
        return rootLocation.getManagedResource().equals(primaryResource.getUri().toString());
    }

    // Return a root shape tree locator associated with a given shape tree location
    private ShapeTreeLocator getRootShapeTreeLocator(ShapeTreeContext shapeTreeContext, ShapeTreeLocation location) throws URISyntaxException, ShapeTreeException {

        URI rootLocationUri = location.getRootShapeTreeLocation();

        URI rootLocationBaseUri = new URI(rootLocationUri.getScheme(), rootLocationUri.getSchemeSpecificPart(), null);

        ShapeTreeResource locatorResource = this.resourceAccessor.getResource(shapeTreeContext, rootLocationBaseUri);
        ensureShapeTreeResourceExists(locatorResource, "Unable to find root shape tree locator");

        return getShapeTreeLocatorFromResource(locatorResource);

    }

    // Return a root shape tree locator associated with a given shape tree location
    private ShapeTreeLocation getRootShapeTreeLocation(ShapeTreeContext shapeTreeContext, ShapeTreeLocation location) throws URISyntaxException, ShapeTreeException {

        ShapeTreeLocator rootLocator = getRootShapeTreeLocator(shapeTreeContext, location);

        for (ShapeTreeLocation rootLocation : rootLocator.getLocations()) {
            if (rootLocation.getUri() != null && rootLocation.getUri().equals(location.getRootShapeTreeLocation())) {
                return rootLocation;
            }
        }
        return null;

    }

    private ShapeTreeResource getRequestTargetContainer(ShapeTreeContext shapeTreeContext, ShapeTreeRequest shapeTreeRequest) throws ShapeTreeException {

        ShapeTreeResource targetContainer = null;
        // if the request is a POST, just return the request resource
        switch (shapeTreeRequest.getMethod()) {

            case POST:
                targetContainer = getRequestResource(shapeTreeContext, shapeTreeRequest);
                break;
            case PATCH:
            case PUT:
                URI targetContainerUri = shapeTreeRequest.getURI().resolve(".");
                if (shapeTreeRequest.getResourceType() != null && shapeTreeRequest.getResourceType().equals(ShapeTreeResourceType.CONTAINER)) {
                    targetContainerUri = shapeTreeRequest.getURI().resolve("..");
                }
                targetContainer = this.resourceAccessor.getResource(shapeTreeContext, targetContainerUri);
                break;

            default:
                return null;

        }
        return targetContainer;
    }

    private void ensureValidationResultIsUsableForAssignment(ValidationResult validationResult, String message) throws ShapeTreeException {
        // Null is a usable state of the validation result in the context of assignment
        if (validationResult != null &&
            (validationResult.getValid() == null ||
            validationResult.getMatchingShapeTree() == null ||
            validationResult.getValidatingShapeTree() == null)) {
                throw new ShapeTreeException(400, message);
        }
    }

    private void ensureRequestResourceIsNotMetadata(ShapeTreeResource shapeTreeResource, String message) throws ShapeTreeException {
        if (shapeTreeResource.isMetadata()) {
            throw new ShapeTreeException(400, message);
        }
    }

    private void ensureShapeTreeResourceExists(ShapeTreeResource shapeTreeResource, String message) throws ShapeTreeException {
        if (shapeTreeResource == null || !shapeTreeResource.isExists()) {
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

    private void ensureShapeTreeLocationExists(ShapeTreeLocation location, String message) throws ShapeTreeException {
        if (location == null) {
            throw new ShapeTreeException(400, message);
        }
    }

    private void ensureResourceIsAssociatedWithMetadataResource(ShapeTreeResource primaryResource, ShapeTreeResource metadataResource, String message) throws ShapeTreeException {
        if (!getShapeTreeMetadataURIForResource(primaryResource).equals(metadataResource.getUri())) {
            throw new ShapeTreeException(500, message);
        }
    }

    private void ensureAllRemovedFromLocatorByDelete(ShapeTreeRequest shapeTreeRequest) throws ShapeTreeException {
        if (!shapeTreeRequest.getMethod().equals(DELETE)) {
            throw new ShapeTreeException(500, "Removal of all ShapeTreeLocations from a ShapeTreeLocator MUST use HTTP DELETE");
        }
    }

    private void ensureRemovedLocationsAreRootLocations(ShapeTreeLocatorDelta delta) throws ShapeTreeException {
        for (ShapeTreeLocation removedLocation : delta.getRemovedLocations()) {
            if (!removedLocation.isRootLocation()) {
                throw new ShapeTreeException(500, "Cannot remove non-root ShapeTreeLocation: " + removedLocation.getUri().toString() + ". Must unplant root location at: " + removedLocation.getRootShapeTreeLocation().toString());
            }
        }
    }

    private void ensureUpdatedLocationsAreRootLocations(ShapeTreeLocatorDelta delta) throws ShapeTreeException {
        for (ShapeTreeLocation updatedLocation : delta.getUpdatedLocations()) {
            if (!updatedLocation.isRootLocation()) {
                throw new ShapeTreeException(500, "Cannot update non-root ShapeTreeLocation: " + updatedLocation.getUri().toString() + ". Must update root location at: " + updatedLocation.getRootShapeTreeLocation().toString());
            }
        }
    }

    private void ensureDeleteIsSuccessful(ShapeTreeResponse response) throws ShapeTreeException {
        List<Integer> successCodes = Arrays.asList(202,204,200);
        if (!successCodes.contains(response.getStatusCode())) {
            throw new ShapeTreeException(500, "Failed to delete metadata resource. Received " + response.getStatusCode() + ": " + response.getBody());
        }
    }

    private void ensureShapeTreeResourceHasLinkHeaders(ShapeTreeResource primaryResource) throws ShapeTreeException {
        if (primaryResource.getAttributes() == null ||
            primaryResource.getAttributes().get(com.janeirodigital.shapetrees.core.enums.HttpHeaders.LINK.getValue()) == null) {
            throw new ShapeTreeException(500, "No available headers for shape tree metadata discovery on " + primaryResource.getUri());
        }
    }

}



class SortByShapeTreeResourceType implements Comparator<ShapeTreeResource>, Serializable {

    // Used for sorting by shape tree resource type with the following order
    // 1. Containers
    // 2. Resources
    // 3. Non-RDF Resources

    public int compare (ShapeTreeResource a, ShapeTreeResource b) {
        return a.getType().compareTo(b.getType());
    }

}
