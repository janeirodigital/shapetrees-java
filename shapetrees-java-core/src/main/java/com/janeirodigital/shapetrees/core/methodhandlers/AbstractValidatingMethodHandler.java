package com.janeirodigital.shapetrees.core.methodhandlers;

import com.janeirodigital.shapetrees.core.*;
import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.core.enums.LinkRelations;
import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.helpers.GraphHelper;
import com.janeirodigital.shapetrees.core.models.*;
import com.janeirodigital.shapetrees.core.vocabularies.LdpVocabulary;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.apache.jena.graph.Graph;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.update.UpdateAction;
import org.apache.jena.update.UpdateFactory;
import org.apache.jena.update.UpdateRequest;

import java.io.IOException;
import java.io.Serializable;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.*;

import static com.janeirodigital.shapetrees.core.helpers.GraphHelper.urlToUri;
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

    protected DocumentResponse manageShapeTree(ShapeTreeInstance shapeTreeInstance, ShapeTreeRequest shapeTreeRequest) throws ShapeTreeException {

        Optional<DocumentResponse> validationResponse = null;
        ShapeTreeManager updatedRootManager = getShapeTreeManagerFromRequest(shapeTreeRequest, shapeTreeInstance.getManagerResource());
        ShapeTreeManager existingRootManager = shapeTreeInstance.getManagerResource().getManager();

        // Determine assignments that have been removed, added, and/or updated
        ShapeTreeManagerDelta delta = ShapeTreeManagerDelta.evaluate(existingRootManager, updatedRootManager);

        // It is invalid for a manager resource to be left with no assignments.
        // Shape Trees, §3: A shape tree manager includes one or more shape tree assignments via st:hasAssignment.
        if (delta.allRemoved()) { ensureAllRemovedFromManagerByDelete(shapeTreeRequest); }

        if (delta.wasReduced()) {
            // An existing assignment has been removed from the manager for the managed resource.
            validationResponse = unplantShapeTree(shapeTreeInstance, shapeTreeInstance.getShapeTreeContext(), delta);
            if (validationResponse.isPresent()) { return validationResponse.get(); }
        }

        if (delta.isUpdated()) {
            // An existing assignment has been updated, or new assignments have been added
            validationResponse = plantShapeTree(shapeTreeInstance, shapeTreeInstance.getShapeTreeContext(), updatedRootManager, delta);
            if (validationResponse.isPresent()) { return validationResponse.get(); }
        }

        // TODO: Test: Need a test with reduce and updated delta to make sure we never return success from plant or unplant.

        return successfulValidation();
    }

    /**
     * Plants a shape tree on an existing resource
     * @param shapeTreeInstance
     * @param shapeTreeContext
     * @param updatedRootManager
     * @param delta
     * @return DocumentResponse
     * @throws IOException
     * @throws MalformedURLException
     */
    protected Optional<DocumentResponse> plantShapeTree(ShapeTreeInstance shapeTreeInstance, ShapeTreeContext shapeTreeContext, ShapeTreeManager updatedRootManager, ShapeTreeManagerDelta delta) throws ShapeTreeException {

        // Cannot directly update assignments that are not root locations
        ensureUpdatedAssignmentIsRoot(delta);

        // Run recursive assignment for each updated assignment in the root manager
        for (ShapeTreeAssignment rootAssignment : delta.getUpdatedAssignments()) {
            Optional<DocumentResponse> validationResponse = assignShapeTreeToResource(shapeTreeInstance, shapeTreeContext, updatedRootManager, rootAssignment, rootAssignment, null);
            if (validationResponse.isPresent()) { return validationResponse; }
        }

        return Optional.empty();
    }

    protected  Optional<DocumentResponse> unplantShapeTree(ShapeTreeInstance shapeTreeInstance, ShapeTreeContext shapeTreeContext, ShapeTreeManagerDelta delta) throws ShapeTreeException {

        ensureRemovedAssignmentsAreRoot(delta); // Cannot unplant a non-root location

        // Run recursive unassignment for each removed assignment in the updated root manager
        for (ShapeTreeAssignment rootAssignment : delta.getRemovedAssignments()) {
            Optional<DocumentResponse> validationResponse = unassignShapeTreeFromResource(shapeTreeInstance, shapeTreeContext, rootAssignment);
            if (validationResponse.isPresent()) { return validationResponse; }
        }

        return Optional.empty();
    }

    // TODO: #87: do sanity checks on meta of meta, c.f. @see https://github.com/xformativ/shapetrees-java/issues/87
    protected Optional<DocumentResponse> createShapeTreeInstance(ShapeTreeInstance shapeTreeInstance, ShapeTreeInstance containerResource, ShapeTreeRequest shapeTreeRequest, String proposedName) throws ShapeTreeException {
        // Sanity check user-owned resource @@ delete 'cause type checks
        ensureShapeTreeInstanceResourceExists(containerResource.getManagedResource(),"Target container for resource creation not found");
        ensureRequestResourceIsContainer(containerResource.getManagedResource(),"Cannot create a shape tree instance in a non-container resource");

        // Prepare the target resource for validation and creation
        URL targetResourceUrl = normalizeSolidResourceUrl(containerResource.getManagedResource().getUrl(), proposedName, shapeTreeRequest.getResourceType());
        ensureTargetManagedResourceDoesNotExist(shapeTreeInstance.getShapeTreeContext(), targetResourceUrl,"Cannot create a shape tree instance in a non-container resource " + targetResourceUrl);

        ensureShapeTreeInstanceResourceExists(containerResource.getManagerResource(), "Should not be creating a shape tree instance on an unmanaged target container");

        ShapeTreeManager containerManager = containerResource.getManagerResource().getManager();
        ensureShapeTreeManagerExists(containerManager, "Cannot have a shape tree manager resource without a shape tree manager containing at least one shape tree assignment");

        // Get the shape tree associated that specifies what resources can be contained by the target container (st:contains)
        ShapeTreeAssignment containingAssignment = containerManager.getContainingAssignment();

        if (containingAssignment == null) {
            // If there is no containing shape tree for the target container, then the request is valid and can
            // be passed straight through
            return Optional.empty();
        }

        URL containerShapeTreeUrl = containingAssignment.getShapeTree();
        ShapeTree containerShapeTree = ShapeTreeFactory.getShapeTree(containerShapeTreeUrl);

        URL targetShapeTree = getIncomingTargetShapeTreeHint(shapeTreeRequest, targetResourceUrl);
        URL incomingFocusNode = getIncomingResolvedFocusNode(shapeTreeRequest, targetResourceUrl);
        Graph incomingBodyGraph = getIncomingBodyGraph(shapeTreeRequest, targetResourceUrl, null);

        ValidationResult validationResult = containerShapeTree.validateContainedResource(proposedName, shapeTreeRequest.getResourceType(), targetShapeTree, incomingBodyGraph, incomingFocusNode);
        if (Boolean.FALSE.equals(validationResult.isValid())) {
            return failValidation(validationResult);
        }

        log.debug("Creating shape tree instance at {}", targetResourceUrl);

        ShapeTreeInstance createdResource = new ShapeTreeInstance(targetResourceUrl, this.resourceAccessor, shapeTreeInstance.getShapeTreeContext(), shapeTreeRequest);

        ShapeTreeAssignment rootShapeTreeAssignment = getRootAssignment(shapeTreeInstance.getShapeTreeContext(), containingAssignment);
        ensureAssignmentExists(rootShapeTreeAssignment, "Unable to find root shape tree assignment at " + containingAssignment.getRootAssignment());

        log.debug("Assigning shape tree to created resource: {}", createdResource.getManagerResource().getUrl());
        // Note: By providing the positive advance validationResult, we let the assignment operation know that validation
        // has already been performed with a positive result, and avoid having it perform the validation a second time
        Optional<DocumentResponse> assignResult = assignShapeTreeToResource(createdResource, shapeTreeInstance.getShapeTreeContext(), null, rootShapeTreeAssignment, containingAssignment, validationResult);
        if (assignResult.isPresent()) { return assignResult; }

        return Optional.of(successfulValidation());
    }

    protected Optional<DocumentResponse> updateShapeTreeInstance(ShapeTreeInstance targetResource, ShapeTreeContext shapeTreeContext, ShapeTreeRequest shapeTreeRequest) throws ShapeTreeException {


        ensureShapeTreeInstanceResourceExists(targetResource.getManagedResource(),"Target resource to update not found");
        ensureShapeTreeInstanceResourceExists(targetResource.getManagerResource(), "Should not be updating an unmanaged resource as a shape tree instance");

        ShapeTreeManager manager = targetResource.getManagerResource().getManager();
        ensureShapeTreeManagerExists(manager, "Cannot have a shape tree manager resource without a shape tree manager with at least one shape tree assignment");

        for (ShapeTreeAssignment assignment : manager.getAssignments()) {

            // Evaluate the update against each ShapeTreeAssignment managing the resource.
            // All must pass for the update to validate
            ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(assignment.getShapeTree());
            URL managedResourceUrl = targetResource.getManagedResource().getUrl();
            ValidationResult validationResult = shapeTree.validateResource(null, shapeTreeRequest.getResourceType(), getIncomingBodyGraph(shapeTreeRequest, managedResourceUrl, targetResource.getManagedResource()), getIncomingResolvedFocusNode(shapeTreeRequest, managedResourceUrl));
            if (Boolean.FALSE.equals(validationResult.isValid())) { return failValidation(validationResult); }

        }

        // No issues with validation, so the request is passed along
        return Optional.empty();

    }

    protected Optional<DocumentResponse> deleteShapeTreeInstance() {
        // Nothing to validate in a delete request, so the request is passed along
        return Optional.empty();
    }

    protected Optional<DocumentResponse> assignShapeTreeToResource(ShapeTreeInstance shapeTreeInstance,
                                                                   ShapeTreeContext shapeTreeContext,
                                                                   ShapeTreeManager rootManager,
                                                                   ShapeTreeAssignment rootAssignment,
                                                                   ShapeTreeAssignment parentAssignment,
                                                                   ValidationResult advanceValidationResult)
            throws ShapeTreeException {

        ShapeTree managedResourceShapeTree = null;
        ShapeTreeManager shapeTreeManager = null;
        URL matchingFocusNode = null;
        ShapeTreeAssignment managedResourceAssignment = null;
        Optional<DocumentResponse> validationResponse = null;

        ensureValidationResultIsUsableForAssignment(advanceValidationResult, "Invalid advance validation result provided for resource assignment");
        if (advanceValidationResult != null) { managedResourceShapeTree = advanceValidationResult.getMatchingShapeTree(); }
        if (advanceValidationResult != null) { matchingFocusNode = advanceValidationResult.getMatchingFocusNode(); }

        if (atRootOfPlantHierarchy(rootAssignment, shapeTreeInstance.getManagedResource())) {

            // If we are at the root of the plant hierarchy we don't need to validate the managed resource against
            // a shape tree managing a parent container. We only need to validate the managed resource against
            // the shape tree that is being planted at the root to ensure it conforms.
            managedResourceShapeTree = ShapeTreeFactory.getShapeTree(rootAssignment.getShapeTree());
            if (advanceValidationResult == null) {    // If this validation wasn't performed in advance
                ValidationResult validationResult = managedResourceShapeTree.validateResource(shapeTreeInstance.getManagedResource());
                if (Boolean.FALSE.equals(validationResult.isValid())) { return failValidation(validationResult); }
                matchingFocusNode = validationResult.getMatchingFocusNode();
            }

        } else {

            // Not at the root of the plant hierarchy. Validate proposed resource against the shape tree
            // managing the parent container, then extract the matching shape tree and focus node on success
            if (advanceValidationResult == null) {    // If this validation wasn't performed in advance
                ShapeTree parentShapeTree = ShapeTreeFactory.getShapeTree(parentAssignment.getShapeTree());
                ValidationResult validationResult = parentShapeTree.validateContainedResource(shapeTreeInstance.getManagedResource());
                if (Boolean.FALSE.equals(validationResult.isValid())) { return failValidation(validationResult); }
                managedResourceShapeTree = validationResult.getMatchingShapeTree();
                matchingFocusNode = validationResult.getMatchingFocusNode();
            }

        }

        shapeTreeManager = getManagerForAssignment(shapeTreeInstance, rootManager, rootAssignment);
        managedResourceAssignment = getAssignment(shapeTreeInstance.getManagedResource(), shapeTreeManager, rootAssignment, managedResourceShapeTree, matchingFocusNode);

        // If the primary resource is a container, and its shape tree specifies its contents with st:contains
        // Recursively traverse the hierarchy and perform shape tree assignment
        if (shapeTreeInstance.getManagedResource().isContainer() && managedResourceShapeTree.getContains() != null && !managedResourceShapeTree.getContains().isEmpty()) {

            // If the container is not empty, perform a recursive, depth first validation and assignment for each
            // contained resource by recursively calling this method (assignShapeTreeToResource)
            // TODO - Provide a configurable maximum limit on contained resources for a recursive plant, generate ShapeTreeException
            List<ShapeTreeInstance> containedResources = this.resourceAccessor.getContainedResources(shapeTreeContext, shapeTreeInstance.getManagedResource().getUrl());
            if (!containedResources.isEmpty()) {
                Collections.sort(containedResources, new SortByShapeTreeResourceType());  // Evaluate containers, then resources
                for (ShapeTreeInstance containedResource : containedResources) {
                    validationResponse = assignShapeTreeToResource(containedResource, shapeTreeContext, null, rootAssignment, managedResourceAssignment, null);
                    if (validationResponse.isPresent()) { return validationResponse; }
                }
            }
        }
        shapeTreeInstance.createOrUpdateManagerResource(shapeTreeManager);

        return Optional.empty();

    }

    protected Optional<DocumentResponse> unassignShapeTreeFromResource(ShapeTreeInstance shapeTreeInstance, ShapeTreeContext shapeTreeContext,
                                                                       ShapeTreeAssignment rootAssignment) throws ShapeTreeException {


        ensureShapeTreeInstanceResourceExists(shapeTreeInstance.getManagedResource(), "Cannot remove assignment from non-existent managed resource");
        ensureShapeTreeInstanceResourceExists(shapeTreeInstance.getManagerResource(), "Cannot remove assignment from non-existent manager resource");

        ShapeTreeManager shapeTreeManager = shapeTreeInstance.getManagerResource().getManager();
        ShapeTreeAssignment removeAssignment = shapeTreeManager.getAssignmentForRoot(rootAssignment);
        ShapeTree managedResourceShapeTree = ShapeTreeFactory.getShapeTree(removeAssignment.getShapeTree());

        Optional<DocumentResponse> validationResponse = null;

        // If the managed resource is a container, and its shape tree specifies its contents with st:contains
        // Recursively traverse the hierarchy and perform shape tree unassignment
        if (shapeTreeInstance.getManagedResource().isContainer() && managedResourceShapeTree.getContains() != null && !managedResourceShapeTree.getContains().isEmpty()) {

            // TODO - Should there also be a configurable maximum limit on unplanting?
            List<ShapeTreeInstance> containedResources = this.resourceAccessor.getContainedResources(shapeTreeContext, shapeTreeInstance.getManagedResource().getUrl());
            // If the container is not empty
            if (!containedResources.isEmpty()) {
                // Sort contained resources so that containers are evaluated first, then resources
                Collections.sort(containedResources, new SortByShapeTreeResourceType());
                // Perform a depth first unassignment for each contained resource
                for (ShapeTreeInstance containedResource : containedResources) {
                    // Recursively call this function on the contained resource
                    validationResponse = unassignShapeTreeFromResource(containedResource, shapeTreeContext, rootAssignment);
                    if (validationResponse.isPresent()) { return validationResponse; }
                }
            }
        }

        shapeTreeManager.removeAssignment(removeAssignment);

        deleteOrUpdateManagerResource(shapeTreeContext, shapeTreeInstance.getManagerResource(), shapeTreeManager);

        return Optional.empty();

    }

    // TODO - Consider moving to ShapeTreeRequest Interface / implementations or new RequestHelper
    /**
     * Builds a ShapeTreeContext from the incoming request.  Specifically it retrieves
     * the incoming Authorization header and stashes that value for use on any additional requests made during
     * validation.
     * @param shapeTreeRequest Incoming request
     * @return ShapeTreeContext object populated with authentication details, if present
     */
    protected ShapeTreeContext buildContextFromRequest(ShapeTreeRequest shapeTreeRequest) {
        return new ShapeTreeContext(shapeTreeRequest.getHeaderValue(HttpHeaders.AUTHORIZATION.getValue()));
    }

    // TODO - Consider moving to ShapeTreeRequest Interface / implementations or new RequestHelper
    /**
     * This determines the type of resource being processed.
     *
     * Initial test is based on the incoming request headers, specifically the Content-Type header.
     * If the content type is not one of the accepted RDF types, it will be treated as a NON-RDF source.
     *
     * Then the determination becomes whether or not the resource is a container.
     *
     * If it is a PATCH or PUT and the URL provided already exists, then the existing resource's Link header(s)
     * are used to determine if it is a container or not.
     *
     * If it is a POST or if the resource does not already exist, the incoming request Link header(s) are relied
     * upon.
     *
     * @param shapeTreeRequest The current incoming request
     * @param existingResource The resource located at the incoming request's URL
     * @return ShapeTreeResourceType aligning to current request
     * @throws ShapeTreeException ShapeTreeException throw, specifically if Content-Type is not included on request
     */
    protected ShapeTreeResourceType determineResourceType(ShapeTreeRequest shapeTreeRequest, ShapeTreeInstance existingResource) throws ShapeTreeException {
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
        boolean resourceAlreadyExists = existingResource.getManagedResource().wasSuccessful();
        if ((shapeTreeRequest.getMethod().equals(PUT) || shapeTreeRequest.getMethod().equals(PATCH)) && resourceAlreadyExists) {
            isContainer = existingResource.getManagedResource().isContainer();
        } else if (shapeTreeRequest.getLinkHeaders() != null) {
            isContainer = getIsContainerFromRequest(shapeTreeRequest);
        }

        return isContainer ? ShapeTreeResourceType.CONTAINER : ShapeTreeResourceType.RESOURCE;
    }

    /**
     * Normalizes the BaseURL to use for a request based on the incoming request.
     * @param url URL of request
     * @param requestedName Requested name of resource (provided on created resources via POST)
     * @param resourceType Description of resource (Container, NonRDF, Resource)
     * @return BaseURL to use for RDF Graphs
     * @throws ShapeTreeException ShapeTreeException
     */
    protected URL normalizeSolidResourceUrl(URL url, String requestedName, ShapeTreeResourceType resourceType) throws ShapeTreeException {
        String urlString = url.toString();
        if (requestedName != null) {
            urlString += requestedName;
        }
        if (resourceType == ShapeTreeResourceType.CONTAINER && !urlString.endsWith("/")) {
            urlString += "/";
        }
        try {
            return new URL(urlString);
        } catch (MalformedURLException ex) {
            throw new ShapeTreeException(500, "normalized to malformed URL <" + urlString + "> - " + ex.getMessage());
        }
    }

    // TODO - Consider moving to ShapeTreeRequest Interface / implementations or new RequestHelper
    /**
     * Loads body of request into graph
     * @param shapeTreeRequest Request
     * @param baseUrl BaseURL to use for graph
     * @param targetResource
     * @return Graph representation of request body
     * @throws ShapeTreeException ShapeTreeException
     */
    protected Graph getIncomingBodyGraph(ShapeTreeRequest shapeTreeRequest, URL baseUrl, ShapeTreeInstance.Resource targetResource) throws ShapeTreeException {
        log.debug("Reading request body into graph with baseUrl {}", baseUrl);

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
                targetResourceGraph = targetResource.getGraph(baseUrl);
            }

            if (targetResourceGraph == null) {   // if the target resource doesn't exist or has no content
                log.debug("Existing target resource graph to patch does not exist.  Creating an empty graph.");
                targetResourceGraph = ModelFactory.createDefaultModel().getGraph();
            }

            // Perform a SPARQL update locally to ensure that resulting graph validates against ShapeTree
            UpdateRequest updateRequest = UpdateFactory.create(shapeTreeRequest.getBody(), baseUrl.toString());
            UpdateAction.execute(updateRequest, targetResourceGraph);

            if (targetResourceGraph == null) {
                throw new ShapeTreeException(400, "No graph after update");
            }

        } else {
            targetResourceGraph = GraphHelper.readStringIntoGraph(urlToUri(baseUrl), shapeTreeRequest.getBody(), shapeTreeRequest.getContentType());
        }

        return targetResourceGraph;
    }

    // TODO - Consider moving to ShapeTreeRequest Interface / implementations or new RequestHelper
    /**
     * Gets focus node from request header
     * @param shapeTreeRequest Request
     * @param baseUrl Base URL for use on relative focus nodes
     * @return URL of focus node
     * @throws IOException IOException
     */
    protected URL getIncomingResolvedFocusNode(ShapeTreeRequest shapeTreeRequest, URL baseUrl) throws ShapeTreeException {
        final String focusNode = shapeTreeRequest.getLinkHeaders().firstValue(LinkRelations.FOCUS_NODE.getValue()).orElse(null);
        if (focusNode != null) {
            try {
                return new URL(baseUrl, focusNode);
            } catch (MalformedURLException e) {
                throw new ShapeTreeException(500, "Malformed focus node when resolving <" + focusNode + "> against <" + baseUrl + ">");
            }
        }
        return null;
    }

    // TODO - Consider moving to ShapeTreeRequest Interface / implementations or new RequestHelper
    /**
     * Gets target shape tree / hint from request header
     * @param shapeTreeRequest Request
     * @return URL value of target shape tree
     * @throws ShapeTreeException ShapeTreeException
     */
    protected URL getIncomingTargetShapeTreeHint(ShapeTreeRequest shapeTreeRequest, URL baseUrl) throws ShapeTreeException {
        final String targetShapeTree = shapeTreeRequest.getLinkHeaders().firstValue(LinkRelations.TARGET_SHAPETREE.getValue()).orElse(null);
        if (targetShapeTree != null) {
            try {
                return new URL(targetShapeTree);
            } catch (MalformedURLException e) {
                throw new ShapeTreeException(500, "Malformed focus node when resolving <" + targetShapeTree + "> against <" + baseUrl + ">");
            }
        }
        return null;
    }

    // TODO - Consider moving to ShapeTreeRequest Interface / implementations or new RequestHelper
    /**
     * Determines if a resource should be treated as a container based on its request Link headers
     * @param shapeTreeRequest Request
     * @return Is the resource a container?
     */
    protected Boolean getIsContainerFromRequest(ShapeTreeRequest shapeTreeRequest) {
        // First try to determine based on link headers
        if (shapeTreeRequest.getLinkHeaders() != null) {
            final List<String> typeLinks = shapeTreeRequest.getLinkHeaders().allValues(LinkRelations.TYPE.getValue());
            if (typeLinks.size() != 0) {
                return (typeLinks.contains(LdpVocabulary.CONTAINER) ||
                        typeLinks.contains(LdpVocabulary.BASIC_CONTAINER));
            }
        }
        // As a secondary attempt, use slash path semantics
        return shapeTreeRequest.getUrl().getPath().endsWith("/");
    }

    // TODO - Consider moving to ShapeTreeRequest Interface / implementations or new RequestHelper
    /**
     * Determines whether a content type is a supported RDF type
     * @param incomingRequestContentType Content type to test
     * @return Boolean indicating whether it is RDF or not
     */
    protected boolean determineIsNonRdfSource(String incomingRequestContentType) {
        return (!this.supportedRDFContentTypes.contains(incomingRequestContentType.toLowerCase()) &&
                !this.supportedSPARQLContentTypes.contains(incomingRequestContentType.toLowerCase()));
    }

    // TODO - Consider moving to ShapeTreeRequest interface / implementations
    protected ShapeTreeManager getShapeTreeManagerFromRequest(ShapeTreeRequest shapeTreeRequest, ShapeTreeInstance.ManagerResource managerResource) throws ShapeTreeException {

        Graph incomingBodyGraph = getIncomingBodyGraph(shapeTreeRequest, normalizeSolidResourceUrl(shapeTreeRequest.getUrl(), null, ShapeTreeResourceType.RESOURCE), managerResource);
        if (incomingBodyGraph == null) { return null; }
        return ShapeTreeManager.getFromGraph(shapeTreeRequest.getUrl(), incomingBodyGraph);
    }

    private void deleteOrUpdateManagerResource(ShapeTreeContext shapeTreeContext,
                                               ShapeTreeInstance.ManagerResource managerResource,
                                               ShapeTreeManager shapeTreeManager) throws ShapeTreeException {

        if (shapeTreeManager.getAssignments().isEmpty()) {
            DocumentResponse response = this.resourceAccessor.deleteResource(shapeTreeContext, managerResource);
            ensureDeleteIsSuccessful(response);
        } else {
            // Update the existing manager resource for the managed resource
            this.resourceAccessor.updateResource(shapeTreeContext, "PUT", managerResource, shapeTreeManager.getGraph().toString());
        }

    }

    private ShapeTreeManager getManagerForAssignment(ShapeTreeInstance shapeTreeInstance,
                                                     ShapeTreeManager rootManager,
                                                     ShapeTreeAssignment rootAssignment) throws ShapeTreeException {

        ShapeTreeManager shapeTreeManager = null;

        // When at the top of the plant hierarchy, use the root manager from the initial plant request body
        if (atRootOfPlantHierarchy(rootAssignment, shapeTreeInstance.getManagedResource())) { return rootManager; }

        if (!shapeTreeInstance.getManagerResource().wasSuccessful()) {
            // If the existing manager resource doesn't exist make a new shape tree manager
            shapeTreeManager = new ShapeTreeManager(shapeTreeInstance.getManagerResource().getUrl());
        } else {
            // Get the existing shape tree manager from the manager resource graph
            // TODO - this was seemingly incorrect before it was adjusted. Needs to be debugged and confirmed as working properly now
            Graph managerGraph = shapeTreeInstance.getManagerResource().getGraph(shapeTreeInstance.getManagerResource().getUrl());
            shapeTreeManager = ShapeTreeManager.getFromGraph(shapeTreeInstance.getManagerResource().getUrl(), managerGraph);
        }

        return shapeTreeManager;

    }

    private ShapeTreeAssignment getAssignment(ShapeTreeInstance.Resource managedResource,
                                              ShapeTreeManager shapeTreeManager,
                                              ShapeTreeAssignment rootAssignment,
                                              ShapeTree managedResourceShapeTree,
                                              URL matchingFocusNode) throws ShapeTreeException {

        URL assignmentUrl = null;

        if (!atRootOfPlantHierarchy(rootAssignment, managedResource)) {
            // Mint a new assignment URL, since it wouldn't have been passed in the initial request body
            assignmentUrl = shapeTreeManager.mintAssignment();
        }

        // Build the managed resource assignment
        URL matchingNode = matchingFocusNode == null ? null : matchingFocusNode;
        ShapeTreeAssignment managedResourceAssignment = new ShapeTreeAssignment(managedResourceShapeTree.getId(),
                managedResource.getUrl(),
                rootAssignment.getUrl(),
                matchingNode,
                managedResourceShapeTree.getShape(),
                assignmentUrl);

        if (!atRootOfPlantHierarchy(rootAssignment, managedResource)) {
            // Add the shape tree assignment to the shape tree managed for the managed resource
            shapeTreeManager.addAssignment(managedResourceAssignment);
        }

        return managedResourceAssignment;

    }

    private boolean atRootOfPlantHierarchy(ShapeTreeAssignment rootAssignment, ShapeTreeInstance.Resource managedResource) {
        return rootAssignment.getManagedResource().toString().equals(managedResource.getUrl().toString());
    }

    // TODO - Move to ShapeTreeInstance
    // Return a root shape tree manager associated with a given shape tree assignment
    private ShapeTreeManager getRootManager(ShapeTreeContext shapeTreeContext, ShapeTreeAssignment assignment) throws ShapeTreeException {

        URL rootAssignmentUrl = assignment.getRootAssignment();
        ShapeTreeInstance.ManagerResource managerResource = new ShapeTreeInstance(rootAssignmentUrl, this.resourceAccessor, shapeTreeContext).getManagerResource(); // this.resourceAccessor.getResource(shapeTreeContext, rootLocationBaseUrl);

        return managerResource.getManager();

    }

    // TODO - Move to ShapeTreeInstance
    // Return a root shape tree manager associated with a given shape tree assignment
    private ShapeTreeAssignment getRootAssignment(ShapeTreeContext shapeTreeContext, ShapeTreeAssignment assignment) throws ShapeTreeException {

        ShapeTreeManager rootManager = getRootManager(shapeTreeContext, assignment);

        for (ShapeTreeAssignment rootAssignment : rootManager.getAssignments()) {
            if (rootAssignment.getUrl() != null && rootAssignment.getUrl().equals(assignment.getRootAssignment())) {
                return rootAssignment;
            }
        }
        return null;

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

    private void ensureShapeTreeInstanceResourceExists(ShapeTreeInstance.Resource shapeTreeResource, String message) throws ShapeTreeException {
        if (shapeTreeResource == null || !shapeTreeResource.wasSuccessful()) {
            throw new ShapeTreeException(404, message);
        }
    }

    private void ensureRequestResourceIsContainer(ShapeTreeInstance.ManagedResource shapeTreeResource, String message) throws ShapeTreeException {
        if (!shapeTreeResource.isContainer()) {
            throw new ShapeTreeException(400, message);
        }
    }

    private void ensureTargetManagedResourceDoesNotExist(ShapeTreeContext shapeTreeContext, URL targetResourceUrl, String message) throws ShapeTreeException {
        ShapeTreeInstance targetManagedResource = new ShapeTreeInstance(targetResourceUrl, this.resourceAccessor, shapeTreeContext);
        if (targetManagedResource.wasCreatedFromManager() || targetManagedResource.getManagedResource().wasSuccessful()) {
            throw new ShapeTreeException(409, message);
        }
    }

    private void ensureShapeTreeManagerExists(ShapeTreeManager manager, String message) throws ShapeTreeException {
        if (manager == null || manager.getAssignments() == null || manager.getAssignments().isEmpty()) {
            throw new ShapeTreeException(400, message);
        }
    }

    private void ensureAssignmentExists(ShapeTreeAssignment assignment, String message) throws ShapeTreeException {
        if (assignment == null) {
            throw new ShapeTreeException(400, message);
        }
    }

    private void ensureAllRemovedFromManagerByDelete(ShapeTreeRequest shapeTreeRequest) throws ShapeTreeException {
        if (!shapeTreeRequest.getMethod().equals(DELETE)) {
            throw new ShapeTreeException(500, "Removal of all ShapeTreeAssignments from a ShapeTreeManager MUST use HTTP DELETE");
        }
    }

    private void ensureRemovedAssignmentsAreRoot(ShapeTreeManagerDelta delta) throws ShapeTreeException {
        for (ShapeTreeAssignment assignment : delta.getRemovedAssignments()) {
            if (!assignment.isRootAssignment()) {
                throw new ShapeTreeException(500, "Cannot remove non-root assignment: " + assignment.getUrl().toString() + ". Must unplant root assignment at: " + assignment.getRootAssignment().toString());
            }
        }
    }

    private void ensureUpdatedAssignmentIsRoot(ShapeTreeManagerDelta delta) throws ShapeTreeException {
        for (ShapeTreeAssignment updatedAssignment : delta.getUpdatedAssignments()) {
            if (!updatedAssignment.isRootAssignment()) {
                throw new ShapeTreeException(500, "Cannot update non-root assignment: " + updatedAssignment.getUrl().toString() + ". Must update root assignment at: " + updatedAssignment.getRootAssignment().toString());
            }
        }
    }

    private void ensureDeleteIsSuccessful(DocumentResponse response) throws ShapeTreeException {
        List<Integer> successCodes = Arrays.asList(202,204,200);
        if (!successCodes.contains(response.getStatusCode())) {
            throw new ShapeTreeException(500, "Failed to delete manager resource. Received " + response.getStatusCode() + ": " + response.getBody());
        }
    }

    private DocumentResponse successfulValidation() {
        return new DocumentResponse(new ResourceAttributes(), "OK", 201);
    }

    private Optional<DocumentResponse> failValidation(ValidationResult validationResult) {
        return Optional.of(new DocumentResponse(new ResourceAttributes(), validationResult.getMessage(),422));
    }
}



class SortByShapeTreeResourceType implements Comparator<ShapeTreeInstance>, Serializable {

    // Used for sorting by shape tree resource type with the following order
    // 1. Containers
    // 2. Resources
    // 3. Non-RDF Resources

    @SneakyThrows // @@ These are known to be user-owned
    public int compare (ShapeTreeInstance a, ShapeTreeInstance b) {
        return a.getManagedResource().getResourceType().compareTo(b.getManagedResource().getResourceType());
    }

}
