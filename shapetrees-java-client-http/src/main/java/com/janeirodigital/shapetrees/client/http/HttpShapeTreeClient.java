package com.janeirodigital.shapetrees.client.http;

import com.janeirodigital.shapetrees.client.core.ShapeTreeClient;
import com.janeirodigital.shapetrees.core.*;
import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.core.enums.LinkRelations;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import lombok.extern.slf4j.Slf4j;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;

import java.io.StringWriter;
import java.net.URL;
import java.util.List;
import java.util.Optional;

@Slf4j
public class HttpShapeTreeClient implements ShapeTreeClient {

    private boolean useClientShapeTreeValidation = true;

    @Override
    public boolean isShapeTreeValidationSkipped() {
        return !this.useClientShapeTreeValidation;
    }

    @Override
    public void skipShapeTreeValidation(boolean skipValidation) {
        this.useClientShapeTreeValidation = !skipValidation;
    }

    /**
     * Discover the ShapeTreeManager associated with a given target resource.
     * Implements {@link ShapeTreeClient#discoverShapeTree}
     *
     * Shape Trees, §4.1: This operation is used by a client-side agent to discover any shape trees associated
     * with a given resource. If URL is a managed resource, the associated Shape Tree Manager will be returned.
     * https://shapetrees.org/TR/specification/#discover
     *
     * @param context ShapeTreeContext that would be used for authentication purposes
     * @param targetResource The URL of the target resource for shape tree discovery
     * @return
     * @throws ShapeTreeException
     */
    @Override
    public Optional<ShapeTreeManager> discoverShapeTree(ShapeTreeContext context, URL targetResource) throws ShapeTreeException {

        if (targetResource == null) {
            throw new ShapeTreeException(500, "Must provide a value target resource for discovery");
        }

        log.debug("Discovering shape tree manager managing {}", targetResource);

        // Lookup the target resource for pointer to associated shape tree manager
        final HttpResourceAccessor resourceAccessor = new HttpResourceAccessor();
        ManageableInstance instance = resourceAccessor.getInstance(context, targetResource);
        ManageableResource manageableResource = instance.getManageableResource();

        if  (!manageableResource.isExists()) {
            log.debug("Target resource for discovery {} does not exist", targetResource);
            return Optional.empty();
        }

        if (instance.wasRequestForManager()) {
            throw new ShapeTreeException(500, "Discovery target must not be a shape tree manager resource");
        }

        if (instance.isUnmanaged()) { return Optional.empty(); }

        return Optional.of(instance.getManagerResource().getManager());

    }

    /**
     * Shape Trees, §4.2: This operation marks an existing resource as being managed by one or more shape trees,
     * by associating a shape tree manager with the resource, and turning it into a managed resource.
     *
     * If the resource is already managed, the associated shape tree manager will be updated with another
     * shape tree assignment for the planted shape tree.
     *
     * If the resource is a container that already contains existing resources, and a recursive plant is requested,
     * this operation will perform a depth first traversal through the containment hierarchy, validating
     * and assigning as it works its way back up to the target root resource of this operation.
     *
     * https://shapetrees.org/TR/specification/#plant-shapetree
     *
     * @param context ShapeTreeContext that would be used for authentication purposes
     * @param targetResource The URL of the resource to plant on
     * @param targetShapeTree A URL representing the shape tree to plant for targetResource
     * @param focusNode An optional URL representing the target subject within targetResource used for shape validation
     * @return The URL of the Shape Tree Manager that was planted for targetResource
     * @throws ShapeTreeException
     */
    @Override
    public DocumentResponse plantShapeTree(ShapeTreeContext context, URL targetResource, URL targetShapeTree, URL focusNode) throws ShapeTreeException {

        if (context == null || targetResource == null || targetShapeTree == null) {
            throw new ShapeTreeException(500, "Must provide a valid context, target resource, and target shape tree to the plant shape tree");
        }

        log.debug("Planting shape tree {} on {}: ", targetShapeTree, targetResource);
        log.debug("Focus node: {}", focusNode == null ? "None provided" : focusNode);

        // Lookup the target resource
        final HttpResourceAccessor resourceAccessor = new HttpResourceAccessor();
        ManageableInstance instance = resourceAccessor.getInstance(context, targetResource);
        ManageableResource manageableResource = instance.getManageableResource();

        if (!manageableResource.isExists()) {
            return new DocumentResponse(null, "Cannot find target resource to plant: " + targetResource, 404);
        }

        ShapeTreeManager manager;
        URL managerResourceUrl = instance.getManagerResource().getUrl();

        if (instance.isManaged()) {
            manager = instance.getManagerResource().getManager();
        } else {
            manager = new ShapeTreeManager(managerResourceUrl);
        }

        // Initialize a shape tree assignment based on the supplied parameters
        URL assignmentUrl = manager.mintAssignment();
        ShapeTreeAssignment assignment = new ShapeTreeAssignment(targetShapeTree,
                                                                 targetResource,
                                                                 assignmentUrl,
                                                                 focusNode,
                                                                 null,
                                                                 assignmentUrl);

        // Add the assignment to the manager
        manager.addAssignment(assignment);

        // Get an RDF version of the manager stored in a turtle string
        StringWriter sw = new StringWriter();
        RDFDataMgr.write(sw, manager.getGraph(), Lang.TURTLE);

        // Build an HTTP PUT request with the manager graph in turtle as the content body + link header
        HttpClient fetcher = HttpClientFactoryManager.getFactory().get(this.useClientShapeTreeValidation);
        ResourceAttributes headers = new ResourceAttributes();
        headers.maybeSet(HttpHeaders.AUTHORIZATION.getValue(), context.getAuthorizationHeaderValue());
        return fetcher.fetchShapeTreeResponse(new HttpRequest("PUT", managerResourceUrl, headers, sw.toString(), "text/turtle"));
    }

    @Override
    public DocumentResponse postManagedInstance(ShapeTreeContext context, URL parentContainer, List<URL> focusNodes, List<URL> targetShapeTrees, String proposedResourceName, Boolean isContainer, String bodyString, String contentType) throws ShapeTreeException {

        if (context == null || parentContainer == null) {
            throw new ShapeTreeException(500, "Must provide a valid context and parent container to post shape tree instance");
        }

        log.debug("POST-ing shape tree instance to {}", parentContainer);
        log.debug ("Proposed name: {}", proposedResourceName == null ? "None provided" : proposedResourceName);
        log.debug ("Target Shape Tree: {}", targetShapeTrees == null || targetShapeTrees.isEmpty()  ? "None provided" : targetShapeTrees.toString());
        log.debug("Focus Node: {}", focusNodes == null || focusNodes.isEmpty() ? "None provided" : focusNodes.toString());

        HttpClient fetcher = HttpClientFactoryManager.getFactory().get(this.useClientShapeTreeValidation);
        ResourceAttributes headers = getCommonHeaders(context, focusNodes, targetShapeTrees, isContainer, proposedResourceName, contentType);
        return fetcher.fetchShapeTreeResponse(new HttpRequest("POST", parentContainer, headers, bodyString, contentType));
    }

    // Create via HTTP PUT
    @Override
    public DocumentResponse putManagedInstance(ShapeTreeContext context, URL resourceUrl, List<URL> focusNodes, List<URL> targetShapeTrees, Boolean isContainer, String bodyString, String contentType) throws ShapeTreeException {

        if (context == null || resourceUrl == null) {
            throw new ShapeTreeException(500, "Must provide a valid context and target resource to create shape tree instance via PUT");
        }

        log.debug("Creating shape tree instance via PUT at {}", resourceUrl);
        log.debug ("Target Shape Tree: {}", targetShapeTrees == null || targetShapeTrees.isEmpty()  ? "None provided" : targetShapeTrees.toString());
        log.debug("Focus Node: {}", focusNodes == null || focusNodes.isEmpty() ? "None provided" : focusNodes.toString());

        HttpClient fetcher = HttpClientFactoryManager.getFactory().get(this.useClientShapeTreeValidation);
        ResourceAttributes headers = getCommonHeaders(context, focusNodes, targetShapeTrees, isContainer,null, contentType);
        return fetcher.fetchShapeTreeResponse(new HttpRequest("PUT", resourceUrl, headers, bodyString, contentType));
    }

    // Update via HTTP PUT
    @Override
    public DocumentResponse putManagedInstance(ShapeTreeContext context, URL resourceUrl, List<URL> focusNodes, String bodyString, String contentType) throws ShapeTreeException {

        if (context == null || resourceUrl == null) {
            throw new ShapeTreeException(500, "Must provide a valid context and target resource to update shape tree instance via PUT");
        }

        log.debug("Updating shape tree instance via PUT at {}", resourceUrl);
        log.debug("Focus Node: {}", focusNodes == null || focusNodes.isEmpty() ? "None provided" : focusNodes.toString());

        HttpClient fetcher = HttpClientFactoryManager.getFactory().get(this.useClientShapeTreeValidation);
        ResourceAttributes headers = getCommonHeaders(context, focusNodes, null, null, null, contentType);
        return fetcher.fetchShapeTreeResponse(new HttpRequest("PUT", resourceUrl, headers, bodyString, contentType));
    }

    @Override
    public DocumentResponse patchManagedInstance(ShapeTreeContext context, URL resourceUrl, List<URL> focusNodes, String patchString) throws ShapeTreeException {

        if (context == null || resourceUrl == null || patchString == null) {
            throw new ShapeTreeException(500, "Must provide a valid context, target resource, and PATCH expression to PATCH shape tree instance");
        }

        log.debug("PATCH-ing shape tree instance at {}", resourceUrl);
        log.debug("PATCH String: {}", patchString);
        log.debug("Focus Node: {}", focusNodes == null || focusNodes.isEmpty() ? "None provided" : focusNodes.toString());

        String contentType = "application/sparql-update";

        HttpClient fetcher = HttpClientFactoryManager.getFactory().get(this.useClientShapeTreeValidation);
        ResourceAttributes headers = getCommonHeaders(context, focusNodes, null, null, null, contentType);
        return fetcher.fetchShapeTreeResponse(new HttpRequest("PATCH", resourceUrl, headers, patchString, contentType));
    }

    @Override
    public DocumentResponse deleteManagedInstance(ShapeTreeContext context, URL resourceUrl) throws ShapeTreeException {

        if (context == null || resourceUrl == null) {
            throw new ShapeTreeException(500, "Must provide a valid context and target resource to DELETE shape tree instance");
        }

        log.debug("DELETE-ing shape tree instance at {}", resourceUrl);

        HttpClient fetcher = HttpClientFactoryManager.getFactory().get(this.useClientShapeTreeValidation);
        ResourceAttributes headers = getCommonHeaders(context, null, null, null, null, null);
        return fetcher.fetchShapeTreeResponse(new HttpRequest("DELETE", resourceUrl, headers,null,null));
    }

    @Override
    public DocumentResponse unplantShapeTree(ShapeTreeContext context, URL targetResource, URL targetShapeTree) throws ShapeTreeException {

        if (context == null || targetResource == null || targetShapeTree == null) {
            throw new ShapeTreeException(500, "Must provide a valid context, target resource, and target shape tree to unplant");
        }

        log.debug("Unplanting shape tree {} managing {}: ", targetShapeTree, targetResource);

        // Lookup the target resource
        final HttpResourceAccessor resourceAccessor = new HttpResourceAccessor();
        ManageableInstance instance = resourceAccessor.getInstance(context, targetResource);
        ManageableResource manageableResource = instance.getManageableResource();

        if (!manageableResource.isExists()) {
            return new DocumentResponse(null, "Cannot find target resource to unplant: " + targetResource, 404);
        }

        if (instance.isUnmanaged()) {
            return new DocumentResponse(null, "Cannot unplant target resource that is not managed by a shapetree: " + targetResource, 500);
        }

        // Remove assignment from manager that corresponds with the provided shape tree
        ShapeTreeManager manager = instance.getManagerResource().getManager();
        manager.removeAssignmentForShapeTree(targetShapeTree);

        String method;
        String body;
        String contentType;
        if (manager.getAssignments().isEmpty()) {
            method = "DELETE";
            body = null;
            contentType = null;
        } else {
            // Build an HTTP PUT request with the manager graph in turtle as the content body + link header
            method = "PUT";
            // Get a RDF version of the manager stored in a turtle string
            StringWriter sw = new StringWriter();
            RDFDataMgr.write(sw, manager.getGraph(), Lang.TURTLE);
            body = sw.toString();
            contentType = "text/turtle";
        }

        HttpClient fetcher = HttpClientFactoryManager.getFactory().get(this.useClientShapeTreeValidation);
        return fetcher.fetchShapeTreeResponse(new HttpRequest(method, manager.getId(),
                                              null, // why no getCommonHeaders(context, null, null, null, null, null)
                                              body, contentType));
    }

    private ResourceAttributes getCommonHeaders(ShapeTreeContext context, List<URL> focusNodes, List<URL> targetShapeTrees, Boolean isContainer, String proposedResourceName, String contentType) {
        ResourceAttributes ret = new ResourceAttributes();

        if (context.getAuthorizationHeaderValue() != null) {
            ret.maybeSet(HttpHeaders.AUTHORIZATION.getValue(), context.getAuthorizationHeaderValue());
        }

        if (isContainer != null) {
            String resourceTypeUrl = Boolean.TRUE.equals(isContainer) ? "http://www.w3.org/ns/ldp#Container" : "http://www.w3.org/ns/ldp#Resource";
            ret.maybeSet(HttpHeaders.LINK.getValue(), "<" + resourceTypeUrl + ">; rel=\"type\"");
        }

        if (focusNodes != null && !focusNodes.isEmpty()) {
            for (URL focusNode : focusNodes) {
                ret.maybeSet(HttpHeaders.LINK.getValue(), "<" + focusNode + ">; rel=\"" + LinkRelations.FOCUS_NODE.getValue() + "\"");
            }
        }

        if (targetShapeTrees != null && !targetShapeTrees.isEmpty()) {
            for (URL targetShapeTree : targetShapeTrees) {
                ret.maybeSet(HttpHeaders.LINK.getValue(), "<" + targetShapeTree + ">; rel=\"" + LinkRelations.TARGET_SHAPETREE.getValue() + "\"");
            }
        }

        if (proposedResourceName != null) {
            ret.maybeSet(HttpHeaders.SLUG.getValue(), proposedResourceName);
        }

        if (contentType != null) {
            ret.maybeSet(HttpHeaders.CONTENT_TYPE.getValue(), contentType);
        }

        return ret;
    }
}
