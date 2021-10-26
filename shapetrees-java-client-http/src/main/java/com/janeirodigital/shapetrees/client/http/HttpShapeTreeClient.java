package com.janeirodigital.shapetrees.client.http;

import com.janeirodigital.shapetrees.client.core.ShapeTreeClient;
import com.janeirodigital.shapetrees.core.DocumentResponse;
import com.janeirodigital.shapetrees.core.ResourceAttributes;
import com.janeirodigital.shapetrees.core.ShapeTreeResource;
import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.core.enums.LinkRelations;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.helpers.GraphHelper;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;
import com.janeirodigital.shapetrees.core.models.ShapeTreeLocation;
import com.janeirodigital.shapetrees.core.models.ShapeTreeLocator;
import lombok.extern.slf4j.Slf4j;
import org.apache.jena.graph.Graph;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;

import java.io.StringWriter;
import java.net.URL;
import java.net.MalformedURLException;
import java.util.Optional;

import static com.janeirodigital.shapetrees.core.helpers.GraphHelper.urlToUri;

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
     * Discover the ShapeTreeLocator associated with a given target resource.
     * Implements {@link ShapeTreeClient#discoverShapeTree}
     *
     * Shape Trees, §4.1: This operation is used by a client-side agent to discover any shape trees associated
     * with a given resource. If URL is a managed resource, the associated Shape Tree Locator will be returned.
     * https://shapetrees.org/TR/specification/#discover
     *
     * @param context ShapeTreeContext that would be used for authentication purposes
     * @param targetResource The URL of the target resource for shape tree discovery
     * @return
     * @throws ShapeTreeException
     */
    @Override
    public Optional<ShapeTreeLocator> discoverShapeTree(ShapeTreeContext context, URL targetResource) throws ShapeTreeException {

        if (targetResource == null) {
            throw new ShapeTreeException(500, "Must provide a value target resource for discovery");
        }

        log.debug("Discovering shape tree locator managing {}", targetResource);

        // Lookup the target resource for pointer to associated shape tree locator
        final HttpRemoteResourceAccessor resourceAccessor = new HttpRemoteResourceAccessor();
        ShapeTreeResource resource = new ShapeTreeResource(targetResource, resourceAccessor, context);
        ShapeTreeResource.Primary primaryResource = resource.getUserOwnedResourceFork();
        URL metadataUri = primaryResource.getMetadataResourceUrl().orElseThrow( // politely handle no-metadata case before getMetadataResourceFork() throws less informatively
                () -> new ShapeTreeException(500, "No metadata resource for <" + primaryResource.getUrl() + ">")
        );

        if  (Boolean.FALSE.equals(primaryResource.isExists())) {
            log.debug("Target resource for discovery {} does not exist", targetResource);
            return Optional.empty();
        }

        // Lookup the associated shape tree locator resource based on the pointer
        ShapeTreeResource.Metadata locatorResource = resource.getMetadataResourceFork();

        // Ensure the metadata resource exists
        // Shape Trees, §4.1: If LOCATORURI is empty, the resource at RESOURCEURI is not a managed resource,
        // and no shape tree locator will be returned.
        if (Boolean.FALSE.equals(locatorResource.isExists())) {
            log.debug("Shape tree locator for {} does not exist", targetResource);
            return Optional.empty();
        }

        Graph locatorGraph = GraphHelper.readStringIntoGraph(urlToUri(metadataUri), locatorResource.getBody(), locatorResource.getAttributes().firstValue(HttpHeaders.CONTENT_TYPE.getValue()).orElse(null));

        // Populate a ShapeTreeLocator from the graph in locatorResource and return it
        return Optional.of(ShapeTreeLocator.getShapeTreeLocatorFromGraph(metadataUri, locatorGraph)
        );
    }

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
     * @param context ShapeTreeContext that would be used for authentication purposes
     * @param targetResource The URL of the resource to plant on
     * @param targetShapeTree A URL representing the shape tree to plant for targetResource
     * @param focusNode An optional URL representing the target subject within targetResource used for shape validation
     * @return The URL of the Shape Tree Locator that was planted for targetResource
     * @throws ShapeTreeException
     * @throws MalformedURLException
     */
    @Override
    public DocumentResponse plantShapeTree(ShapeTreeContext context, URL targetResource, URL targetShapeTree, URL focusNode) throws ShapeTreeException, MalformedURLException {

        if (context == null || targetResource == null || targetShapeTree == null) {
            throw new ShapeTreeException(500, "Must provide a valid context, target resource, and target shape tree to the plant shape tree");
        }

        log.debug("Planting shape tree {} on {}: ", targetShapeTree, targetResource);
        log.debug("Focus node: {}", focusNode == null ? "None provided" : focusNode);

        // Lookup the target resource
        final HttpRemoteResourceAccessor resourceAccessor = new HttpRemoteResourceAccessor();
        ShapeTreeResource resource = new ShapeTreeResource(targetResource, resourceAccessor, context);
        ShapeTreeResource.Primary primaryResource = resource.getUserOwnedResourceFork();
        if (Boolean.FALSE.equals(primaryResource.isExists())) {
            return new DocumentResponse(null, "Cannot find target resource to plant: " + targetResource, 404);
        }
        URL metadataUrl = primaryResource.getMetadataResourceUrl().orElseThrow( // politely handle no-metadata case before getMetadataResourceFork() throws less informatively
                () -> new IllegalStateException("No metadata resource for <" + primaryResource.getUrl() + ">") // TODO: Spec/API: should this return a 404 or something like that? nearby: ProjectTests.failPlantOnMissingDataContainer()
        );

        // Determine whether the target resource is already a managed resource
        ShapeTreeLocator locator = discoverShapeTree(context, targetResource)
            // If the target resource is not managed, initialize a new locator
            .orElse(new ShapeTreeLocator(metadataUrl));

        // Initialize a shape tree location based on the supplied parameters
        URL locationUrl = locator.mintLocation();
        ShapeTreeLocation location = new ShapeTreeLocation(targetShapeTree,
                                                           targetResource.toString(),
                                                           locationUrl,
                                                           focusNode == null ? null : focusNode.toString(),
                                                           null,
                                                           locationUrl);

        // Add the location to the locator
        locator.addShapeTreeLocation(location);

        // Get an RDF version of the locator stored in a turtle string
        StringWriter sw = new StringWriter();
        RDFDataMgr.write(sw, locator.getGraph(), Lang.TURTLE);

        // Build an HTTP PUT request with the locator graph in turtle as the content body + link header
        HttpClient fetcher = AbstractHttpClientFactory.getFactory().get(this.useClientShapeTreeValidation);
        ResourceAttributes headers = new ResourceAttributes();
        headers.maybeSet(HttpHeaders.AUTHORIZATION.getValue(), context.getAuthorizationHeaderValue());
        return fetcher.fetchShapeTreeResponse(new HttpRequest("PUT", metadataUrl, headers, sw.toString(), "text/turtle"));
    }

    @Override
    public DocumentResponse postShapeTreeInstance(ShapeTreeContext context, URL parentContainer, URL focusNode, URL targetShapeTree, String proposedResourceName, Boolean isContainer, String bodyString, String contentType) throws ShapeTreeException {

        if (context == null || parentContainer == null) {
            throw new ShapeTreeException(500, "Must provide a valid context and parent container to post shape tree instance");
        }

        log.debug("POST-ing shape tree instance to {}", parentContainer);
        log.debug ("Proposed name: {}", proposedResourceName == null ? "None provided" : proposedResourceName);
        log.debug ("Target Shape Tree: {}", targetShapeTree == null ? "None provided" : targetShapeTree.toString());
        log.debug("Focus Node: {}", focusNode == null ? "None provided" : focusNode.toString());

        HttpClient fetcher = AbstractHttpClientFactory.getFactory().get(this.useClientShapeTreeValidation);
        ResourceAttributes headers = getCommonHeaders(context, focusNode, targetShapeTree, isContainer, proposedResourceName, contentType);
        return fetcher.fetchShapeTreeResponse(new HttpRequest("POST", parentContainer, headers, bodyString, contentType));
    }

    // Create via HTTP PUT
    @Override
    public DocumentResponse putShapeTreeInstance(ShapeTreeContext context, URL resourceUrl, URL focusNode, URL targetShapeTree, Boolean isContainer, String bodyString, String contentType) throws ShapeTreeException {

        if (context == null || resourceUrl == null) {
            throw new ShapeTreeException(500, "Must provide a valid context and target resource to create shape tree instance via PUT");
        }

        log.debug("Creating shape tree instance via PUT at {}", resourceUrl);
        log.debug ("Target Shape Tree: {}", targetShapeTree == null ? "None provided" : targetShapeTree.toString());
        log.debug("Focus Node: {}", focusNode == null ? "None provided" : focusNode);

        HttpClient fetcher = AbstractHttpClientFactory.getFactory().get(this.useClientShapeTreeValidation);
        ResourceAttributes headers = getCommonHeaders(context, focusNode, targetShapeTree, isContainer,null, contentType);
        return fetcher.fetchShapeTreeResponse(new HttpRequest("PUT", resourceUrl, headers, bodyString, contentType));
    }

    // Update via HTTP PUT
    @Override
    public DocumentResponse putShapeTreeInstance(ShapeTreeContext context, URL resourceUrl, URL focusNode, String bodyString, String contentType) throws ShapeTreeException {

        if (context == null || resourceUrl == null) {
            throw new ShapeTreeException(500, "Must provide a valid context and target resource to update shape tree instance via PUT");
        }

        log.debug("Updating shape tree instance via PUT at {}", resourceUrl);
        log.debug("Focus Node: {}", focusNode == null ? "None provided" : focusNode);

        HttpClient fetcher = AbstractHttpClientFactory.getFactory().get(this.useClientShapeTreeValidation);
        ResourceAttributes headers = getCommonHeaders(context, focusNode, null, null, null, contentType);
        return fetcher.fetchShapeTreeResponse(new HttpRequest("PUT", resourceUrl, headers, bodyString, contentType));
    }

    @Override
    public DocumentResponse patchShapeTreeInstance(ShapeTreeContext context, URL resourceUrl, URL focusNode, String patchString) throws ShapeTreeException {

        if (context == null || resourceUrl == null || patchString == null) {
            throw new ShapeTreeException(500, "Must provide a valid context, target resource, and PATCH expression to PATCH shape tree instance");
        }

        log.debug("PATCH-ing shape tree instance at {}", resourceUrl);
        log.debug("PATCH String: {}", patchString);
        log.debug("Focus Node: {}", focusNode == null ? "None provided" : focusNode);

        String contentType = "application/sparql-update";

        HttpClient fetcher = AbstractHttpClientFactory.getFactory().get(this.useClientShapeTreeValidation);
        ResourceAttributes headers = getCommonHeaders(context, focusNode, null, null, null, contentType);
        return fetcher.fetchShapeTreeResponse(new HttpRequest("PATCH", resourceUrl, headers, patchString, contentType));
    }

    @Override
    public DocumentResponse deleteShapeTreeInstance(ShapeTreeContext context, URL resourceUrl) throws ShapeTreeException {

        if (context == null || resourceUrl == null) {
            throw new ShapeTreeException(500, "Must provide a valid context and target resource to DELETE shape tree instance");
        }

        log.debug("DELETE-ing shape tree instance at {}", resourceUrl);

        HttpClient fetcher = AbstractHttpClientFactory.getFactory().get(this.useClientShapeTreeValidation);
        ResourceAttributes headers = getCommonHeaders(context, null, null, null, null, null);
        return fetcher.fetchShapeTreeResponse(new HttpRequest("DELETE", resourceUrl, headers,null,null));
    }

    @Override
    public DocumentResponse unplantShapeTree(ShapeTreeContext context, URL targetResource, URL targetShapeTree) throws ShapeTreeException, MalformedURLException {

        if (context == null || targetResource == null || targetShapeTree == null) {
            throw new ShapeTreeException(500, "Must provide a valid context, target resource, and target shape tree to unplant");
        }

        log.debug("Unplanting shape tree {} managing {}: ", targetShapeTree, targetResource);

        // Lookup the target resource
        final HttpRemoteResourceAccessor resourceAccessor = new HttpRemoteResourceAccessor();
        ShapeTreeResource resource = new ShapeTreeResource(targetResource, resourceAccessor, context);
        ShapeTreeResource.Primary primaryResource = resource.getUserOwnedResourceFork();
        URL metadataUrl = primaryResource.getMetadataResourceUrl().orElseThrow( // politely handle no-metadata case before getMetadataResourceFork() throws less informatively
                () -> new IllegalStateException("No metadata resource for <" + primaryResource.getUrl() + ">")
        );

        if (Boolean.FALSE.equals(primaryResource.isExists())) {
            return new DocumentResponse(null, "Cannot find target resource to unplant: " + targetResource, 404);
        }

        // Determine whether the target resource is already a managed resource
        Optional<ShapeTreeLocator> discovered = discoverShapeTree(context, targetResource);
        if (discovered.isEmpty()) {
            return new DocumentResponse(null, "Cannot unplant target resource that is not managed by a shapetree: " + targetResource, 500);
        }
        ShapeTreeLocator locator = discovered.get();

        // Remove location from locator that corresponds with the provided shape tree
        locator.removeShapeTreeLocationForShapeTree(targetShapeTree);

        String method;
        String body;
        String contentType;
        if (locator.getLocations().isEmpty()) {
            method = "DELETE";
            body = null;
            contentType = null;
        } else {
            // Build an HTTP PUT request with the locator graph in turtle as the content body + link header
            method = "PUT";

            // Get a RDF version of the locator stored in a turtle string
            StringWriter sw = new StringWriter();
            RDFDataMgr.write(sw, locator.getGraph(), Lang.TURTLE);
            body = sw.toString();
            contentType = "text/turtle";
        }

        HttpClient fetcher = AbstractHttpClientFactory.getFactory().get(this.useClientShapeTreeValidation);
        return fetcher.fetchShapeTreeResponse(new HttpRequest(method, metadataUrl,
                                              null, // why no getCommonHeaders(context, null, null, null, null, null)
                                              body, contentType));
    }

    private ResourceAttributes getCommonHeaders(ShapeTreeContext context, URL focusNode, URL targetShapeTree, Boolean isContainer, String proposedResourceName, String contentType) {
        ResourceAttributes ret = new ResourceAttributes();

        if (context.getAuthorizationHeaderValue() != null) {
            ret.maybeSet(HttpHeaders.AUTHORIZATION.getValue(), context.getAuthorizationHeaderValue());
        }

        if (isContainer != null) {
            String resourceTypeUrl = Boolean.TRUE.equals(isContainer) ? "http://www.w3.org/ns/ldp#Container" : "http://www.w3.org/ns/ldp#Resource";
            ret.maybeSet(HttpHeaders.LINK.getValue(), "<" + resourceTypeUrl + ">; rel=\"type\"");
        }

        if (focusNode != null) {
            ret.maybeSet(HttpHeaders.LINK.getValue(), "<" + focusNode + ">; rel=\"" + LinkRelations.FOCUS_NODE.getValue() + "\"");
        }

        if (targetShapeTree != null) {
            ret.maybeSet(HttpHeaders.LINK.getValue(), "<" + targetShapeTree + ">; rel=\"" + LinkRelations.TARGET_SHAPETREE.getValue() + "\"");
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
