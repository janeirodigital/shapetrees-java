package com.janeirodigital.shapetrees.client.fetch;

import com.janeirodigital.shapetrees.client.core.ShapeTreeClient;
import com.janeirodigital.shapetrees.core.ShapeTreeResponse;
import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.core.enums.LinkRelations;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;
import com.janeirodigital.shapetrees.core.models.ShapeTreeLocation;
import com.janeirodigital.shapetrees.core.models.ShapeTreeLocator;
import lombok.extern.slf4j.Slf4j;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;

import java.io.IOException;
import java.io.StringWriter;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Slf4j
public class FetchShapeTreeClient implements ShapeTreeClient {

    private boolean skipValidation = false;
    private final ShapeTreeClientConfiguration validatingClientConfig;
    private final ShapeTreeClientConfiguration nonValidatingClientConfig;

    public FetchShapeTreeClient() {
        this.validatingClientConfig = new ShapeTreeClientConfiguration(true, false);
        this.nonValidatingClientConfig = new ShapeTreeClientConfiguration(false, false);
    }

    @Override
    public boolean isValidationSkipped() {
        return skipValidation;
    }

    @Override
    public void skipValidation(boolean skipValidation) {
        this.skipValidation = skipValidation;
    }

    /**
     * Discover the ShapeTreeLocator associated with a given target resource.
     * Implements {@link ShapeTreeClient#discoverShapeTree}
     *
     * Shape Trees, §4.1: This operation is used by a client-side agent to discover any shape trees associated
     * with a given resource. If URI is a managed resource, the associated Shape Tree Locator will be returned.
     * https://shapetrees.org/TR/specification/#discover
     *
     * @param context ShapeTreeContext that would be used for authentication purposes
     * @param targetResource The URI of the target resource for shape tree discovery
     * @return
     * @throws IOException
     */
    @Override
    public ShapeTreeLocator discoverShapeTree(ShapeTreeContext context, URI targetResource) throws IOException {

        if (targetResource == null) {
            throw new IOException("Must provide a value target resource for discovery");
        }

        log.debug("Discovering shape tree locator managing {}", targetResource);

        // Lookup the target resource for pointer to associated shape tree locator
        RemoteResource resource = new RemoteResource(targetResource, context.getAuthorizationHeaderValue());

        if  (Boolean.FALSE.equals(resource.exists())) {
            log.debug("Target resource for discovery {} does not exist", targetResource);
            return null;
        }

        // Lookup the associated shape tree locator resource based on the pointer
        RemoteResource locatorResource = resource.getMetadataResource(context.getAuthorizationHeaderValue());

        // Ensure the metadata resource exists
        // Shape Trees, §4.1: If LOCATORURI is empty, the resource at RESOURCEURI is not a managed resource,
        // and no shape tree locator will be returned.
        if (Boolean.FALSE.equals(locatorResource.exists())) {
            log.debug("Shape tree locator for {} does not exist", targetResource);
            return null;
        }

        // Populate a ShapeTreeLocator from the graph in locatorResource and return it
        return ShapeTreeLocator.getShapeTreeLocatorFromGraph(resource.getMetadataURI(),
                                                             locatorResource.getGraph(locatorResource.getUri()));

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
     * @param targetResource The URI of the resource to plant on
     * @param targetShapeTree A URI representing the shape tree to plant for targetResource
     * @param focusNode An optional URI representing the target subject within targetResource used for shape validation
     * @return The URI of the Shape Tree Locator that was planted for targetResource
     * @throws IOException
     * @throws URISyntaxException
     */
    @Override
    public ShapeTreeResponse plantShapeTree(ShapeTreeContext context, URI targetResource, URI targetShapeTree, URI focusNode) throws IOException, URISyntaxException {

        if (context == null || targetResource == null || targetShapeTree == null) {
            throw new IOException("Must provide a valid context, target resource, and target shape tree to the plant shape tree");
        }

        log.debug("Planting shape tree {} on {}: ", targetShapeTree, targetResource);
        log.debug("Focus node: {}", focusNode == null ? "None provided" : focusNode);

        // Lookup the target resource
        RemoteResource resource = new RemoteResource(targetResource, context.getAuthorizationHeaderValue());

        if (Boolean.FALSE.equals(resource.exists())) {
            return new ShapeTreeResponse(404, "Cannot find target resource to plant: " + targetResource, null);
        }

        // Determine whether the target resource is already a managed resource
        ShapeTreeLocator locator = discoverShapeTree(context, targetResource);

        // If the target resource is not managed, initialize a new locator
        if (locator == null) {
            locator = new ShapeTreeLocator(resource.getMetadataURI());
        }

        // Initialize a shape tree location based on the supplied parameters
        URI locationUri = locator.mintLocation();
        ShapeTreeLocation location = new ShapeTreeLocation(targetShapeTree.toString(),
                                                           targetResource.toString(),
                                                           locationUri,
                                                           focusNode == null ? null : focusNode.toString(),
                                                           null,
                                                           locationUri);

        // Add the location to the locator
        locator.addShapeTreeLocation(location);

        // Get an RDF version of the locator stored in a turtle string
        StringWriter sw = new StringWriter();
        RDFDataMgr.write(sw, locator.getGraph(), Lang.TURTLE);

        // Build an HTTP PUT request with the locator graph in turtle as the content body + link header
        OkHttpFetcher fetcher = OkHttpFetcher.getForConfig(getConfiguration(this.skipValidation));
        return fetcher.fetchShapeTreeResponse("PUT", new URI(resource.getMetadataURI()), null, context.getAuthorizationHeaderValue(), sw.toString(), "text/turtle");
    }

    @Override
    public ShapeTreeResponse postShapeTreeInstance(ShapeTreeContext context, URI parentContainer, URI focusNode, URI targetShapeTree, String proposedResourceName, Boolean isContainer, String bodyString, String contentType) throws IOException {

        if (context == null || parentContainer == null) {
            throw new IOException("Must provide a valid context and parent container to post shape tree instance");
        }

        log.debug("POST-ing shape tree instance to {}", parentContainer.toString());
        log.debug ("Proposed name: {}", proposedResourceName == null ? "None provided" : proposedResourceName);
        log.debug ("Target Shape Tree: {}", targetShapeTree == null ? "None provided" : targetShapeTree.toString());
        log.debug("Focus Node: {}", focusNode == null ? "None provided" : focusNode.toString());

        OkHttpFetcher fetcher = OkHttpFetcher.getForConfig(getConfiguration(this.skipValidation));
        return fetcher.fetchShapeTreeResponse("POST", parentContainer,
                                              getCommonHeaders(context, focusNode, targetShapeTree, isContainer,
                                                               proposedResourceName, contentType),
                                              null, bodyString,
                                              contentType);
    }

    // Create via HTTP PUT
    @Override
    public ShapeTreeResponse putShapeTreeInstance(ShapeTreeContext context, URI resourceURI, URI focusNode, URI targetShapeTree, Boolean isContainer, String bodyString, String contentType) throws IOException {

        if (context == null || resourceURI == null) {
            throw new IOException("Must provide a valid context and target resource to create shape tree instance via PUT");
        }

        log.debug("Creating shape tree instance via PUT at {}", resourceURI);
        log.debug ("Target Shape Tree: {}", targetShapeTree == null ? "None provided" : targetShapeTree.toString());
        log.debug("Focus Node: {}", focusNode == null ? "None provided" : focusNode);

        OkHttpFetcher fetcher = OkHttpFetcher.getForConfig(getConfiguration(this.skipValidation));
        return fetcher.fetchShapeTreeResponse("PUT", resourceURI,
                                              getCommonHeaders(context, focusNode, targetShapeTree, isContainer,
                                                               null, contentType),
                                              null, bodyString,
                                              contentType);
    }

    // Update via HTTP PUT
    @Override
    public ShapeTreeResponse putShapeTreeInstance(ShapeTreeContext context, URI resourceURI, URI focusNode, String bodyString, String contentType) throws IOException {

        if (context == null || resourceURI == null) {
            throw new IOException("Must provide a valid context and target resource to update shape tree instance via PUT");
        }

        log.debug("Updating shape tree instance via PUT at {}", resourceURI);
        log.debug("Focus Node: {}", focusNode == null ? "None provided" : focusNode);

        OkHttpFetcher fetcher = OkHttpFetcher.getForConfig(getConfiguration(this.skipValidation));
        return fetcher.fetchShapeTreeResponse("PUT", resourceURI,
                                              getCommonHeaders(context, focusNode, null, null, null, contentType),
                                              null, bodyString,
                                              contentType);
    }

    @Override
    public ShapeTreeResponse patchShapeTreeInstance(ShapeTreeContext context, URI resourceURI, URI focusNode, String patchString) throws IOException {

        if (context == null || resourceURI == null || patchString == null) {
            throw new IOException("Must provide a valid context, target resource, and PATCH expression to PATCH shape tree instance");
        }

        log.debug("PATCH-ing shape tree instance at {}", resourceURI);
        log.debug("PATCH String: {}", patchString);
        log.debug("Focus Node: {}", focusNode == null ? "None provided" : focusNode);

        String contentType = "application/sparql-update";

        OkHttpFetcher fetcher = OkHttpFetcher.getForConfig(getConfiguration(this.skipValidation));
        return fetcher.fetchShapeTreeResponse("PATCH", resourceURI,
                                              getCommonHeaders(context, focusNode, null, null, null, contentType),
                                              null, patchString,
                                              contentType);
    }

    @Override
    public ShapeTreeResponse deleteShapeTreeInstance(ShapeTreeContext context, URI resourceURI) throws IOException {

        if (context == null || resourceURI == null) {
            throw new IOException("Must provide a valid context and target resource to DELETE shape tree instance");
        }

        log.debug("DELETE-ing shape tree instance at {}", resourceURI);

        OkHttpFetcher fetcher = OkHttpFetcher.getForConfig(getConfiguration(this.skipValidation));
        return fetcher.fetchShapeTreeResponse("DELETE", resourceURI,
                                              getCommonHeaders(context, null, null, null, null, null),
                                              null, null,
                                              null);
    }

    @Override
    public ShapeTreeResponse unplantShapeTree(ShapeTreeContext context, URI targetResource, URI targetShapeTree) throws IOException, URISyntaxException {

        if (context == null || targetResource == null || targetShapeTree == null) {
            throw new IOException("Must provide a valid context, target resource, and target shape tree to unplant");
        }

        log.debug("Unplanting shape tree {} managing {}: ", targetShapeTree, targetResource);

        // Lookup the target resource
        RemoteResource resource = new RemoteResource(targetResource, context.getAuthorizationHeaderValue());

        if (Boolean.FALSE.equals(resource.exists())) {
            return new ShapeTreeResponse(404, "Cannot find target resource to unplant: " + targetResource.toString(), null);
        }

        // Determine whether the target resource is already a managed resource
        ShapeTreeLocator locator = discoverShapeTree(context, targetResource);

        // If the target resource is not managed, initialize a new locator
        if (locator == null) {
            return new ShapeTreeResponse(500, "Cannot unplant target resource that is not managed by a shapetree: " + targetResource.toString(), null);
        }

        // Remove location from locator that corresponds with the provided shape tree
        locator.removeShapeTreeLocationForShapeTree(targetShapeTree);

        String method, body, contentType;
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

        OkHttpFetcher fetcher = OkHttpFetcher.getForConfig(getConfiguration(this.skipValidation));
        return fetcher.fetchShapeTreeResponse(method, new URI(resource.getMetadataURI()),
                                              null, // why no getCommonHeaders(context, null, null, null, null, null) ?
                                              null, body, contentType);
    }

    private ShapeTreeClientConfiguration getConfiguration(boolean skipValidation) {
        if (skipValidation) {
            return this.nonValidatingClientConfig;
        } else {
            return this.validatingClientConfig;
        }
    }

    private class Headers extends HashMap<String, List<String>> {
        private void set(String attr, String value) {
            if (this.containsKey(attr)) {
                this.get(attr).add(value);
            } else {
                ArrayList<String> list = new ArrayList<String>();
                list.add(value);
                this.put(attr, list);
            }
        }
    }

    private Map<String, List<String>> getCommonHeaders(ShapeTreeContext context, URI focusNode, URI targetShapeTree, Boolean isContainer, String proposedResourceName, String contentType) {
        Headers ret = new Headers();

        if (context.getAuthorizationHeaderValue() != null) {
            ret.set(HttpHeaders.AUTHORIZATION.getValue(), context.getAuthorizationHeaderValue());
        }

        if (isContainer != null) {
            String resourceTypeUri = Boolean.TRUE.equals(isContainer) ? "http://www.w3.org/ns/ldp#Container" : "http://www.w3.org/ns/ldp#Resource";
            ret.set(HttpHeaders.LINK.getValue(), "<" + resourceTypeUri + ">; rel=\"type\"");
        }

        if (focusNode != null) {
            ret.set(HttpHeaders.LINK.getValue(), "<" + focusNode + ">; rel=\"" + LinkRelations.FOCUS_NODE.getValue() + "\"");
        }

        if (targetShapeTree != null) {
            ret.set(HttpHeaders.LINK.getValue(), "<" + targetShapeTree + ">; rel=\"" + LinkRelations.TARGET_SHAPETREE.getValue() + "\"");
        }

        if (proposedResourceName != null) {
            ret.set(HttpHeaders.SLUG.getValue(), proposedResourceName);
        }

        if (contentType != null) {
            ret.set(HttpHeaders.CONTENT_TYPE.getValue(), contentType);
        }

        return ret;
    }
}
