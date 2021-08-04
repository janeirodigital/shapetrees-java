package com.janeirodigital.shapetrees.client.okhttp;

import com.janeirodigital.shapetrees.client.core.ShapeTreeClient;
import com.janeirodigital.shapetrees.core.ShapeTreeResponse;
import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.core.enums.LinkRelations;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;
import com.janeirodigital.shapetrees.core.models.ShapeTreeLocation;
import com.janeirodigital.shapetrees.core.models.ShapeTreeLocator;
import lombok.extern.slf4j.Slf4j;
import okhttp3.HttpUrl;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.RequestBody;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;

import java.io.IOException;
import java.io.StringWriter;
import java.net.URI;
import java.net.URISyntaxException;

@Slf4j
public class OkHttpShapeTreeClient implements ShapeTreeClient {

    private boolean skipValidation = false;
    private final ShapeTreeClientConfiguration validatingClientConfig;
    private final ShapeTreeClientConfiguration nonValidatingClientConfig;

    public OkHttpShapeTreeClient() {
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

        if  (!resource.exists()) {
            log.debug("Target resource for discovery {} does not exist", targetResource);
            return null;
        }

        // Lookup the associated shape tree locator resource based on the pointer
        RemoteResource locatorResource = resource.getMetadataResource(context.getAuthorizationHeaderValue());

        // Ensure the metadata resource exists
        // Shape Trees, §4.1: If LOCATORURI is empty, the resource at RESOURCEURI is not a managed resource,
        // and no shape tree locator will be returned.
        if (!locatorResource.exists()) {
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
    public ShapeTreeResponse plantShapeTree(ShapeTreeContext context, URI targetResource, URI targetShapeTree, String focusNode) throws IOException, URISyntaxException {

        if (context == null || targetResource == null || targetShapeTree == null) {
            throw new IOException("Must provide a valid context, target resource, and target shape tree to the plant shape tree");
        }

        log.debug("Planting shape tree {} on {}: ", targetShapeTree, targetResource);
        log.debug("Focus node: {}", focusNode == null ? "None provided" : focusNode);

        // Lookup the target resource
        RemoteResource resource = new RemoteResource(targetResource, context.getAuthorizationHeaderValue());

        if (!resource.exists()) {
            return new ShapeTreeResponse(404, "Cannot find target resource to plant: " + targetResource.toString(), null);
        }

        // Determine whether the target resource is already a managed resource
        ShapeTreeLocator locator = discoverShapeTree(context, targetResource);

        // If the target resource is not managed, initialize a new locator
        if (locator == null) {
            locator = new ShapeTreeLocator(resource.getMetadataURI().toString());
        }

        // Initialize a shape tree location based on the supplied parameters
        URI locationUri = locator.mintLocation();
        ShapeTreeLocation location = new ShapeTreeLocation(targetShapeTree.toString(),
                                                           targetResource.toString(),
                                                           locationUri,
                                                           focusNode,
                                                           null,
                                                           locationUri);

        // Add the location to the locator
        locator.addShapeTreeLocation(location);

        // Get an OkHttpClient to use to update locator metadata
        OkHttpClient client = ShapeTreeHttpClientHolder.getForConfig(getConfiguration(this.skipValidation));

        Request.Builder builder = new Request.Builder().url(resource.getMetadataURI());

        // Get a RDF version of the locator stored in a turtle string
        StringWriter sw = new StringWriter();
        RDFDataMgr.write(sw, locator.getGraph(), Lang.TURTLE);

        // Convert the string into a byte array. Turtle is always UTF-8 https://www.w3.org/TR/turtle/#h3_sec-mime
        byte[] bytes = sw.toString().getBytes("UTF-8");

        // Build an HTTP PUT request with the locator graph in turtle as the content body + link header
        Request plantBuilder = builder.put(RequestBody.create(bytes)).build();

        // Send the request and map the response
        return OkHttpHelper.mapOkHttpResponseToShapeTreeResponse(client.newCall(plantBuilder).execute());
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

        OkHttpClient client = ShapeTreeHttpClientHolder.getForConfig(getConfiguration(this.skipValidation));

        byte[] bytes = new byte[]{};
        if (bodyString != null) {
            bytes = bodyString.getBytes();
        }

        Request.Builder postBuilder = new Request.Builder().url(HttpUrl.get(parentContainer)).post(RequestBody.create(bytes));

        applyCommonHeaders(context, postBuilder, focusNode, targetShapeTree, isContainer, proposedResourceName, contentType);

        return OkHttpHelper.mapOkHttpResponseToShapeTreeResponse(client.newCall(postBuilder.build()).execute());
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

        OkHttpClient client = ShapeTreeHttpClientHolder.getForConfig(getConfiguration(this.skipValidation));

        byte[] bytes = new byte[]{};
        if (bodyString != null) {
            bytes = bodyString.getBytes();
        }

        Request.Builder putBuilder = new Request.Builder().url(resourceURI.toString()).put(RequestBody.create(bytes));

        applyCommonHeaders(context, putBuilder, focusNode, targetShapeTree, isContainer, null, contentType);

        return OkHttpHelper.mapOkHttpResponseToShapeTreeResponse(client.newCall(putBuilder.build()).execute());
    }

    // Update via HTTP PUT
    @Override
    public ShapeTreeResponse putShapeTreeInstance(ShapeTreeContext context, URI resourceURI, URI focusNode, String bodyString, String contentType) throws IOException {

        if (context == null || resourceURI == null) {
            throw new IOException("Must provide a valid context and target resource to update shape tree instance via PUT");
        }

        log.debug("Updating shape tree instance via PUT at {}", resourceURI);
        log.debug("Focus Node: {}", focusNode == null ? "None provided" : focusNode);

        OkHttpClient client = ShapeTreeHttpClientHolder.getForConfig(getConfiguration(this.skipValidation));

        byte[] bytes = new byte[]{};
        if (bodyString != null) {
            bytes = bodyString.getBytes();
        }

        Request.Builder putBuilder = new Request.Builder().url(resourceURI.toString()).put(RequestBody.create(bytes));

        applyCommonHeaders(context, putBuilder, focusNode, null, null, null, contentType);

        return OkHttpHelper.mapOkHttpResponseToShapeTreeResponse(client.newCall(putBuilder.build()).execute());
    }

    @Override
    public ShapeTreeResponse patchShapeTreeInstance(ShapeTreeContext context, URI resourceURI, URI focusNode, String patchString) throws IOException {

        if (context == null || resourceURI == null || patchString == null) {
            throw new IOException("Must provide a valid context, target resource, and PATCH expression to PATCH shape tree instance");
        }

        log.debug("PATCH-ing shape tree instance at {}", resourceURI);
        log.debug("PATCH String: {}", patchString);
        log.debug("Focus Node: {}", focusNode == null ? "None provided" : focusNode);

        OkHttpClient client = ShapeTreeHttpClientHolder.getForConfig(getConfiguration(this.skipValidation));
        String contentType = "application/sparql-update";
        byte[] sparqlUpdateBytes = patchString.getBytes();

        Request.Builder patchBuilder = new Request.Builder().url(resourceURI.toString()).patch(RequestBody.create(sparqlUpdateBytes));

        applyCommonHeaders(context, patchBuilder, focusNode, null, null, null, contentType);

        return OkHttpHelper.mapOkHttpResponseToShapeTreeResponse(client.newCall(patchBuilder.build()).execute());
    }

    @Override
    public ShapeTreeResponse deleteShapeTreeInstance(ShapeTreeContext context, URI resourceURI) throws IOException {

        if (context == null || resourceURI == null) {
            throw new IOException("Must provide a valid context and target resource to DELETE shape tree instance");
        }

        log.debug("DELETE-ing shape tree instance at {}", resourceURI);

        OkHttpClient client = ShapeTreeHttpClientHolder.getForConfig(getConfiguration(this.skipValidation));

        Request.Builder deleteBuilder = new Request.Builder().url(resourceURI.toString()).delete();

        applyCommonHeaders(context, deleteBuilder, null, null, null, null, null);

        return OkHttpHelper.mapOkHttpResponseToShapeTreeResponse(client.newCall(deleteBuilder.build()).execute());
    }

    @Override
    public ShapeTreeResponse unplantShapeTree(ShapeTreeContext context, URI targetResource, URI targetShapeTree) throws IOException, URISyntaxException {

        if (context == null || targetResource == null || targetShapeTree == null) {
            throw new IOException("Must provide a valid context, target resource, and target shape tree to unplant");
        }

        log.debug("Unplanting shape tree {} managing {}: ", targetShapeTree, targetResource);

        // Lookup the target resource
        RemoteResource resource = new RemoteResource(targetResource, context.getAuthorizationHeaderValue());

        if (!resource.exists()) {
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

        // Get an OkHttpClient to use to update locator metadata
        OkHttpClient client = ShapeTreeHttpClientHolder.getForConfig(getConfiguration(this.skipValidation));

        Request unplantBuilder = null;

        if (locator.getLocations().isEmpty()) {

            unplantBuilder = new Request.Builder().url(resource.getMetadataURI()).delete().build();

        } else {

            // Get a RDF version of the locator stored in a turtle string
            StringWriter sw = new StringWriter();
            RDFDataMgr.write(sw, locator.getGraph(), Lang.TURTLE);
            // Convert the string into a byte array. Turtle is always UTF-8 https://www.w3.org/TR/turtle/#h3_sec-mime
            byte[] bytes = sw.toString().getBytes("UTF-8");
            // Build an HTTP PUT request with the locator graph in turtle as the content body + link header
            unplantBuilder = new Request.Builder().url(resource.getMetadataURI()).put(RequestBody.create(bytes)).build();

        }

        // Send the request and map the response
        return OkHttpHelper.mapOkHttpResponseToShapeTreeResponse(client.newCall(unplantBuilder).execute());

    }

    private ShapeTreeClientConfiguration getConfiguration(boolean skipValidation) {
        if (skipValidation) {
            return this.nonValidatingClientConfig;
        } else {
            return this.validatingClientConfig;
        }
    }

    private void applyCommonHeaders(ShapeTreeContext context, Request.Builder builder, URI focusNode, URI targetShapeTree, Boolean isContainer, String proposedResourceName, String contentType) {

        if (context.getAuthorizationHeaderValue() != null) {
            builder.addHeader(HttpHeaders.AUTHORIZATION.getValue(), context.getAuthorizationHeaderValue());
        }

        if (isContainer != null) {
            String resourceTypeUri = Boolean.TRUE.equals(isContainer) ? "http://www.w3.org/ns/ldp#Container" : "http://www.w3.org/ns/ldp#Resource";
            builder.addHeader(HttpHeaders.LINK.getValue(), "<" + resourceTypeUri + ">; rel=\"type\"");
        }

        if (focusNode != null) {
            builder.addHeader(HttpHeaders.LINK.getValue(), "<" + focusNode + ">; rel=\"" + LinkRelations.FOCUS_NODE.getValue() + "\"");
        }

        if (targetShapeTree != null) {
            builder.addHeader(HttpHeaders.LINK.getValue(), "<" + targetShapeTree + ">; rel=\"" + LinkRelations.TARGET_SHAPETREE.getValue() + "\"");
        }

        if (proposedResourceName != null) {
            builder.addHeader(HttpHeaders.SLUG.getValue(), proposedResourceName);
        }

        if (contentType != null) {
            builder.addHeader(HttpHeaders.CONTENT_TYPE.getValue(), contentType);
        }
    }
}
