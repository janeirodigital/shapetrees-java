package com.janeirodigital.shapetrees.client.okhttp;

import com.janeirodigital.shapetrees.client.core.ShapeTreeClient;
import com.janeirodigital.shapetrees.core.ShapeTreeResponse;
import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.core.enums.LinkRelations;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;
import com.janeirodigital.shapetrees.core.models.ShapeTreeLocation;
import com.janeirodigital.shapetrees.core.models.ShapeTreeLocator;
import lombok.extern.slf4j.Slf4j;
import okhttp3.*;
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

        log.debug("Discovering Shape Trees present at {}", targetResource);

        // Lookup the target resource for pointer to associated shape tree locator
        RemoteResource resource = new RemoteResource(targetResource, context.getAuthorizationHeaderValue());

        // Lookup the associated shape tree locator resource based on the pointer
        RemoteResource locatorResource = resource.getMetadataResource(context.getAuthorizationHeaderValue());

        // Ensure the metadata resource exists
        // Shape Trees, §4.1: If LOCATORURI is empty, the resource at RESOURCEURI is not a managed resource,
        // and no shape tree locator will be returned.
        if (!locatorResource.exists()) { return null; }

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
     * @param recursive An optional flag specifying a recursive plant over an existing hierarchy of resources. False by default.
     * @return The URI of the Shape Tree Locator that was planted for targetResource
     * @throws IOException
     * @throws URISyntaxException
     */
    @Override
    public URI plantShapeTree(ShapeTreeContext context, URI targetResource, URI targetShapeTree, String focusNode, Boolean recursive) throws IOException, URISyntaxException {

        // Determine whether the target resource is already a managed resource
        ShapeTreeLocator locator = discoverShapeTree(context, targetResource);

        // Initialize a shape tree location based on the supplied parameters
        ShapeTreeLocation location = new ShapeTreeLocation(targetShapeTree.toString(),
                                                           targetShapeTree.toString(),
                                                           targetResource.toString(),
                                                           focusNode);

        // If the target resource is not managed, initialize a new locator
        if (locator == null) {
            // Lookup the target resource for pointer to associated shape tree locator
            RemoteResource resource = new RemoteResource(targetResource, context.getAuthorizationHeaderValue());
            locator = new ShapeTreeLocator(resource.getUri().toString(), null);
        }

        // Add the location to the locator
        locator.getLocations().add(location);

        // Get an OkHttpClient to use to update locator metadata
        OkHttpClient client = ShapeTreeHttpClientHolder.getForConfig(getConfiguration(this.skipValidation));

        Request.Builder builder = new Request.Builder()
                .url(targetResource.toString());

        // Determine which HTTP Link header type to send based on the recursive setting
        String linkType = Boolean.TRUE.equals(recursive) ? LinkRelations.PLANT_SHAPETREE_HIERARCHY.getValue() :
                                                           LinkRelations.PLANT_SHAPETREE.getValue();
        builder.addHeader(HttpHeaders.LINK.getValue(), "<" + linkType + ">; rel=\"type\"");

        // Get a RDF version of the locator stored in a turtle string
        StringWriter sw = new StringWriter();
        RDFDataMgr.write(sw, locator.getGraph(), Lang.TURTLE);

        // Convert the string into a byte array. Turtle is always UTF-8 https://www.w3.org/TR/turtle/#h3_sec-mime
        byte[] bytes = sw.toString().getBytes("UTF-8");

        // Build an HTTP PUT request with the locator graph in turtle as the content body + link header
        Request plantPost = builder
                .put(RequestBody.create(bytes))
                .build();

        // Send the request
        Response response = client.newCall(plantPost).execute();

        // SUCCESS - Locator was created or updated with new location
        if (response.isSuccessful()) {

            String locationHeader = response.header(HttpHeaders.LOCATION.getValue());
            if (locationHeader != null) {
                return new URI(locationHeader);
            } else {
                throw new IOException(response.code() + " No Location Header provided");
            }
        } else {
            // FAILURE - Pass along the response code and response body
            String responseBodyString = null;
            try (ResponseBody body = response.body()) {
                if (body != null) {
                    responseBodyString = body.string();
                }
            }
            throw new IOException(response.code() + " " + responseBodyString);
        }
    }

    // TODO - make createDataInstanceWithPut and createDataInstanceWithPost client routines

    @Override
    public ShapeTreeResponse createDataInstance(ShapeTreeContext context, URI parentContainer, String focusNode, URI shapeTreeHint, String proposedResourceName, Boolean isContainer, String bodyString, String contentType) throws IOException {
        log.debug("Creating data instance {} in {} with hint {}", parentContainer, proposedResourceName, shapeTreeHint);
        OkHttpClient client = ShapeTreeHttpClientHolder.getForConfig(getConfiguration(this.skipValidation));

        byte[] bytes = new byte[]{};
        if (bodyString != null) {
            bytes = bodyString.getBytes();
        }

        String resourceURI = parentContainer.toString();
        if (!resourceURI.endsWith("/")) {
            resourceURI += "/";
        }
        resourceURI += proposedResourceName;
        log.debug("Build Resource URI {}", resourceURI);

        Request.Builder putBuilder = new Request.Builder()
                .url(resourceURI)
                .put(RequestBody.create(bytes));

        // proposed resource is name is nulled since a Slug will not be used
        applyCommonHeaders(context, putBuilder, focusNode, shapeTreeHint, isContainer, null, contentType);

        return OkHttpHelper.mapOkHttpResponseToShapeTreeResponse(client.newCall(putBuilder.build()).execute());
    }

    @Override
    public ShapeTreeResponse updateDataInstance(ShapeTreeContext context, URI resourceURI, String focusNode, URI shapeTreeHint, String bodyString, String contentType) throws IOException {
        OkHttpClient client = ShapeTreeHttpClientHolder.getForConfig(getConfiguration(this.skipValidation));

        byte[] bytes = new byte[]{};
        if (bodyString != null) {
            bytes = bodyString.getBytes();
        }

        Request.Builder putBuilder = new Request.Builder()
                .url(resourceURI.toString())
                .put(RequestBody.create(bytes));

        applyCommonHeaders(context, putBuilder, focusNode, shapeTreeHint, null, null, contentType);

        return OkHttpHelper.mapOkHttpResponseToShapeTreeResponse(client.newCall(putBuilder.build()).execute());
    }

    @Override
    public ShapeTreeResponse updateDataInstanceWithPatch(ShapeTreeContext context, URI resourceURI, String focusNode, URI shapeTreeHint, String bodyString) throws IOException {
        OkHttpClient client = ShapeTreeHttpClientHolder.getForConfig(getConfiguration(this.skipValidation));
        String contentType = "application/sparql-update";
        byte[] sparqlUpdateBytes = bodyString.getBytes();

        Request.Builder patchBuilder = new Request.Builder()
                .url(resourceURI.toString())
                .patch(RequestBody.create(sparqlUpdateBytes));

        applyCommonHeaders(context, patchBuilder, focusNode, shapeTreeHint, null, null, contentType);

        return OkHttpHelper.mapOkHttpResponseToShapeTreeResponse(client.newCall(patchBuilder.build()).execute());
    }

    @Override
    public ShapeTreeResponse deleteDataInstance(ShapeTreeContext context, URI resourceURI, URI shapeTreeURI) throws IOException {
        OkHttpClient client = ShapeTreeHttpClientHolder.getForConfig(getConfiguration(this.skipValidation));

        Request.Builder deleteBuilder = new Request.Builder()
                .url(resourceURI.toString())
                .delete();

        applyCommonHeaders(context, deleteBuilder, null, shapeTreeURI, null, null, null);

        return OkHttpHelper.mapOkHttpResponseToShapeTreeResponse(client.newCall(deleteBuilder.build()).execute());
    }

    @Override
    public void unplantShapeTree(ShapeTreeContext context, URI containerURI, URI shapeTreeURI) {

    }

    private ShapeTreeClientConfiguration getConfiguration(boolean skipValidation) {
        if (skipValidation) {
            return this.nonValidatingClientConfig;
        } else {
            return this.validatingClientConfig;
        }
    }

    private void applyCommonHeaders(ShapeTreeContext context, Request.Builder builder,  String focusNode, URI shapeTreeHint, Boolean isContainer, String proposedResourceName, String contentType) {

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

        if (shapeTreeHint != null) {
            builder.addHeader(HttpHeaders.LINK.getValue(), "<" + shapeTreeHint + ">; rel=\"" + LinkRelations.TARGET_SHAPETREE.getValue() + "\"");
        }

        if (proposedResourceName != null) {
            builder.addHeader(HttpHeaders.SLUG.getValue(), proposedResourceName);
        }

        if (contentType != null) {
            builder.addHeader(HttpHeaders.CONTENT_TYPE.getValue(), contentType);
        }
    }
}
