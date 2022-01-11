package com.janeirodigital.shapetrees.client.okhttp;

import com.janeirodigital.shapetrees.core.*;
import com.janeirodigital.shapetrees.core.enums.ContentType;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import lombok.extern.slf4j.Slf4j;
import okhttp3.*;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;

import java.io.StringWriter;
import java.net.URL;
import java.util.List;
import java.util.Objects;

import static com.janeirodigital.shapetrees.core.ManageableInstance.getInstance;
import static com.janeirodigital.shapetrees.core.enums.ContentType.SPARQL_UPDATE;
import static com.janeirodigital.shapetrees.core.enums.ContentType.TEXT_TURTLE;
import static com.janeirodigital.shapetrees.core.enums.HttpHeader.CONTENT_TYPE;
import static com.janeirodigital.shapetrees.core.enums.HttpHeader.SLUG;
import static com.janeirodigital.shapetrees.core.enums.LinkRelation.*;
import static com.janeirodigital.shapetrees.core.vocabularies.LdpVocabulary.BASIC_CONTAINER;
import static com.janeirodigital.shapetrees.client.okhttp.OkHttpHelper.*;

@Slf4j
public class OkHttpShapeTreeClient {

    private OkHttpShapeTreeClient() { }

    /**
     * Discover the ShapeTreeManager associated with a given target resource.
     * @param okHttpClient OkHttp client to use for requests
     * @param context ShapeTreeContext used for authorization
     * @param resourceUrl URL of the target resource for shape tree discovery
     * @return ShapeTreeManager if it is a managed resource, or null if it is not managed
     * @throws ShapeTreeException
     */
    public static ShapeTreeManager discover(OkHttpClient okHttpClient, ShapeTreeContext context, URL resourceUrl) throws ShapeTreeException {
        Objects.requireNonNull(okHttpClient, "Must provide a valid HTTP client for shape tree discovery");
        Objects.requireNonNull(resourceUrl, "Must provide a resource target for shape tree discovery");
        Objects.requireNonNull(context, "Must provide a valid shape tree context for shape tree discovery");
        log.debug("Discovering shape tree manager managing {}", resourceUrl);
        final OkHttpResourceAccessor accessor = new OkHttpResourceAccessor();
        ManageableInstance instance = getInstance(accessor, context, resourceUrl);
        ManageableResource manageableResource = instance.getManageableResource();

        if  (!manageableResource.isExists()) {
            log.debug("Target resource for discovery {} does not exist", resourceUrl);
            return null;
        }

        if (instance.wasRequestForManager()) {
            throw new ShapeTreeException(500, "Discovery target must not be a shape tree manager resource");
        }

        if (instance.isUnmanaged()) {
            log.debug("Resource {} is not managed by a shape tree", resourceUrl);
            return null;
        }

        log.debug("Resource {} is managed by a shape tree: {}", resourceUrl, instance.getManagerResource().getManager().toString());
        return instance.getManagerResource().getManager();
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
     *  https://shapetrees.org/TR/specification/#plant-shapetree
     * @param okHttpClient OkHttp client to use for requests
     * @param context ShapeTreeContext used for authorization
     * @param resourceUrl URL of the target resource to assign a shape tree to
     * @param shapeTreeUrl URL of the shape tree to assign
     * @param focusNodeUrl Optional URL of the focus node for shape validation
     * @return OkHttp Response
     * @throws ShapeTreeException
     */
    public static Response plant(OkHttpClient okHttpClient, ShapeTreeContext context, URL resourceUrl, URL shapeTreeUrl, URL focusNodeUrl) throws ShapeTreeException {
        Objects.requireNonNull(okHttpClient, "Must provide a valid HTTP client to plant the shape tree with");
        Objects.requireNonNull(context, "Must provide a valid shape tree context to plant the shape tree with");
        Objects.requireNonNull(resourceUrl, "Must provide a target resource to plant the shape tree on");
        Objects.requireNonNull(shapeTreeUrl, "Must provide a shape tree to plant");

        log.debug("Planting shape tree {} on {}: ", shapeTreeUrl, resourceUrl);
        log.debug("Focus node: {}", focusNodeUrl == null ? "None provided" : focusNodeUrl);

        final OkHttpResourceAccessor accessor = new OkHttpResourceAccessor();
        // Lookup the shape tree
        ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(shapeTreeUrl);
        // Lookup the target resource
        ManageableInstance instance = getInstance(accessor, context, resourceUrl);
        ManageableResource manageableResource = instance.getManageableResource();

        if (!manageableResource.isExists()) {
            log.error("Cannot find target resource to plant: " + resourceUrl);
            return getErrorResponse(manageableResource.getUrl(), "GET", 404, "Not Found");
        }

        ShapeTreeManager manager;
        URL managerResourceUrl = instance.getManagerResource().getUrl();
        if (instance.isManaged()) {
            manager = instance.getManagerResource().getManager();
            log.debug("Resource {} is already managed", resourceUrl);
        } else {
            manager = new ShapeTreeManager(managerResourceUrl);
            log.debug("Resource {} is not currently managed by a shape tree", resourceUrl);
        }

        // Initialize a shape tree assignment based on the supplied parameters and add it to the manager
        URL assignmentUrl = manager.mintAssignmentUrl();
        ShapeTreeAssignment assignment = new ShapeTreeAssignment(shapeTreeUrl, resourceUrl, assignmentUrl, focusNodeUrl, shapeTree.getShape(), assignmentUrl);
        manager.addAssignment(assignment);

        // Get an RDF version of the manager stored in a turtle string
        StringWriter sw = new StringWriter();
        RDFDataMgr.write(sw, manager.getGraph(), Lang.TURTLE);

        log.debug("Updating shape tree manager resource at {}", manager.getId());

        return putHttpResource(okHttpClient, managerResourceUrl, context.getCredentials(), null, sw.toString(), TEXT_TURTLE);
    }

    public static Response unplant(OkHttpClient okHttpClient, ShapeTreeContext context, URL resourceUrl, URL shapeTreeUrl) throws ShapeTreeException {

        Objects.requireNonNull(okHttpClient, "Must provide a valid HTTP client to plant the shape tree with");
        Objects.requireNonNull(context, "Must provide a valid shape tree context to plant the shape tree with");
        Objects.requireNonNull(resourceUrl, "Must provide a target resource to plant the shape tree on");
        Objects.requireNonNull(shapeTreeUrl, "Must provide a shape tree to plant");

        log.debug("Unplanting shape tree {} managing {}: ", shapeTreeUrl, resourceUrl);

        // Lookup the target resource
        final OkHttpResourceAccessor accessor = new OkHttpResourceAccessor();
        ManageableInstance instance = getInstance(accessor, context, resourceUrl);
        ManageableResource manageableResource = instance.getManageableResource();

        if (!manageableResource.isExists()) {
            log.error("Cannot find target resource to plant: " + resourceUrl);
            return getErrorResponse(manageableResource.getUrl(), "GET", 404, "Not Found");
        }

        if (instance.isUnmanaged()) {
            throw new ShapeTreeException(500, "Cannot unplant target resource that is not managed by a shapetree: " + resourceUrl);
        }

        // Remove assignment from manager that corresponds with the provided shape tree
        ShapeTreeManager manager = instance.getManagerResource().getManager();
        manager.removeAssignmentForShapeTree(shapeTreeUrl);

        Response response;
        if (manager.getAssignments().isEmpty()) {
            response = deleteHttpResource(okHttpClient, manager.getId(), context.getCredentials());
        } else {
            StringWriter sw = new StringWriter();
            RDFDataMgr.write(sw, manager.getGraph(), Lang.TURTLE);
            String body = sw.toString();
            response = putHttpResource(okHttpClient, manager.getId(), null, body, TEXT_TURTLE);
        }
        return response;
    }

    public static Response post(OkHttpClient okHttpClient, ShapeTreeContext context, URL parentContainer, List<URL> focusNodes, List<URL> targetShapeTrees, String slug, boolean isContainer, String body, ContentType contentType) throws ShapeTreeException {
        Objects.requireNonNull(okHttpClient, "Must provide a valid HTTP client to POST with");
        Objects.requireNonNull(context, "Must provide a valid shape tree context to POST with");
        Objects.requireNonNull(parentContainer, "Must provide a parent container to POST into");

        log.debug("POST-ing shape tree instance to {}", parentContainer);
        log.debug ("Proposed name: {}", slug == null ? "None provided" : slug);
        log.debug ("Target Shape Tree: {}", targetShapeTrees == null || targetShapeTrees.isEmpty()  ? "None provided" : targetShapeTrees.toString());
        log.debug("Focus Node: {}", focusNodes == null || focusNodes.isEmpty() ? "None provided" : focusNodes.toString());

        Headers headers = setRequestHeaders(focusNodes, targetShapeTrees, isContainer, slug, contentType);

        return postHttpResource(okHttpClient, parentContainer, context.getCredentials(), headers, body, contentType);
    }

    public static Response put(OkHttpClient okHttpClient, ShapeTreeContext context, URL resourceUrl, List<URL> focusNodes, List<URL> targetShapeTrees, Boolean isContainer, String body, ContentType contentType) throws ShapeTreeException {
        Objects.requireNonNull(okHttpClient, "Must provide a valid HTTP client to PUT with");
        Objects.requireNonNull(context, "Must provide a valid shape tree context to PUT with");
        Objects.requireNonNull(resourceUrl, "Must provide a resource URL to PUT to");

        log.debug("Creating shape tree instance via PUT at {}", resourceUrl);
        log.debug ("Target Shape Tree: {}", targetShapeTrees == null || targetShapeTrees.isEmpty()  ? "None provided" : targetShapeTrees.toString());
        log.debug("Focus Node: {}", focusNodes == null || focusNodes.isEmpty() ? "None provided" : focusNodes.toString());

        Headers headers = setRequestHeaders(focusNodes, targetShapeTrees, isContainer, null, contentType);

        return putHttpResource(okHttpClient, resourceUrl, context.getCredentials(), headers, body, contentType);
    }

    public static Response put(OkHttpClient okHttpClient, ShapeTreeContext context, URL resourceUrl, List<URL> focusNodes, String body, ContentType contentType) throws ShapeTreeException {

        Objects.requireNonNull(okHttpClient, "Must provide a valid HTTP client to PUT with");
        Objects.requireNonNull(context, "Must provide a valid shape tree context to PUT with");
        Objects.requireNonNull(resourceUrl, "Must provide a resource URL to PUT to");

        log.debug("Updating shape tree instance via PUT at {}", resourceUrl);
        log.debug("Focus Node: {}", focusNodes == null || focusNodes.isEmpty() ? "None provided" : focusNodes.toString());

        Headers headers = setRequestHeaders(focusNodes, null, false, null, contentType);

        return putHttpResource(okHttpClient, resourceUrl, context.getCredentials(), headers, body, contentType);
    }

    public static Response patch(OkHttpClient okHttpClient, ShapeTreeContext context, URL resourceUrl, List<URL> focusNodes, String body) throws ShapeTreeException {

        Objects.requireNonNull(okHttpClient, "Must provide a valid HTTP client to PATCH with");
        Objects.requireNonNull(context, "Must provide a valid shape tree context to PATCH with");
        Objects.requireNonNull(resourceUrl, "Must provide a resource URL to PATCH to");
        Objects.requireNonNull(body, "Must provide a PATCH body");

        log.debug("PATCH-ing shape tree instance at {}", resourceUrl);
        log.debug("PATCH String: {}", body);
        log.debug("Focus Node: {}", focusNodes == null || focusNodes.isEmpty() ? "None provided" : focusNodes.toString());

        Headers headers = setRequestHeaders(focusNodes, null, false, null, SPARQL_UPDATE);

        return patchHttpResource(okHttpClient, resourceUrl, context.getCredentials(), headers, body, SPARQL_UPDATE);

    }

    private static Headers setRequestHeaders(List<URL> focusNodes, List<URL> targetShapeTrees, boolean isContainer, String slug, ContentType contentType) {
        Headers headers = null;
        if (contentType != null) { headers = addHttpHeader(CONTENT_TYPE, contentType.getValue(), headers); }
        if (targetShapeTrees != null) {
            for (URL shapeTreeUrl : targetShapeTrees) {
                headers = addLinkRelationHeader(TARGET_SHAPETREE, shapeTreeUrl.toString(), headers);
            }
        }
        if (focusNodes != null) {
            for (URL focusNodeUrl : focusNodes) {
                headers = addLinkRelationHeader(FOCUS_NODE, focusNodeUrl.toString(), headers);
            }
        }

        if (isContainer) { headers = addLinkRelationHeader(TYPE, BASIC_CONTAINER, headers); }
        if (slug != null) { headers = addHttpHeader(SLUG, slug, headers); }
        return headers;
    }

    private static Response getErrorResponse(URL resourceUrl, String method, int code, String message) {
        // first build a stub request
        Request.Builder requestBuilder = new Request.Builder();
        Request request = requestBuilder.url(resourceUrl).method(method, null).build();
        // build fabricated error response
        Response.Builder builder = new Response.Builder();
        return builder.request(request).code(code).message(message).protocol(Protocol.HTTP_1_1).build();
    }

}
