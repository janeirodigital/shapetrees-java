package com.janeirodigital.shapetrees.okhttp;

import com.janeirodigital.shapetrees.core.*;
import com.janeirodigital.shapetrees.core.enums.ContentType;
import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import okhttp3.Headers;
import okhttp3.OkHttpClient;
import okhttp3.Response;

import java.io.IOException;
import java.net.URL;
import java.util.Optional;

import static com.janeirodigital.shapetrees.core.enums.ContentType.OCTET_STREAM;
import static com.janeirodigital.shapetrees.core.enums.HttpHeader.*;
import static com.janeirodigital.shapetrees.core.enums.LinkRelation.MANAGED_BY;
import static com.janeirodigital.shapetrees.core.enums.LinkRelation.MANAGES;
import static com.janeirodigital.shapetrees.core.helpers.DocumentResponseHelper.*;
import static com.janeirodigital.shapetrees.okhttp.OkHttpHelper.*;

/**
 * Allows the {@link com.janeirodigital.shapetrees.core shapetrees-core} to access
 * {@link ManageableInstance}s and {@link InstanceResource}s over the network via HTTP. This is
 * particularly effective when employing client-side shape-tree validation in a
 * <a href="https://shapetrees.org/TR/specification/index.html#shapetree-support-from-proxy-or-client-side-library">proxy scenario</a>.
 *
 * <p>Given the fact that resources are accessed via HTTP, some inferences must be made on
 * resource state based on responses to HTTP requests.</p>
 */
@NoArgsConstructor
@Slf4j
public class OkHttpResourceAccessor implements ResourceAccessor {

    /**
     * Get a {@link InstanceResource} at the provided <code>url</code>, which may or may not exist.
     * Most of the work happens in {@link #generateResource(URL, DocumentResponse)}, which
     * processes the response and returns the corresponding typed resource.
     * @param context {@link ShapeTreeContext}
     * @param url Url of the resource to get
     * @return {@link InstanceResource}
     * @throws ShapeTreeException
     */
    @Override
    public InstanceResource
    getResource(ShapeTreeContext context, URL url) throws ShapeTreeException {

        OkHttpClient okHttpClient = OkHttpClientFactoryManager.getFactory().getOkHttpClient();

        try (Response response = getHttpResource(okHttpClient, url, context.getCredentials())) {
            ResourceAttributes attributes = new ResourceAttributes(response.headers().toMultimap());
            String body = response.body() == null ? "" : response.body().string();
            DocumentResponse documentResponse = new DocumentResponse(attributes, body, response.code());
            return generateResource(url, documentResponse);
        } catch(IOException ex) {
            throw new ShapeTreeException(500, "Failed to get remote resource: " + ex.getMessage());
        }
    }

    /**
     * Create a {@link InstanceResource} at the provided <code>url</code> via the provided HTTP
     * <code>method</code>. Most of the work happens in {@link #generateResource(URL, DocumentResponse)},
     * which processes the response and returns the corresponding typed resource.
     * @param context {@link ShapeTreeContext}
     * @param url Url of the resource to create
     * @param attributes HTTP attributes to use for resource creation
     * @param body Body of resource to create
     * @param contentType HTTP content-type
     * @return {@link InstanceResource}
     * @throws ShapeTreeException
     */
    @Override
    public InstanceResource
    createResource(ShapeTreeContext context, URL url, ResourceAttributes attributes, String body, String contentType) throws ShapeTreeException {
        log.debug("createResource: URL [{}]", url);
        log.debug("createResource: Attributes [{}]", attributes == null ? "None" : attributes.toString());

        // Get an okHttpClient
        OkHttpClient okHttpClient = OkHttpClientFactoryManager.getFactory().getOkHttpClient();
        if (contentType == null) { contentType = OCTET_STREAM.getValue(); }
        Headers headers = attributesToHeaders(attributes);
        headers = addHttpHeader(IF_NONE_MATCH, "*", headers);
        Response response = putHttpResource(okHttpClient, url, context.getCredentials(), headers, body, ContentType.get(contentType));
        if (!response.isSuccessful()) {
            throw new ShapeTreeException(response.code(), "Failed to create " + url + ": " + response.code() + " " + response.message());
        }
        return getResource(context, url);

    }

    /**
     * Updates the provided {@link InstanceResource} <code>updateResource</code> with <code>body</code> via the supplied
     * <code>method</code>
     * @param context Shape tree context
     * @param updateResource {@link InstanceResource} to update
     * @param body Body to use for update
     * @return {@link DocumentResponse} of the result
     * @throws ShapeTreeException
     */
    @Override
    public DocumentResponse
    updateResource(ShapeTreeContext context, InstanceResource updateResource, String body) throws ShapeTreeException {

        // Get an okHttpClient
        OkHttpClient okHttpClient = OkHttpClientFactoryManager.getFactory().getOkHttpClient();

        String contentTypeString = getHeader(updateResource.getAttributes(), CONTENT_TYPE).orElse(OCTET_STREAM.getValue());

        // [careful] updateResource attributes may contain illegal client headers (connection, content-length, date, expect, from, host, upgrade, via, warning)
        Response response = putHttpResource(okHttpClient, updateResource.getUrl(), context.getCredentials(), attributesToHeaders(updateResource.getAttributes()), body, ContentType.get(contentTypeString));
        if (!response.isSuccessful()) {
            throw new ShapeTreeException(response.code(), "Failed to update " + updateResource.getUrl() + ": " + response.code() + " " + response.message());
        }
        ResourceAttributes attributes = new ResourceAttributes(response.headers().toMultimap());
        return new DocumentResponse(attributes, response.message(), response.code());
    }

    /**
     * Deletes the provided {@link InstanceResource }<code>deleteResource</code>
     * @param context {@link ShapeTreeContext}
     * @param deleteResource {@link InstanceResource} to delete
     * @return {@link DocumentResponse} of the result
     * @throws ShapeTreeException
     */
    @Override
    public DocumentResponse
    deleteResource(ShapeTreeContext context, ManagerResource deleteResource) throws ShapeTreeException {

        OkHttpClient okHttpClient = OkHttpClientFactoryManager.getFactory().getOkHttpClient();

        // [careful] updateResource attributes may contain illegal client headers (connection, content-length, date, expect, from, host, upgrade, via, warning)
        Response response = deleteHttpResource(okHttpClient, deleteResource.getUrl(), context.getCredentials(), attributesToHeaders(deleteResource.getAttributes()));
        if (!response.isSuccessful()) {
            throw new ShapeTreeException(response.code(), "Failed to delete " + deleteResource.getUrl() + ": " + response.code() + " " + response.message());
        }
        ResourceAttributes attributes = new ResourceAttributes(response.headers().toMultimap());
        return new DocumentResponse(attributes, response.message(), response.code());
    }

    /**
     * Generates a typed {@link InstanceResource} based on the response from {@link #getResource(ShapeTreeContext, URL)} or
     * {@link #createResource(ShapeTreeContext, URL, ResourceAttributes, String, String)}.
     * Determines whether the resource is an existing {@link ManageableResource} or {@link ManagerResource}.
     * @param url Url of the resource to generate
     * @param response Response from a create or update of <code>url</code>
     * @return Generated {@link InstanceResource}, either {@link ManageableResource} or {@link ManagerResource}
     * @throws ShapeTreeException
     */
    private InstanceResource
    generateResource(URL url, DocumentResponse response) throws ShapeTreeException {

        // If a resource was created, ensure the URL returned in the Location header is valid
        final Optional<String> locationValue = getHeader(response, LOCATION);
        if (locationValue.isPresent()) { checkStringAsUrl(locationValue.get()); }

        // Determine whether the resource exists based on the response. Even if the resource
        // doesn't exist, additional context and processing is done to provide the appropriate
        // typed resource with adequate context to the caller
        final boolean exists = response.isExists();
        final ResourceAttributes attributes = response.getResourceAttributes();
        final ShapeTreeResourceType resourceType = getResourceType(response);
        final String name = calculateName(url);
        final String body = response.getBody();
        //  Parse Link headers from response
        final ResourceAttributes linkRelations = getLinkRelations(response);
        // Determine if the resource is a shape tree manager based on the response
        final boolean isManager = calculateIsManager(url, exists, linkRelations);

        if (isManager) {
            final URL managedResourceUrl = calculateManagedUrl(url, linkRelations);
            if (exists) {
                return new ManagerResource(url, resourceType, attributes, body, name, true, managedResourceUrl);
            } else {
                return new MissingManagerResource(url, resourceType, attributes, body, name, managedResourceUrl);
            }
        } else {
            // Look for presence of st:managedBy in link headers from response and get the target manager URL
            final Optional<URL> managerUrl = calculateManagerUrl(url, linkRelations);
            if (exists) {
                return new ManageableResource(url, resourceType, attributes, body, name, true, managerUrl);
            } else {
                return new MissingManageableResource(url, resourceType, attributes, body, name, managerUrl);
            }
        }
    }

    /**
     * Looks for the presence of the http://www.w3.org/ns/shapetrees#managedBy HTTP Link Relation in the
     * provided <code>parsedLinkHeaders</code>, with a valid target URL of a {@link ShapeTreeManager} associated
     * with the provided <code>url</code>.
     * @param url URL of the (potentially) managed resource
     * @param parsedLinkHeaders Parsed HTTP Link headers to evaluate
     * @return
     * @throws ShapeTreeException
     */
    private Optional<URL>
    calculateManagerUrl(URL url, ResourceAttributes parsedLinkHeaders) throws ShapeTreeException {
        final Optional<String> optManagerString = getLinkRelation(parsedLinkHeaders, MANAGED_BY);
        if (optManagerString.isEmpty()) { return Optional.empty(); }
        String managerUrlString = optManagerString.get();
        return Optional.of(stringToUrl(url, managerUrlString));
    }

    /**
     * Looks for the presence of the http://www.w3.org/ns/shapetrees#manages HTTP Link Relation in the
     * provided <code>parsedLinkHeaders</code>, with a valid target URL of a {@link ManagedResource}. Falls
     * back to a relatively crude inference when the more reliable header isn't available
     * @param managerUrl URL of the {@link ShapeTreeManager}
     * @param parsedLinkHeaders Parsed link headers from {@link ManagerResource} response
     * @return URL of {@link ManagedResource}
     * @throws ShapeTreeException
     */
    private URL
    calculateManagedUrl(URL managerUrl, ResourceAttributes parsedLinkHeaders) throws ShapeTreeException {
        String managedUrlString;
        final Optional<String> optManagedString = getLinkRelation(parsedLinkHeaders, MANAGES);
        if (optManagedString.isPresent()) {
            managedUrlString = optManagedString.get();
        } else {
            // Attempt to (crudely) infer based on path calculation
            // If this implementation uses a dot notation for meta, trim it from the path
            // Rebuild without the query string in case that was employed
            managedUrlString = managerUrl.getPath().replaceAll("\\.shapetree$", "");
        }
        return stringToUrl(managerUrl, managedUrlString);
    }

    /**
     * Calculates the name of the resource itself, removing any leading path and any trailing slash. In
     * the event that the resource is '/', then '/' will be returned.
     * @param url URL of the resource to evaluate
     * @return Name of resource
     */
    private String
    calculateName(URL url) {
        String path = url.getPath();

        if (path.equals("/")) return "/";

        // if this is a container, trim the trailing slash
        if (path.endsWith("/")) {
            path = path.substring(0, path.length() - 1);
        }

        int pathIndex = path.lastIndexOf('/');

        // No slashes in the path
        if (pathIndex == -1) {
            return path;
        }

        return path.substring(path.lastIndexOf('/') + 1);
    }

    /**
     * Determine whether <code>url</code> is a {@link ManagerResource}. Since this is a completely HTTP based
     * resource processor, this determination can't be made with special server-side knowledge about
     * the nature of the resources it serves. Instead, this must be derived based on information
     * present in the HTTP response from the server.
     * @param url URL of the resource that is being evaluated
     * @param exists whether the resource at <code>url</code> exists
     * @param parsedLinkHeaders Parsed HTTP Link headers from the response for <code>url</code>
     * @return True if {@link ManagerResource}
     */
    private boolean
    calculateIsManager(URL url, boolean exists, ResourceAttributes parsedLinkHeaders) {
        // If the resource has an HTTP Link header of type of https://www.w3.org/ns/shapetrees#managedBy
        // with a manager target, it is not a manager resource (because it is managed by one)
        if (exists && getLinkRelation(parsedLinkHeaders, MANAGED_BY).isPresent()) { return false; }
        // If the resource has an HTTP Link header of type of https://www.w3.org/ns/shapetrees#manages
        // it is a manager resource (because it manages another one).
        if (exists && getLinkRelation(parsedLinkHeaders, MANAGES).isPresent()) { return true; }
        // If the resource doesn't exist, attempt to infer based on the URL
        if (url.getPath() != null && url.getPath().matches(".*\\.shapetree$")) { return true; }
        return url.getQuery() != null && url.getQuery().matches(".*ext\\=shapetree$");
    }

}
