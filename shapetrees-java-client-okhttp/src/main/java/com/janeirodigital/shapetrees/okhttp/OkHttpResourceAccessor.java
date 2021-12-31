package com.janeirodigital.shapetrees.okhttp;

import com.janeirodigital.shapetrees.core.*;
import com.janeirodigital.shapetrees.core.enums.HttpHeader;
import com.janeirodigital.shapetrees.core.enums.LinkRelation;
import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.vocabularies.LdpVocabulary;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import okhttp3.*;
import org.apache.jena.graph.Graph;
import org.apache.jena.graph.Node;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Triple;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.*;

import static com.janeirodigital.shapetrees.core.enums.ContentType.OCTET_STREAM;
import static com.janeirodigital.shapetrees.core.helpers.GraphHelper.readStringIntoGraph;
import static com.janeirodigital.shapetrees.core.helpers.GraphHelper.urlToUri;
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

    private static final Set<String> supportedRDFContentTypes = Set.of("text/turtle", "application/rdf+xml", "application/n-triples", "application/ld+json");

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
        log.debug("OkHttpResourceAccessor#getResource({})", url);

        OkHttpClient okHttpClient = OkHttpClientFactoryManager.getFactory().getOkHttpClient();

        Request.Builder requestBuilder = new Request.Builder();
        requestBuilder.url(url);
        requestBuilder.method("GET", null);
        // Set the authorization header if we have credentials
        Headers requestHeaders = null;
        if (context.hasCredentials()) { requestHeaders = setHttpHeader(HttpHeader.AUTHORIZATION, context.getCredentials()); }
        if (requestHeaders != null) { requestBuilder.headers(requestHeaders); }

        DocumentResponse documentResponse;
        try (Response response = checkResponse(okHttpClient.newCall(requestBuilder.build()).execute())) {
            ResourceAttributes attributes = new ResourceAttributes(response.headers().toMultimap());
            documentResponse = new DocumentResponse(attributes, response.body().string(), response.code());
        } catch(IOException ex) {
            throw new ShapeTreeException(500, "Failed to get remote resource: " + ex.getMessage());
        }

        return generateResource(url, documentResponse);
    }

    /**
     * Create a {@link InstanceResource} at the provided <code>url</code> via the provided HTTP
     * <code>method</code>. Most of the work happens in {@link #generateResource(URL, DocumentResponse)},
     * which processes the response and returns the corresponding typed resource.
     * @param context {@link ShapeTreeContext}
     * @param method HTTP method to use for resource creation
     * @param url Url of the resource to create
     * @param headers HTTP headers to use for resource creation
     * @param body Body of resource to create
     * @param contentType HTTP content-type
     * @return {@link InstanceResource}
     * @throws ShapeTreeException
     */
    @Override
    public InstanceResource
    createResource(ShapeTreeContext context, String method, URL url, ResourceAttributes headers, String body, String contentType) throws ShapeTreeException {
        log.debug("createResource via {}: URL [{}], headers [{}]", method, url, headers.toString());

        // Get an okHttpClient
        OkHttpClient okHttpClient = OkHttpClientFactoryManager.getFactory().getOkHttpClient();

        Request.Builder requestBuilder = new Request.Builder();
        requestBuilder.url(url);
        if (contentType == null) { contentType = OCTET_STREAM.getValue(); }
        RequestBody requestBody = RequestBody.create(body, MediaType.get(contentType));
        requestBuilder.method(method, requestBody);

        Headers requestHeaders = null;
        if (headers != null) { requestHeaders = attributesToHeaders(headers); }
        if (context.hasCredentials()) { requestHeaders = setHttpHeader(HttpHeader.AUTHORIZATION, context.getCredentials(), requestHeaders); }
        if (requestHeaders != null) { requestBuilder.headers(requestHeaders); }

        DocumentResponse documentResponse;
        try (Response response = checkResponse(okHttpClient.newCall(requestBuilder.build()).execute())) {
            ResourceAttributes attributes = new ResourceAttributes(response.headers().toMultimap());
            documentResponse = new DocumentResponse(attributes, response.body().string(), response.code());
        } catch(IOException ex) {
            throw new ShapeTreeException(500, "Failed to get remote resource: " + ex.getMessage());
        }

        return generateResource(url, documentResponse);
    }

    /**
     * Generates a typed {@link InstanceResource} based on the response from {@link #getResource(ShapeTreeContext, URL)} or
     * {@link #createResource(ShapeTreeContext, String, URL, ResourceAttributes, String, String)}.
     * Determines whether the resource is an existing {@link ManageableResource} or {@link ManagerResource}.
     * @param url Url of the resource to generate
     * @param response Response from a create or update of <code>url</code>
     * @return Generated {@link InstanceResource}, either {@link ManageableResource} or {@link ManagerResource}
     * @throws ShapeTreeException
     */
    private InstanceResource
    generateResource(URL url, DocumentResponse response) throws ShapeTreeException {

        // If a resource was created, ensure the URL returned in the Location header is valid
        Optional<String> location = response.getResourceAttributes().firstValue(HttpHeader.LOCATION.getValue());
        if (location.isPresent()) {
            try {
                url = new URL(location.get());
            } catch (MalformedURLException e) {
                throw new ShapeTreeException(500, "Retrieving <" + url + "> yielded a Location header \"" + location.get() + "\" which doesn't parse as a URL: " + e.getMessage());
            }
        }

        // Determine whether the resource exists based on the response. Even if the resource
        // doesn't exist, additional context and processing is done to provide the appropriate
        // typed resource with adequate context to the caller
        final boolean exists = response.isExists();
        final boolean container = isContainerFromHeaders(response.getResourceAttributes(), url);
        final ResourceAttributes attributes = response.getResourceAttributes();
        final ShapeTreeResourceType resourceType = getResourceTypeFromHeaders(response.getResourceAttributes());

        final String name = calculateName(url);
        final String body = response.getBody();
        if (response.getBody() == null) {
            log.error("Could not retrieve the body string from response for " + url);
        }

        //  Parse Link headers from response and populate ResourceAttributes
        final List<String> linkHeaders = attributes.allValues(HttpHeader.LINK.getValue());
        ResourceAttributes parsedLinkHeaders = linkHeaders.isEmpty() // !!
                ? new ResourceAttributes()
                : ResourceAttributes.parseLinkHeaders(linkHeaders);

        // Determine if the resource is a shape tree manager based on the response
        final boolean isManager = calculateIsManager(url, exists, parsedLinkHeaders);

        if (Boolean.TRUE.equals(isManager)) {
            final URL managedResourceUrl = calculateManagedUrl(url, parsedLinkHeaders);
            if (exists) {
                return new ManagerResource(url, resourceType, attributes, body, name, true, managedResourceUrl);
            } else {
                return new MissingManagerResource(url, resourceType, attributes, body, name, managedResourceUrl);
            }
        } else {
            // Look for presence of st:managedBy in link headers from response and get the target manager URL
            final Optional<URL> managerUrl = calculateManagerUrl(url, parsedLinkHeaders);
            if (exists) {
                return new ManageableResource(url, resourceType, attributes, body, name, true, managerUrl, container);
            } else {
                return new MissingManageableResource(url, resourceType, attributes, body, name, managerUrl, container);
            }
        }
    }

    /**
     * Updates the provided {@link InstanceResource} <code>updateResource</code> with <code>body</code> via the supplied
     * <code>method</code>
     * @param context Shape tree context
     * @param method HTTP method to use for update
     * @param updateResource {@link InstanceResource} to update
     * @param body Body to use for update
     * @return {@link DocumentResponse} of the result
     * @throws ShapeTreeException
     */
    @Override
    public DocumentResponse
    updateResource(ShapeTreeContext context, String method, InstanceResource updateResource, String body) throws ShapeTreeException {
        log.debug("updateResource: URL [{}]", updateResource.getUrl());

        // Get an okHttpClient
        OkHttpClient okHttpClient = OkHttpClientFactoryManager.getFactory().getOkHttpClient();

        String contentType = updateResource.getAttributes().firstValue(HttpHeader.CONTENT_TYPE.getValue()).orElse(null);
        if (contentType == null) { contentType = OCTET_STREAM.getValue(); }

        Request.Builder requestBuilder = new Request.Builder();
        requestBuilder.url(updateResource.getUrl());
        RequestBody requestBody = RequestBody.create(body, MediaType.get(contentType));
        requestBuilder.method(method, requestBody);

        // [careful] updateResource attributes may contain illegal client headers (connection, content-length, date, expect, from, host, upgrade, via, warning)
        ResourceAttributes allHeaders = updateResource.getAttributes().maybePlus(HttpHeader.AUTHORIZATION.getValue(), context.getCredentials());
        if (allHeaders != null) { requestBuilder.headers(attributesToHeaders(allHeaders)); }

        try (Response response = checkResponse(okHttpClient.newCall(requestBuilder.build()).execute())) {
            ResourceAttributes attributes = new ResourceAttributes(response.headers().toMultimap());
            return new DocumentResponse(attributes, response.body().string(), response.code());
        } catch(IOException ex) {
            throw new ShapeTreeException(500, "Failed to update remote resource: " + ex.getMessage());
        }
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
        log.debug("deleteResource: URL [{}]", deleteResource.getUrl());

        OkHttpClient okHttpClient = OkHttpClientFactoryManager.getFactory().getOkHttpClient();

        Request.Builder requestBuilder = new Request.Builder();
        requestBuilder.url(deleteResource.getUrl());
        RequestBody requestBody = RequestBody.create(null, new byte[0]);
        requestBuilder.method("DELETE", requestBody);

        // [careful] updateResource attributes may contain illegal client headers (connection, content-length, date, expect, from, host, upgrade, via, warning)
        ResourceAttributes allHeaders = deleteResource.getAttributes().maybePlus(HttpHeader.AUTHORIZATION.getValue(), context.getCredentials());
        if (allHeaders != null) { requestBuilder.headers(attributesToHeaders(allHeaders)); }

        try (Response response = checkResponse(okHttpClient.newCall(requestBuilder.build()).execute())) {
            ResourceAttributes attributes = new ResourceAttributes(response.headers().toMultimap());
            if (response.code() < 200 || response.code() >= 400) {
                log.error("Error deleting resource {}, Status {}", deleteResource.getUrl(), response.code());
            }
            return new DocumentResponse(attributes, response.body().string(), response.code());
        } catch(IOException ex) {
            throw new ShapeTreeException(500, "Failed to delete remote resource: " + ex.getMessage());
        }
    }

    /**
     * Look for a Link rel=type of ldp:Container or ldp:BasicContainer
     * @param headers to parse
     * @return True if headers indicating a container are found
     */
    private boolean
    isContainerFromHeaders(ResourceAttributes headers, URL url) {

        List<String> linkHeaders = headers.allValues(HttpHeader.LINK.getValue());

        if (linkHeaders.isEmpty()) { return url.getPath().endsWith("/"); }

        ResourceAttributes parsedLinkHeaders = ResourceAttributes.parseLinkHeaders(linkHeaders);

        List<String> typeLinks = parsedLinkHeaders.allValues(LinkRelation.TYPE.getValue());
        if (!typeLinks.isEmpty()) {
            return typeLinks.contains(LdpVocabulary.CONTAINER) ||
                    typeLinks.contains(LdpVocabulary.BASIC_CONTAINER);
        }
        return false;
    }

    /**
     * Determine a resource type by parsing Link rel=type headers
     * @param headers to parse
     * @return Type of resource
     */
    private ShapeTreeResourceType
    getResourceTypeFromHeaders(ResourceAttributes headers) {

        List<String> linkHeaders = headers.allValues(HttpHeader.LINK.getValue());

        if (linkHeaders == null) { return null; }

        ResourceAttributes parsedLinkHeaders = ResourceAttributes.parseLinkHeaders(linkHeaders);

        List<String> typeLinks = parsedLinkHeaders.allValues(LinkRelation.TYPE.getValue());
        if (typeLinks != null &&
                (typeLinks.contains(LdpVocabulary.CONTAINER) ||
                        typeLinks.contains(LdpVocabulary.BASIC_CONTAINER))) {
            return ShapeTreeResourceType.CONTAINER;
        }

        if (supportedRDFContentTypes.contains(headers.firstValue(HttpHeader.CONTENT_TYPE.getValue()).orElse(""))) { // orElse("") because contains(null) throw NPE
            return ShapeTreeResourceType.RESOURCE;
        }
        return ShapeTreeResourceType.NON_RDF;
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
        final Optional<String> optManagerString = parsedLinkHeaders.firstValue(LinkRelation.MANAGED_BY.getValue());
        if (optManagerString.isEmpty()) {
            log.info("The resource {} does not contain a link header of {}", url, LinkRelation.MANAGED_BY.getValue());
            return Optional.empty();
        }
        String managerUrlString = optManagerString.get();
        try {
            return Optional.of(new URL(url, managerUrlString));
        } catch (MalformedURLException e) {
            throw new ShapeTreeException(500, "Malformed relative URL <" + managerUrlString + "> (resolved from <" + url + ">)");
        }
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
        URL managedResourceUrl;

        final Optional<String> optManagedString = parsedLinkHeaders.firstValue(LinkRelation.MANAGES.getValue());
        if (!optManagedString.isEmpty()) {
            managedUrlString = optManagedString.get();
        } else {
            // Attempt to (crudely) infer based on path calculation
            // If this implementation uses a dot notation for meta, trim it from the path
            // Rebuild without the query string in case that was employed
            managedUrlString = managerUrl.getPath().replaceAll("\\.shapetree$", "");
        }

        try {
            managedResourceUrl = new URL(managerUrl, managedUrlString);
        } catch (MalformedURLException e) {
            throw new ShapeTreeException(500, "Can't calculate managed resource for shape tree manager <" + managerUrl + ">");
        }

        return managedResourceUrl;
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
        if (Boolean.TRUE.equals(exists) && parsedLinkHeaders.firstValue(LinkRelation.MANAGED_BY.getValue()).isPresent()) {
            return false;
        }
        // If the resource has an HTTP Link header of type of https://www.w3.org/ns/shapetrees#manages
        // it is a manager resource (because it manages another one).
        if (Boolean.TRUE.equals(exists) && parsedLinkHeaders.firstValue(LinkRelation.MANAGES.getValue()).isPresent()) {
            return true;
        }
        // If the resource doesn't exist, attempt to infer based on the URL
        if (url.getPath() != null && url.getPath().matches(".*\\.shapetree$")) { return true; }
        return url.getQuery() != null && url.getQuery().matches(".*ext\\=shapetree$");
    }

}
