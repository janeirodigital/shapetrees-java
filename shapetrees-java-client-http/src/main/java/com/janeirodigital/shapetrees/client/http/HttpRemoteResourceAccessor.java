package com.janeirodigital.shapetrees.client.http;

import com.janeirodigital.shapetrees.core.*;
import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.core.enums.LinkRelations;
import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.helpers.GraphHelper;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;
import com.janeirodigital.shapetrees.core.vocabularies.LdpVocabulary;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.jena.graph.Graph;
import org.apache.jena.graph.Node;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Triple;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.*;

import static com.janeirodigital.shapetrees.core.helpers.GraphHelper.*;

@NoArgsConstructor
@Slf4j
public class HttpRemoteResourceAccessor implements ResourceAccessor {

    protected static final Set<String> supportedRDFContentTypes = Set.of("text/turtle", "application/rdf+xml", "application/n-triples", "application/ld+json");
    private static final String POST = "POST";
    private static final String PUT = "PUT";
    private static final String PATCH = "PATCH";

    @Override
    public ShapeTreeResource.Fork getResource(ShapeTreeContext context, URL url) throws ShapeTreeException {
        log.debug("HttpRemoteResourceAccessor#getResource({})", url);
        ResourceAttributes headers = new ResourceAttributes();
        headers.maybeSet(HttpHeaders.AUTHORIZATION.getValue(), context.getAuthorizationHeaderValue());
        HttpClient fetcher = AbstractHttpClientFactory.getFactory().get(false);
        HttpRequest req = new HttpRequest("GET", url, headers, null, null);

        DocumentResponse response = fetcher.fetchShapeTreeResponse(req);
        return makeAFork(url, response);
    }

    @Override
    public ShapeTreeResource.Fork createResource(ShapeTreeContext context, String method, URL url, ResourceAttributes headers, String body, String contentType) throws ShapeTreeException {
        log.debug("createResource via {}: URL [{}], headers [{}]", method, url, headers.toString());

        HttpClient fetcher = AbstractHttpClientFactory.getFactory().get(false);
        ResourceAttributes allHeaders = headers.maybePlus(HttpHeaders.AUTHORIZATION.getValue(), context.getAuthorizationHeaderValue());
        DocumentResponse response = fetcher.fetchShapeTreeResponse(new HttpRequest(method, url, allHeaders, body, contentType));
        if (!response.isExists()) {
            throw new ShapeTreeException(500, "Unable to create pre-existing resource <" + url + ">");
        }
        return makeAFork(url, response);
    }

    protected ShapeTreeResource.Fork makeAFork(URL url, DocumentResponse response) throws ShapeTreeException {
        Optional<String> location = response.getResourceAttributes().firstValue(HttpHeaders.LOCATION.getValue());
        if (location.isPresent()) {
            try {
                url = new URL(location.get());
            } catch (MalformedURLException e) {
                throw new ShapeTreeException(500, "Retrieving <" + url + "> yielded a Location header \"" + location.get() + "\" which doesn't parse as a URL: " + e.getMessage());
            }
        }
        // this.exists = response.exists(); !!
        final boolean exists = response.getStatusCode()/100 == 2;
        final boolean container = isContainerFromHeaders(response.getResourceAttributes(), url);
        final ResourceAttributes attributes = response.getResourceAttributes();
        final ShapeTreeResourceType resourceType = getResourceTypeFromHeaders(response.getResourceAttributes());

        final String name = calculateName(url);
        final String body = response.getBody();
        if (response.getBody() == null) {
            log.error("Exception retrieving body string");
        }

        final List<String> linkHeaders = attributes.allValues(HttpHeaders.LINK.getValue());
        ResourceAttributes parsedLinkHeaders = linkHeaders.size() == 0 // !!
                ? new ResourceAttributes()
                : ResourceAttributes.parseLinkHeaders(linkHeaders);
        final Optional<URL> metadataUri = calculateMetadataURI(url, parsedLinkHeaders);
        final boolean metadata = calculateIsMetadata(url, exists, parsedLinkHeaders);;

        if (Boolean.TRUE.equals(metadata)) {
            // If this implementation uses a dot notation for meta, trim it from the path
            // Rebuild without the query string in case that was employed
            // @see https://github.com/xformativ/shapetrees-java/issues/86
            final URL userOwnedResourceUri;
            try {
                userOwnedResourceUri = new URL(url, url.getPath().replaceAll("\\.shapetree$", ""));
            } catch (MalformedURLException e) {
                throw new ShapeTreeException(500, "can't calculate primary resource for metadata <" + url + ">");
            }

            final Optional<String> contentType = attributes.firstValue(HttpHeaders.CONTENT_TYPE.getValue().toLowerCase());
            return new ShapeTreeResource.Metadata(url, resourceType, attributes, body, name, exists, userOwnedResourceUri);
        } else {
            return new ShapeTreeResource.Primary(url, resourceType, attributes, body, name, exists, metadataUri, container);
        }
    }

    @Override
    public List<ShapeTreeResource> getContainedResources(ShapeTreeContext context, URL containerResourceUrl) throws ShapeTreeException {
        try {
            ShapeTreeResource.Fork rf = this.getResource(context, containerResourceUrl);
            if (!(rf instanceof ShapeTreeResource.Primary)) {
                throw new ShapeTreeException(500, "Cannot get contained resources for a metadata resource <" + containerResourceUrl + ">");
            }
            ShapeTreeResource.Primary containerResource = (ShapeTreeResource.Primary) rf;

            if (Boolean.FALSE.equals(containerResource.isContainer())) {
                throw new ShapeTreeException(500, "Cannot get contained resources for a resource that is not a Container <" + containerResourceUrl + ">");
            }

            Graph containerGraph = readStringIntoGraph(urlToUri(containerResourceUrl), containerResource.getBody(), containerResource.getAttributes().firstValue(HttpHeaders.CONTENT_TYPE.getValue()).orElse(null));

            if (containerGraph.isEmpty()) { return Collections.emptyList(); }

            List<Triple> containerTriples = containerGraph.find(NodeFactory.createURI(containerResourceUrl.toString()),
                    NodeFactory.createURI(LdpVocabulary.CONTAINS),
                    Node.ANY).toList();

            if (containerTriples.isEmpty()) { return Collections.emptyList(); }

            ArrayList<ShapeTreeResource> containedResources = new ArrayList<>();

            for (Triple containerTriple : containerTriples) {
                ShapeTreeResource containedResource = new ShapeTreeResource(new URL(containerTriple.getObject().getURI()), this, context); // getResource(context,URL.create(containerTriple.getObject().getURL()));
                containedResources.add(containedResource);
            }

            return containedResources;
        } catch (Exception ex) {
            throw new ShapeTreeException(500, ex.getMessage());
        }
    }

    @Override
    public DocumentResponse updateResource(ShapeTreeContext context, String method, ShapeTreeResource.Fork updatedResource, String body) throws ShapeTreeException {
        log.debug("updateResource: URL [{}]", updatedResource.getUrl());

        String contentType = updatedResource.getAttributes().firstValue(HttpHeaders.CONTENT_TYPE.getValue()).orElse(null);
        // [careful] updatedResource attributes may contain illegal client headers (connection, content-length, date, expect, from, host, upgrade, via, warning)
        ResourceAttributes allHeaders = updatedResource.getAttributes().maybePlus(HttpHeaders.AUTHORIZATION.getValue(), context.getAuthorizationHeaderValue());
        HttpClient fetcher = AbstractHttpClientFactory.getFactory().get(false);
        DocumentResponse response = fetcher.fetchShapeTreeResponse(new HttpRequest(method, updatedResource.getUrl(), allHeaders, body, contentType));
        return response;
    }

    @Override
    public DocumentResponse deleteResource(ShapeTreeContext context, ShapeTreeResource.Metadata deletedResource) throws ShapeTreeException {
        log.debug("deleteResource: URL [{}]", deletedResource.getUrl());

        HttpClient fetcher = AbstractHttpClientFactory.getFactory().get(false);
        ResourceAttributes allHeaders = deletedResource.getAttributes().maybePlus(HttpHeaders.AUTHORIZATION.getValue(), context.getAuthorizationHeaderValue());
        DocumentResponse response = fetcher.fetchShapeTreeResponse(new HttpRequest("DELETE", deletedResource.getUrl(), allHeaders, null, null));
        int respCode = response.getStatusCode();
        if (respCode < 200 || respCode >= 400) {
            log.error("Error deleting resource {}, Status {}", deletedResource.getUrl(), respCode);
        }
        return response;
    }

    /**
     * Look for a Link rel=type of ldp:Container or ldp:BasicContainer
     * @param headers to parse
     * @return
     */
    public static boolean isContainerFromHeaders(ResourceAttributes headers, URL url) {

        List<String> linkHeaders = headers.allValues(HttpHeaders.LINK.getValue());

        if (linkHeaders.size() == 0) { return url.getPath().endsWith("/"); }

        ResourceAttributes parsedLinkHeaders = ResourceAttributes.parseLinkHeaders(linkHeaders);

        List<String> typeLinks = parsedLinkHeaders.allValues(LinkRelations.TYPE.getValue());
        if (typeLinks.size() != 0) {
            return typeLinks.contains(LdpVocabulary.CONTAINER) ||
                    typeLinks.contains(LdpVocabulary.BASIC_CONTAINER);
        }
        return false;
    }

    /**
     * Determine a resource type by parsing Link rel=type headers
     * @param headers to parse
     * @return
     */
    public static ShapeTreeResourceType getResourceTypeFromHeaders(ResourceAttributes headers) {

        List<String> linkHeaders = headers.allValues(HttpHeaders.LINK.getValue());

        if (linkHeaders == null) { return null; }

        ResourceAttributes parsedLinkHeaders = ResourceAttributes.parseLinkHeaders(linkHeaders);

        List<String> typeLinks = parsedLinkHeaders.allValues(LinkRelations.TYPE.getValue());
        if (typeLinks != null &&
                (typeLinks.contains(LdpVocabulary.CONTAINER) ||
                        typeLinks.contains(LdpVocabulary.BASIC_CONTAINER))) {
            return ShapeTreeResourceType.CONTAINER;
        }

        if (supportedRDFContentTypes.contains(headers.firstValue(HttpHeaders.CONTENT_TYPE.getValue()).orElse(""))) { // orElse("") because contains(null) throw NPE
            return ShapeTreeResourceType.RESOURCE;
        }
        return ShapeTreeResourceType.NON_RDF;
    }

    static Optional<URL> calculateMetadataURI(URL url, ResourceAttributes parsedLinkHeaders) throws ShapeTreeException {
        final Optional<String> optLocatorString = parsedLinkHeaders.firstValue(LinkRelations.SHAPETREE_LOCATOR.getValue());
        if (optLocatorString.isEmpty()) {
            log.info("The resource {} does not contain a link header of {}", url, LinkRelations.SHAPETREE_LOCATOR.getValue());
            return Optional.empty();
        }
        String metaDataURIString = optLocatorString.get();
        try {
            return Optional.of(new URL(url, metaDataURIString));
        } catch (MalformedURLException e) {
            throw new ShapeTreeException(500, "Malformed relative URL <" + metaDataURIString + "> (resolved from <" + url + ">)");
        }
    }

    static String calculateName(URL url) {
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

    // TODO: #86
    static Boolean calculateIsMetadata(URL url, boolean exists, ResourceAttributes parsedLinkHeaders) {
        // If the resource has an HTTP Link header of type of https://www.w3.org/ns/shapetrees#ShapeTreeLocator
        // with a metadata target, it is not a metadata resource (because it is pointing to one)
        if (Boolean.TRUE.equals(exists) && parsedLinkHeaders != null && parsedLinkHeaders.firstValue(LinkRelations.SHAPETREE_LOCATOR.getValue()).isPresent()) {
            return false;
        }
        // If the resource doesn't exist, currently we need to do some inference based on the URL
        if (url.getPath() != null && url.getPath().matches(".*\\.shapetree$")) { return true; }
        return url.getQuery() != null && url.getQuery().matches(".*ext\\=shapetree$");
    }
}
