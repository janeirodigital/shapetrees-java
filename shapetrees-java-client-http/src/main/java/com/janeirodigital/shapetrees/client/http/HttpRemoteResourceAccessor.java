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

import java.net.URI;
import java.util.*;

@NoArgsConstructor
@Slf4j
public class HttpRemoteResourceAccessor implements ResourceAccessor {

    protected static final Set<String> supportedRDFContentTypes = Set.of("text/turtle", "application/rdf+xml", "application/n-triples", "application/ld+json");
    private static final String POST = "POST";
    private static final String PUT = "PUT";
    private static final String PATCH = "PATCH";

    @Override
    public ShapeTreeResource.Fork getResource(ShapeTreeContext context, URI uri) throws ShapeTreeException {
        log.debug("HttpRemoteResourceAccessor#getResource({})", uri);
        ResourceAttributes headers = new ResourceAttributes();
        headers.maybeSet(HttpHeaders.AUTHORIZATION.getValue(), context.getAuthorizationHeaderValue());
        HttpClient fetcher = AbstractHttpClientFactory.getFactory().get(false);
        HttpRequest req = new HttpRequest("GET", uri, headers, null, null);

        DocumentResponse response = fetcher.fetchShapeTreeResponse(req);
        return makeAFork(uri, response);
    }

    @Override
    public ShapeTreeResource.Fork createResource(ShapeTreeContext context, String method, URI uri, ResourceAttributes headers, String body, String contentType) throws ShapeTreeException {
        log.debug("createResource via {}: URI [{}], headers [{}]", method, uri, headers.toString());

        HttpClient fetcher = AbstractHttpClientFactory.getFactory().get(false);
        ResourceAttributes allHeaders = headers.maybePlus(HttpHeaders.AUTHORIZATION.getValue(), context.getAuthorizationHeaderValue());
        DocumentResponse response = fetcher.fetchShapeTreeResponse(new HttpRequest(method, uri, allHeaders, body, contentType));
        if (!response.exists()) {
            throw new ShapeTreeException(500, "Unable to create pre-existing resource <" + uri + ">");
        }
        return makeAFork(uri, response);
    }

    protected ShapeTreeResource.Fork makeAFork(URI uri, DocumentResponse response) throws ShapeTreeException {
        Optional<String> location = response.getResourceAttributes().firstValue(HttpHeaders.LOCATION.getValue());
        if (location.isPresent()) { uri = URI.create(location.get()); }
        // this.exists = response.exists(); !!
        final boolean exists = response.getStatusCode()/100 == 2;
        final boolean container = isContainerFromHeaders(response.getResourceAttributes(), uri);
        final ResourceAttributes attributes = response.getResourceAttributes();
        final ShapeTreeResourceType resourceType = getResourceTypeFromHeaders(response.getResourceAttributes());

        final String name = calculateName(uri);
        final String body = response.getBody();
        if (response.getBody() == null) {
            log.error("Exception retrieving body string");
        }

        final List<String> linkHeaders = attributes.allValues(HttpHeaders.LINK.getValue());
        ResourceAttributes parsedLinkHeaders = linkHeaders.size() == 0 // !!
                ? new ResourceAttributes()
                : ResourceAttributes.parseLinkHeaders(linkHeaders);
        final Optional<URI> metadataUri = calculateMetadataURI(uri, parsedLinkHeaders);
        final boolean metadata = calculateIsMetadata(uri, exists, parsedLinkHeaders);;

        if (Boolean.TRUE.equals(metadata)) {
            // If this implementation uses a dot notation for meta, trim it from the path
            final String basePath = uri.getPath().replaceAll("\\.shapetree$", "");

            // Rebuild without the query string in case that was employed
            final String userOwnedResourceUriString = uri.getScheme() + "://" + uri.getAuthority() + basePath;
            // @see https://github.com/xformativ/shapetrees-java/issues/86
            final URI userOwnedResourceUri = URI.create(userOwnedResourceUriString);

            final Optional<String> contentType = attributes.firstValue(HttpHeaders.CONTENT_TYPE.getValue().toLowerCase());
            return new ShapeTreeResource.Metadata(uri, resourceType, attributes, body, name, exists, userOwnedResourceUri);
        } else {
            return new ShapeTreeResource.UserOwned(uri, resourceType, attributes, body, name, exists, metadataUri, container);
        }
    }

    @Override
    public List<ShapeTreeResource> getContainedResources(ShapeTreeContext context, URI containerResourceURI) throws ShapeTreeException {
        try {
            ShapeTreeResource.Fork rf = this.getResource(context, containerResourceURI);
            if (!(rf instanceof ShapeTreeResource.UserOwned)) {
                throw new ShapeTreeException(500, "Cannot get contained resources for a metadata resource <" + containerResourceURI + ">");
            }
            ShapeTreeResource.UserOwned containerResource = (ShapeTreeResource.UserOwned) rf;

            if (Boolean.FALSE.equals(containerResource.isContainer())) {
                throw new ShapeTreeException(500, "Cannot get contained resources for a resource that is not a Container <" + containerResourceURI + ">");
            }

            Graph containerGraph = GraphHelper.readStringIntoGraph(containerResourceURI, containerResource.getBody(), containerResource.getAttributes().firstValue(HttpHeaders.CONTENT_TYPE.getValue()).orElse(null));

            if (containerGraph.isEmpty()) { return Collections.emptyList(); }

            List<Triple> containerTriples = containerGraph.find(NodeFactory.createURI(containerResourceURI.toString()),
                    NodeFactory.createURI(LdpVocabulary.CONTAINS),
                    Node.ANY).toList();

            if (containerTriples.isEmpty()) { return Collections.emptyList(); }

            ArrayList<ShapeTreeResource> containedResources = new ArrayList<>();

            for (Triple containerTriple : containerTriples) {
                ShapeTreeResource containedResource = new ShapeTreeResource(URI.create(containerTriple.getObject().getURI()), this, context); // getResource(context,URI.create(containerTriple.getObject().getURI()));
                containedResources.add(containedResource);
            }

            return containedResources;
        } catch (Exception ex) {
            throw new ShapeTreeException(500, ex.getMessage());
        }
    }

    @Override
    public DocumentResponse updateResource(ShapeTreeContext context, String method, ShapeTreeResource.Fork updatedResource, String body) throws ShapeTreeException { // TODO: API: called on Metadata but only uses Fork properties
        log.debug("updateResource: URI [{}]", updatedResource.getUri());

        String contentType = updatedResource.getAttributes().firstValue(HttpHeaders.CONTENT_TYPE.getValue()).orElse(null);
        // [careful] updatedResource attributes may contain illegal client headers (connection, content-length, date, expect, from, host, upgrade, via, warning)
        ResourceAttributes allHeaders = updatedResource.getAttributes().maybePlus(HttpHeaders.AUTHORIZATION.getValue(), context.getAuthorizationHeaderValue());
        HttpClient fetcher = AbstractHttpClientFactory.getFactory().get(false);
        DocumentResponse response = fetcher.fetchShapeTreeResponse(new HttpRequest(method, updatedResource.getUri(), allHeaders, body, contentType));
        return response;
    }

    @Override
    public DocumentResponse deleteResource(ShapeTreeContext context, ShapeTreeResource.Metadata deletedResource) throws ShapeTreeException {
        log.debug("deleteResource: URI [{}]", deletedResource.getUri());

        HttpClient fetcher = AbstractHttpClientFactory.getFactory().get(false);
        ResourceAttributes allHeaders = deletedResource.getAttributes().maybePlus(HttpHeaders.AUTHORIZATION.getValue(), context.getAuthorizationHeaderValue());
        DocumentResponse response = fetcher.fetchShapeTreeResponse(new HttpRequest("DELETE", deletedResource.getUri(), allHeaders, null, null));
        int respCode = response.getStatusCode();
        if (respCode < 200 || respCode >= 400) {
            log.error("Error deleting resource {}, Status {}", deletedResource.getUri(), respCode);
        }
        return response;
    }

    /**
     * Look for a Link rel=type of ldp:Container or ldp:BasicContainer
     * @param headers to parse
     * @return
     */
    public static boolean isContainerFromHeaders(ResourceAttributes headers, URI uri) {

        List<String> linkHeaders = headers.allValues(HttpHeaders.LINK.getValue());

        if (linkHeaders.size() == 0) { return uri.getPath().endsWith("/"); }

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

    static Optional<URI> calculateMetadataURI(URI uri, ResourceAttributes parsedLinkHeaders) {
        final Optional<String> optLocatorString = parsedLinkHeaders.firstValue(LinkRelations.SHAPETREE_LOCATOR.getValue());
        if (optLocatorString.isEmpty()) {
            log.info("The resource {} does not contain a link header of {}", uri, LinkRelations.SHAPETREE_LOCATOR.getValue());
            return Optional.empty();
        }
        String metaDataURIString = optLocatorString.get();
        if (metaDataURIString.startsWith("/")) {
            // If the header value doesn't include scheme/host, prefix it with the scheme & host from container
            URI shapeTreeContainerURI = uri;
            String portFragment;
            if (shapeTreeContainerURI.getPort() > 0) {
                portFragment = ":" + shapeTreeContainerURI.getPort();
            } else {
                portFragment = "";
            }
            metaDataURIString = shapeTreeContainerURI.getScheme() + "://" + shapeTreeContainerURI.getHost() + portFragment + metaDataURIString;
        }

        return Optional.of(URI.create(metaDataURIString));
    }

    static String calculateName(URI uri) {
        String path = uri.getPath();

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
    static Boolean calculateIsMetadata(URI uri, boolean exists, ResourceAttributes parsedLinkHeaders) {
        // If the resource has an HTTP Link header of type of https://www.w3.org/ns/shapetrees#ShapeTreeLocator
        // with a metadata target, it is not a metadata resource (because it is pointing to one)
        if (Boolean.TRUE.equals(exists) && parsedLinkHeaders != null && parsedLinkHeaders.firstValue(LinkRelations.SHAPETREE_LOCATOR.getValue()).isPresent()) {
            return false;
        }
        // If the resource doesn't exist, currently we need to do some inference based on the URI
        if (uri.getPath() != null && uri.getPath().matches(".*\\.shapetree$")) { return true; }
        return uri.getQuery() != null && uri.getQuery().matches(".*ext\\=shapetree$");
    }
}
