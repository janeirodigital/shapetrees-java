package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.core.enums.LinkRelations;
import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;
import com.janeirodigital.shapetrees.core.vocabularies.LdpVocabulary;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.apache.jena.graph.Graph;

import java.net.URI;
import java.util.List;
import java.util.Optional;
import java.util.Set;

@Getter @Slf4j
public class ShapeTreeResource {
    protected URI uri;
    protected boolean exists;
    @Setter protected String body;
    protected ResourceAttributes attributes;
    protected Optional<URI> associatedUri;
    protected String name;
    protected ShapeTreeResourceType resourceType;
    protected boolean container;
    protected boolean metadata;
    protected boolean managed;
    protected Optional<Graph> graph;

    @Override
    public String toString() {
        return "ShapeTreeResource{" +
                "uri=" + this.uri +
                ", associatedUri=" + this.associatedUri +
                ", name='" + this.name + '\'' +
                ", body='" + this.body + '\'' +
                ", type=" + this.resourceType +
                ", exists=" + this.exists +
                ", container=" + this.container +
                ", metadata=" + this.metadata +
                ", managed=" + this.managed +
                ", attributes=" + this.attributes +
                '}';
    }

    protected static final Set<String> supportedRDFContentTypes = Set.of("text/turtle", "application/rdf+xml", "application/n-triples", "application/ld+json");

    public ShapeTreeResource(URI fetchURI, DocumentResponse response) {
        Optional<String> location = response.getResourceAttributes().firstValue(HttpHeaders.LOCATION.getValue());
        this.uri = location.isPresent() ? URI.create(location.get()) : fetchURI;
        this.exists = response.getStatusCode()/100 == 2;
        this.container = isContainerFromHeaders(response.getResourceAttributes(), fetchURI);
        this.attributes = new ResourceAttributes(response.getResourceAttributes().toMultimap());
        this.resourceType = getResourceTypeFromHeaders(response.getResourceAttributes());

        this.body = response.getBody();
        if (response.getBody() == null) {
            log.error("Exception retrieving body string");
        }

        this.name = null;
        this.metadata = false;
        this.associatedUri = Optional.empty();
        this.managed = false; // TODO: Wasn't set before. playing with values
        this.graph = Optional.empty();
    }

    protected ShapeTreeResource() {
    }

    /**
     * Look for a Link rel=type of ldp:Container or ldp:BasicContainer
     * @param headers to parse
     * @return
     */
    protected static boolean isContainerFromHeaders(ResourceAttributes headers, URI uri) {

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
    protected static ShapeTreeResourceType getResourceTypeFromHeaders(ResourceAttributes headers) {

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
}
