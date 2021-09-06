package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.core.enums.LinkRelations;
import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;
import com.janeirodigital.shapetrees.core.vocabularies.LdpVocabulary;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

import java.net.URI;
import java.util.List;
import java.util.Optional;
import java.util.Set;

@Getter @Setter @Slf4j
public class ShapeTreeResource {
    private URI uri;
    private URI associatedUri;
    private String name;
    private String body;
    ShapeTreeResourceType type;
    private boolean exists;
    private boolean container;
    private boolean metadata;
    private boolean managed;
    private ResourceAttributes attributes;

    protected static final Set<String> supportedRDFContentTypes = Set.of("text/turtle", "application/rdf+xml", "application/n-triples", "application/ld+json");

    public ShapeTreeResource() {}

    public ShapeTreeResource(URI fetchURI, DocumentResponse response) {
        this.exists = response.getStatusCode()/100 == 2;
        this.container = isContainerFromHeaders(response.getResourceAttributes());
        this.type = getResourceTypeFromHeaders(response.getResourceAttributes());

        this.body = response.getBody();
        if (response.getBody() == null) {
            log.error("Exception retrieving body string");
        }
        this.attributes = new ResourceAttributes(response.getResourceAttributes().toMultimap());
        Optional<String> location = response.getResourceAttributes().firstValue(HttpHeaders.LOCATION.getValue());
        URI uri = location.isPresent() ? URI.create(location.get()) : fetchURI;
        this.uri = uri;
    }

    /**
     * Look for a Link rel=type of ldp:Container or ldp:BasicContainer
     * @param headers to parse
     * @return
     */
    protected static boolean isContainerFromHeaders(ResourceAttributes headers) {

        List<String> linkHeaders = headers.allValues(HttpHeaders.LINK.getValue());

        if (linkHeaders == null) { return false; }

        ResourceAttributes parsedLinkHeaders = ResourceAttributes.parseLinkHeaders(linkHeaders);

        List<String> typeLinks = parsedLinkHeaders.allValues(LinkRelations.TYPE.getValue());
        if (typeLinks != null) {
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
