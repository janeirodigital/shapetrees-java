package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.core.enums.LinkRelations;
import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;
import com.janeirodigital.shapetrees.core.vocabularies.LdpVocabulary;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

import java.net.URI;
import java.util.List;
import java.util.Optional;
import java.util.Set;

@Getter @Slf4j
public class ShapeTreeResource {
    private final URI uri;
    private final boolean exists;
    @Setter private String body;
    private final ResourceAttributes attributes;

    private Optional<URI> associatedUri;
    private String name;
    ShapeTreeResourceType resourceType;
    private final boolean container;
    private boolean metadata;
    private boolean managed;

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

    public ShapeTreeResource(URI uri,
                             boolean exists,
                             String name,
                             boolean metadata,
                             boolean container,
                             ResourceAttributes attributes,
                             Optional<URI> associatedUri,
                             ShapeTreeResourceType resourceType,
                             boolean managed,
                             String body) {
        this.uri = uri;
        this.exists = exists;
        this.name = name;
        this.metadata = metadata;
        this.container = container;
        this.attributes = attributes;
        this.associatedUri = associatedUri;

        if (this.exists) {
            this.resourceType = resourceType;
            this.managed = managed;
            this.body = body;
        } else {
            this.resourceType = null;
            this.managed = false;
            this.body = null;
        }
    }

    public ShapeTreeResource(URI fetchURI, DocumentResponse response) {
        Optional<String> location = response.getResourceAttributes().firstValue(HttpHeaders.LOCATION.getValue());
        this.uri = location.isPresent() ? URI.create(location.get()) : fetchURI;
        this.exists = response.getStatusCode()/100 == 2;
        this.container = isContainerFromHeaders(response.getResourceAttributes());
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
