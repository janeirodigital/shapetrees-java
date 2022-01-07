package com.janeirodigital.shapetrees.core.helpers;

import com.janeirodigital.shapetrees.core.DocumentResponse;
import com.janeirodigital.shapetrees.core.RelationAttributes;
import com.janeirodigital.shapetrees.core.ResourceAttributes;
import com.janeirodigital.shapetrees.core.enums.HttpHeader;
import com.janeirodigital.shapetrees.core.enums.LinkRelation;
import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.vocabularies.LdpVocabulary;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import static com.janeirodigital.shapetrees.core.ResourceAttributes.parseLinkHeaders;
import static com.janeirodigital.shapetrees.core.enums.HttpHeader.CONTENT_TYPE;
import static com.janeirodigital.shapetrees.core.enums.HttpHeader.LINK;
import static com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType.CONTAINER;
import static com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType.NON_RDF;

public class DocumentResponseHelper {

    private static final Set<String> RDF_CONTENT_TYPES = Set.of("text/turtle", "application/rdf+xml", "application/n-triples", "application/ld+json");

    private DocumentResponseHelper() { }

    /**
     * Gets the string value of the http header <code>header</code> stored in the
     * ResourceAttributes of the provided <code>response</code>.
     * @param response DocumentResponse to extract value from
     * @param header HTTP Header to get the value for
     * @return Optional string value of the header
     */
    public static Optional<String> getHeader(DocumentResponse response, HttpHeader header) {
        return getHeader(response.getResourceAttributes(), header);
    }

    /**
     * Gets the string value of the http header <code>header</code> stored in the
     * provided <code>attributes</code>.
     * @param attributes ResourceAttributes to extract value from
     * @param header HTTP header to get the value for
     * @return Optional string value of the header
     */
    public static Optional<String> getHeader(ResourceAttributes attributes, HttpHeader header) {
        return attributes.firstValue(header.getValue());
    }

    /**
     * Gets a list of string values for HTTP Link headers stored in the provided
     * <code>response</code>
     * @param response DocumentResponse to extract values from
     * @return List of string values from HTTP Link headers
     */
    public static List<String> getLinkHeaders(DocumentResponse response) {
        return response.getResourceAttributes().allValues(LINK.getValue());
    }

    /**
     * Extracts HTTP Link Relations from the HTTP Headers of the provided
     * <code>response</code>. Note that the ResourceAttributes class is
     * used to represent the resultant map of relations.
     * @param response DocumentResponse to extract values from
     * @return Map of link relations for HTTP Link headers
     */
    public static RelationAttributes getLinkRelations(DocumentResponse response) {
        List<String> linkHeaders = getLinkHeaders(response);
        return parseLinkHeaders(linkHeaders);
    }

    /**
     * Gets a single link relation value from the provided <code>linkRelations</code>
     * map of HTTP Link Relations
     * @param linkRelations Map of HTTP Link Relations as ResourceAttributes
     * @param relation Link relation type to get
     * @return Optional string value of the provided LinkRelation
     */
    public static Optional<String> getLinkRelation(ResourceAttributes linkRelations, LinkRelation relation) {
        return linkRelations.firstValue(relation.getValue());
    }

    /**
     * Determine a resource type by parsing Link rel=type headers
     * @param response DocumentResponse to parse
     * @return Type of resource
     */
    public static ShapeTreeResourceType
    getResourceType(DocumentResponse response) {

        RelationAttributes linkRelations = getLinkRelations(response);

        if (linkRelations.isEmpty()) { return NON_RDF; }

        if (linkRelations.containsType(LdpVocabulary.CONTAINER) || linkRelations.containsType(LdpVocabulary.BASIC_CONTAINER)) {
            return CONTAINER;
        }

        if (RDF_CONTENT_TYPES.contains(getHeader(response, CONTENT_TYPE).orElse(""))) { // orElse("") because contains(null) throw NPE
            return ShapeTreeResourceType.RESOURCE;
        }
        return NON_RDF;
    }

    /**
     * Check the validity of a URL string value
     * @param urlString URL string value to check
     * @throws ShapeTreeException thrown when the URL value is malformed
     */
    public static void checkStringAsUrl(String urlString) throws ShapeTreeException {
        stringToUrl(urlString);
    }

    /**
     * Convert the provided URL string value to URL
     * @param urlString URL string value to convert
     * @return Converted URL
     * @throws ShapeTreeException on Malformed URL string value
     */
    public static URL stringToUrl(String urlString) throws ShapeTreeException {
        try {
            return new URL(urlString);
        } catch (MalformedURLException ex) {
            throw new ShapeTreeException(500, "Cannot generate malformed URL: " + urlString);
        }
    }

    /**
     * Convert the provided URL string value to a URL, using the provided URL <code>context</code>
     * @param context URL context URL to create from
     * @param urlString URL string value to convert
     * @return Converted URL
     * @throws ShapeTreeException on Malformed URL string value
     */
    public static URL stringToUrl(URL context, String urlString) throws ShapeTreeException {
        try {
            return new URL(context, urlString);
        } catch (MalformedURLException ex) {
            throw new ShapeTreeException(500, "Cannot generate malformed URL: " + urlString);
        }
    }

}
