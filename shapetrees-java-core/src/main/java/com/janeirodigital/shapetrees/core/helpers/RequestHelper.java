package com.janeirodigital.shapetrees.core.helpers;

import com.janeirodigital.shapetrees.core.enums.HttpHeader;
import com.janeirodigital.shapetrees.core.enums.LinkRelation;
import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.validation.ShapeTreeContext;
import com.janeirodigital.shapetrees.core.validation.ShapeTreeRequest;
import com.janeirodigital.shapetrees.core.resources.InstanceResource;
import com.janeirodigital.shapetrees.core.resources.ManagerResource;
import com.janeirodigital.shapetrees.core.validation.ShapeTreeManager;
import com.janeirodigital.shapetrees.core.vocabularies.LdpVocabulary;
import lombok.extern.slf4j.Slf4j;
import org.apache.jena.graph.Graph;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.update.UpdateAction;
import org.apache.jena.update.UpdateFactory;
import org.apache.jena.update.UpdateRequest;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;

import static com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType.NON_RDF;
import static com.janeirodigital.shapetrees.core.helpers.GraphHelper.urlToUri;

@Slf4j
public class RequestHelper {

    private static final String PUT = "PUT";
    private static final String PATCH = "PATCH";
    private static final String DELETE = "DELETE";

    private static final Set<String> supportedRDFContentTypes = Set.of("text/turtle", "application/rdf+xml", "application/n-triples", "application/ld+json");
    private static final Set<String> supportedSPARQLContentTypes = Set.of("application/sparql-update");

    private RequestHelper() { }

    /**
     * Builds a ShapeTreeContext from the incoming request.  Specifically it retrieves
     * the incoming Authorization header and stashes that value for use on any additional requests made during
     * validation.
     * @param shapeTreeRequest Incoming request
     * @return ShapeTreeContext object populated with authentication details, if present
     */
    public static ShapeTreeContext buildContextFromRequest(ShapeTreeRequest shapeTreeRequest) {
        return new ShapeTreeContext(shapeTreeRequest.getHeaderValue(HttpHeader.AUTHORIZATION.getValue()));
    }

    /**
     * Determines the type of resource represented by an incoming ShapeTreeRequest.
     *
     * Initial test is based on the incoming request headers, specifically the Content-Type header.
     * If the content type is not one of the accepted RDF types, it will be treated as a NON-RDF source.
     *
     * Then the determination becomes whether or not the resource is a container.
     *
     * @param shapeTreeRequest The current incoming request
     * @return ShapeTreeResourceType aligning to current request
     * @throws ShapeTreeException ShapeTreeException throw, specifically if Content-Type is not included on request
     */
    public static ShapeTreeResourceType getIncomingResourceType(ShapeTreeRequest shapeTreeRequest) throws ShapeTreeException {

        final List<String> methods = Arrays.asList("POST", "PUT", "PATCH");

        if (!methods.contains(shapeTreeRequest.getMethod())) { return null; };

        if (shapeTreeRequest.getContentType() == null) {
            throw new ShapeTreeException(500, "Cannot determine incoming resource type because Content-Type is missing from request");
        }

        if (isNonRdfSource(shapeTreeRequest.getContentType())) { return NON_RDF; }

        boolean isContainer = getIsContainerFromRequest(shapeTreeRequest);

        return isContainer ? ShapeTreeResourceType.CONTAINER : ShapeTreeResourceType.RESOURCE;
    }

    public static List<URL>
    getIncomingFocusNodes(ShapeTreeRequest shapeTreeRequest, URL baseUrl) throws ShapeTreeException {
        final List<String> focusNodeStrings = shapeTreeRequest.getLinkHeaders().allValues(LinkRelation.FOCUS_NODE.getValue());
        final List<URL> focusNodeUrls = new ArrayList<>();
        if (!focusNodeStrings.isEmpty()) {
            for (String focusNodeUrlString : focusNodeStrings) {
                try {
                    final URL focusNodeUrl = new URL(baseUrl, focusNodeUrlString);
                    focusNodeUrls.add(focusNodeUrl);
                } catch (MalformedURLException e) {
                    throw new ShapeTreeException(500, "Malformed focus node when resolving <" + focusNodeUrlString + "> against <" + baseUrl + ">");
                }
            }
        }
        return focusNodeUrls;
    }

    /**
     * Gets target shape tree / hint from request header
     * @param shapeTreeRequest Request
     * @return URL value of target shape tree
     * @throws ShapeTreeException ShapeTreeException
     */
    public static List<URL>
    getIncomingTargetShapeTrees(ShapeTreeRequest shapeTreeRequest, URL baseUrl) throws ShapeTreeException {
        final List<String> targetShapeTreeStrings = shapeTreeRequest.getLinkHeaders().allValues(LinkRelation.TARGET_SHAPETREE.getValue());
        final List<URL> targetShapeTreeUrls = new ArrayList<>();
        if (!targetShapeTreeStrings.isEmpty()) {
            for (String targetShapeTreeUrlString : targetShapeTreeStrings) {
                try {
                    final URL targetShapeTreeUrl = new URL(targetShapeTreeUrlString);
                    targetShapeTreeUrls.add(targetShapeTreeUrl);
                } catch (MalformedURLException e) {
                    throw new ShapeTreeException(500, "Malformed focus node when resolving <" + targetShapeTreeUrlString + "> against <" + baseUrl + ">");
                }
            }
        }
        return targetShapeTreeUrls;
    }

    public static ShapeTreeManager getIncomingShapeTreeManager(ShapeTreeRequest shapeTreeRequest, ManagerResource managerResource) throws ShapeTreeException {

        Graph incomingBodyGraph = RequestHelper.getIncomingBodyGraph(shapeTreeRequest, RequestHelper.normalizeSolidResourceUrl(shapeTreeRequest.getUrl(), null, ShapeTreeResourceType.RESOURCE), managerResource);
        if (incomingBodyGraph == null) { return null; }
        return ShapeTreeManager.getFromGraph(shapeTreeRequest.getUrl(), incomingBodyGraph);
    }

    /**
     * Normalizes the BaseURL to use for a request based on the incoming request.
     * @param url URL of request
     * @param requestedName Requested name of resource (provided on created resources via POST)
     * @param resourceType Description of resource (Container, NonRDF, Resource)
     * @return BaseURL to use for RDF Graphs
     * @throws ShapeTreeException ShapeTreeException
     */
    public static URL normalizeSolidResourceUrl(URL url, String requestedName, ShapeTreeResourceType resourceType) throws ShapeTreeException {
        String urlString = url.toString();
        if (requestedName != null) {
            urlString += requestedName;
        }
        if (resourceType == ShapeTreeResourceType.CONTAINER && !urlString.endsWith("/")) {
            urlString += "/";
        }
        try {
            return new URL(urlString);
        } catch (MalformedURLException ex) {
            throw new ShapeTreeException(500, "normalized to malformed URL <" + urlString + "> - " + ex.getMessage());
        }
    }

    /**
     * Loads body of request into graph
     * @param shapeTreeRequest Request
     * @param baseUrl BaseURL to use for graph
     * @param targetResource
     * @return Graph representation of request body
     * @throws ShapeTreeException ShapeTreeException
     */
    public static Graph getIncomingBodyGraph(ShapeTreeRequest shapeTreeRequest, URL baseUrl, InstanceResource targetResource) throws ShapeTreeException {
        log.debug("Reading request body into graph with baseUrl {}", baseUrl);

        if ((shapeTreeRequest.getResourceType() == NON_RDF
                && !shapeTreeRequest.getContentType().equalsIgnoreCase("application/sparql-update"))
                || shapeTreeRequest.getBody() == null
                || shapeTreeRequest.getBody().length() == 0) {
            return null;
        }

        Graph targetResourceGraph = null;

        if (shapeTreeRequest.getMethod().equals(PATCH)) {

            // In the event of a SPARQL PATCH, we get the SPARQL query and evaluate it, passing the
            // resultant graph back to the caller

            if (targetResource != null) {
                targetResourceGraph = targetResource.getGraph(baseUrl);
            }

            if (targetResourceGraph == null) {   // if the target resource doesn't exist or has no content
                log.debug("Existing target resource graph to patch does not exist.  Creating an empty graph.");
                targetResourceGraph = ModelFactory.createDefaultModel().getGraph();
            }

            // Perform a SPARQL update locally to ensure that resulting graph validates against ShapeTree
            UpdateRequest updateRequest = UpdateFactory.create(shapeTreeRequest.getBody(), baseUrl.toString());
            UpdateAction.execute(updateRequest, targetResourceGraph);

            if (targetResourceGraph == null) {
                throw new ShapeTreeException(400, "No graph after update");
            }

        } else {
            targetResourceGraph = GraphHelper.readStringIntoGraph(urlToUri(baseUrl), shapeTreeRequest.getBody(), shapeTreeRequest.getContentType());
        }

        return targetResourceGraph;
    }

    /**
     * Get the URL of the incoming resource target's parent container
     * @return URL of the parent container
     * @throws ShapeTreeException
     */
    public static URL getIncomingParentContainerUrl(ShapeTreeRequest shapeTreeRequest) throws ShapeTreeException {
        final String rel = getIsContainerFromRequest(shapeTreeRequest) ? ".." : ".";
        try {
            return new URL(shapeTreeRequest.getUrl(), rel);
        } catch (MalformedURLException e) {
            throw new ShapeTreeException(500, "Malformed focus node when resolving <" + rel + "> against <" + shapeTreeRequest.getUrl() + ">");
        }
    }

    /**
     * Determines whether a content type is a supported RDF type
     * @param incomingRequestContentType Content type to test
     * @return Boolean indicating whether it is RDF or not
     */
    private static boolean isNonRdfSource(String incomingRequestContentType) {
        return (!supportedRDFContentTypes.contains(incomingRequestContentType.toLowerCase()) &&
                !supportedSPARQLContentTypes.contains(incomingRequestContentType.toLowerCase()));
    }

    /**
     * Determines if a resource should be treated as a container based on its request Link headers
     * @param shapeTreeRequest Request
     * @return Is the resource a container?
     */
    private static Boolean getIsContainerFromRequest(ShapeTreeRequest shapeTreeRequest) {
        // First try to determine based on link headers
        if (shapeTreeRequest.getLinkHeaders() != null) {
            final List<String> typeLinks = shapeTreeRequest.getLinkHeaders().allValues(LinkRelation.TYPE.getValue());
            if (!typeLinks.isEmpty()) {
                return (typeLinks.contains(LdpVocabulary.CONTAINER) ||
                        typeLinks.contains(LdpVocabulary.BASIC_CONTAINER));
            }
        }
        if (shapeTreeRequest.getMethod().equals("POST")) {
            String slug = shapeTreeRequest.getHeaderValue(HttpHeader.SLUG.getValue());
            if (slug != null) { return slug.endsWith("/"); }
        }
        return shapeTreeRequest.getUrl().getPath().endsWith("/");
    }

}
