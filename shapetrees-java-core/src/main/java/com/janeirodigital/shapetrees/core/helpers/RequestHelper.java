package com.janeirodigital.shapetrees.core.helpers;

import com.janeirodigital.shapetrees.core.ShapeTreeManager;
import com.janeirodigital.shapetrees.core.ShapeTreeContext;
import com.janeirodigital.shapetrees.core.ManageableInstance;
import com.janeirodigital.shapetrees.core.InstanceResource;
import com.janeirodigital.shapetrees.core.ManagerResource;
import com.janeirodigital.shapetrees.core.ShapeTreeRequest;
import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.core.enums.LinkRelations;
import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
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
import java.util.List;
import java.util.Set;

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
        return new ShapeTreeContext(shapeTreeRequest.getHeaderValue(HttpHeaders.AUTHORIZATION.getValue()));
    }

    /**
     * This determines the type of resource being processed.
     *
     * Initial test is based on the incoming request headers, specifically the Content-Type header.
     * If the content type is not one of the accepted RDF types, it will be treated as a NON-RDF source.
     *
     * Then the determination becomes whether or not the resource is a container.
     *
     * If it is a PATCH or PUT and the URL provided already exists, then the existing resource's Link header(s)
     * are used to determine if it is a container or not.
     *
     * If it is a POST or if the resource does not already exist, the incoming request Link header(s) are relied
     * upon.
     *
     * @param shapeTreeRequest The current incoming request
     * @param existingResource The resource located at the incoming request's URL
     * @return ShapeTreeResourceType aligning to current request
     * @throws ShapeTreeException ShapeTreeException throw, specifically if Content-Type is not included on request
     */
    public static ShapeTreeResourceType determineResourceType(ShapeTreeRequest shapeTreeRequest, ManageableInstance existingResource) throws ShapeTreeException {
        boolean isNonRdf;
        if (!shapeTreeRequest.getMethod().equals(DELETE)) {
            String incomingRequestContentType = shapeTreeRequest.getContentType();
            // Ensure a content-type is present
            if (incomingRequestContentType == null) {
                throw new ShapeTreeException(400, "Content-Type is required");
            }

            isNonRdf = determineIsNonRdfSource(incomingRequestContentType);

        } else {
            isNonRdf = false;
        }

        if (isNonRdf) {
            return ShapeTreeResourceType.NON_RDF;
        }

        boolean isContainer = false;
        boolean resourceAlreadyExists = existingResource.getManageableResource().isExists();
        if ((shapeTreeRequest.getMethod().equals(PUT) || shapeTreeRequest.getMethod().equals(PATCH)) && resourceAlreadyExists) {
            isContainer = existingResource.getManageableResource().isContainer();
        } else if (shapeTreeRequest.getLinkHeaders() != null) {
            isContainer = getIsContainerFromRequest(shapeTreeRequest);
        }

        return isContainer ? ShapeTreeResourceType.CONTAINER : ShapeTreeResourceType.RESOURCE;
    }

    public static List<URL>
    getIncomingFocusNodes(ShapeTreeRequest shapeTreeRequest, URL baseUrl) throws ShapeTreeException {
        final List<String> focusNodeStrings = shapeTreeRequest.getLinkHeaders().allValues(LinkRelations.FOCUS_NODE.getValue());
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
        final List<String> targetShapeTreeStrings = shapeTreeRequest.getLinkHeaders().allValues(LinkRelations.TARGET_SHAPETREE.getValue());
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

        if ((shapeTreeRequest.getResourceType() == ShapeTreeResourceType.NON_RDF
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
     * Determines whether a content type is a supported RDF type
     * @param incomingRequestContentType Content type to test
     * @return Boolean indicating whether it is RDF or not
     */
    private static boolean determineIsNonRdfSource(String incomingRequestContentType) {
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
            final List<String> typeLinks = shapeTreeRequest.getLinkHeaders().allValues(LinkRelations.TYPE.getValue());
            if (!typeLinks.isEmpty()) {
                return (typeLinks.contains(LdpVocabulary.CONTAINER) ||
                        typeLinks.contains(LdpVocabulary.BASIC_CONTAINER));
            }
        }
        // As a secondary attempt, use slash path semantics
        return shapeTreeRequest.getUrl().getPath().endsWith("/");
    }

}
