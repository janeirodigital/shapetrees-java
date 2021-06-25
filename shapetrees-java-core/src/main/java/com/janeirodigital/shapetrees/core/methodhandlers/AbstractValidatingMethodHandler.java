package com.janeirodigital.shapetrees.core.methodhandlers;

import com.janeirodigital.shapetrees.core.*;
import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.core.enums.LinkRelations;
import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.helpers.GraphHelper;
import com.janeirodigital.shapetrees.core.helpers.HttpHeaderHelper;
import com.janeirodigital.shapetrees.core.models.*;
import com.janeirodigital.shapetrees.core.vocabularies.LdpVocabulary;
import com.janeirodigital.shapetrees.core.vocabularies.ShapeTreeVocabulary;
import lombok.extern.slf4j.Slf4j;
import org.apache.jena.graph.Graph;
import org.apache.jena.graph.GraphUtil;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Triple;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;

import java.io.IOException;
import java.io.StringWriter;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.*;

/**
 * Abstract class providing reusable functionality to different method handlers
 */
@Slf4j
public abstract class AbstractValidatingMethodHandler {
    public static final String TEXT_TURTLE = "text/turtle";
    protected final ResourceAccessor resourceAccessor;
    protected final Set<String> supportedRDFContentTypes = Set.of(TEXT_TURTLE, "application/rdf+xml", "application/n-triples", "application/ld+json");
    private static final String REL_TYPE_CONTAINER = "<" + LdpVocabulary.CONTAINER + ">; rel=\"" + LinkRelations.TYPE.getValue() + "\"";

    protected AbstractValidatingMethodHandler(ResourceAccessor resourceAccessor) {
        this.resourceAccessor = resourceAccessor;
    }

    /**
     * Builds a ShapeTreeContext from the incoming request.  Specifically it retrieves
     * the incoming Authorization header and stashes that value for use on any additional requests made during
     * validation.
     * @param shapeTreeRequest Incoming request
     * @return ShapeTreeContext object populated with authentication details, if present
     */
    protected ShapeTreeContext buildContextFromRequest(ShapeTreeRequest<?> shapeTreeRequest) {
        ShapeTreeContext context = new ShapeTreeContext();
        context.setAuthorizationHeaderValue(shapeTreeRequest.getHeaderValue(HttpHeaders.AUTHORIZATION.getValue()));
        return context;
    }

    /**
     * Retrieves a representation of the resource present at the URI of the incoming ShapeTreeRequest
     * @param context ShapeTreeContext used for authentication
     * @param shapeTreeRequest Incoming request to retrieve resource URI from
     * @return ShapeTreeResource representation of that URI
     * @throws ShapeTreeException ShapeTreeException
     */
    protected ShapeTreeResource getRequestResource(ShapeTreeContext context, ShapeTreeRequest<?> shapeTreeRequest) throws ShapeTreeException {
        return this.resourceAccessor.getResource(context, shapeTreeRequest.getURI());
    }

    /**
     * This determines the type of resource being processed.
     *
     * Initial test is based on the incoming request headers, specifically the Content-Type header.
     * If the content type is not one of the accepted RDF types, it will be treated as a NON-RDF source.
     *
     * Then the determination becomes whether or not the resource is a container.
     *
     * If it is a PATCH or PUT and the URI provided already exists, then the existing resource's Link header(s)
     * are used to determine if it is a container or not.
     *
     * If it is a POST or if the resource does not already exist, the incoming request Link header(s) are relied
     * upon.
     *
     * @param shapeTreeRequest The current incoming request
     * @param existingResource The resource located at the incoming request's URI
     * @return ShapeTreeResourceType aligning to current request
     * @throws ShapeTreeException ShapeTreeException throw, specifically if Content-Type is not included on request
     */
    protected ShapeTreeResourceType determineResourceType(ShapeTreeRequest<?> shapeTreeRequest, ShapeTreeResource existingResource) throws ShapeTreeException {
        boolean isNonRdf;
        if (!shapeTreeRequest.getMethod().equals("DELETE")) {
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

        boolean resourceAlreadyExists = existingResource.isExists();
        boolean isContainer = false;
        if ((shapeTreeRequest.getMethod().equals("PUT") || shapeTreeRequest.getMethod().equals("PATCH")) && resourceAlreadyExists) {
            isContainer = existingResource.isContainer();
        } else if (shapeTreeRequest.getLinkHeaders() != null) {
            isContainer = getIsContainerFromIncomingLinkHeaders(shapeTreeRequest);
        }

        return isContainer ? ShapeTreeResourceType.CONTAINER : ShapeTreeResourceType.RESOURCE;
    }

    /**
     * Normalizes the BaseURI to use for a request based on the incoming request.
     * @param uri URI of request
     * @param requestedName Requested name of resource (provided on created resources via POST)
     * @param resourceType Description of resource (Container, NonRDF, Resource)
     * @return BaseURI to use for RDF Graphs
     * @throws URISyntaxException URISyntaxException
     */
    protected URI normalizeBaseURI(URI uri, String requestedName, ShapeTreeResourceType resourceType) throws URISyntaxException {
        String uriString = uri.toString();
        if (requestedName != null) {
            uriString += requestedName;
        }
        if (resourceType == ShapeTreeResourceType.CONTAINER && !uriString.endsWith("/")) {
            uriString += "/";
        }
        return new URI(uriString);
    }

    /**
     * Loads body of request into graph
     * @param shapeTreeRequest Request
     * @param baseURI BaseURI to use for graph
     * @return Graph representation of request body
     * @throws ShapeTreeException ShapeTreeException
     */
    protected Graph getIncomingBodyGraph(ShapeTreeRequest<?> shapeTreeRequest, URI baseURI) throws ShapeTreeException {
        log.debug("Reading request body into graph with baseURI {}", baseURI);

        if (shapeTreeRequest.getResourceType() != ShapeTreeResourceType.NON_RDF &&
                shapeTreeRequest.getBody() != null &&
                shapeTreeRequest.getBody().length() > 0) {
            return GraphHelper.readStringIntoGraph(baseURI, shapeTreeRequest.getBody(), shapeTreeRequest.getContentType());
        }
        return null;
    }

    /**
     * Gets focus node from request header
     * @param shapeTreeRequest Request
     * @param baseURI Base URI for use on relative focus nodes
     * @return URI of focus node
     * @throws IOException IOException
     */
    protected URI getIncomingResolvedFocusNode(ShapeTreeRequest<?> shapeTreeRequest, URI baseURI) throws IOException {
        if (shapeTreeRequest.getLinkHeaders().get(LinkRelations.FOCUS_NODE.getValue()) != null) {
            String focusNode = shapeTreeRequest.getLinkHeaders().get(LinkRelations.FOCUS_NODE.getValue()).get(0);
            return baseURI.resolve(focusNode);
        } else {
            throw new ShapeTreeException(400, "No Link header with relation " + LinkRelations.FOCUS_NODE.getValue() + " supplied, unable to perform Shape validation");
        }
    }

    /**
     * Gets target shape tree / hint from request header
     * @param shapeTreeRequest Request
     * @return URI value of target shape tree
     * @throws URISyntaxException URISyntaxException
     */
    protected URI getIncomingTargetShapeTreeHint(ShapeTreeRequest<?> shapeTreeRequest) throws URISyntaxException {
        if (shapeTreeRequest.getLinkHeaders().get(LinkRelations.TARGET_SHAPETREE.getValue()) != null) {
            return new URI(shapeTreeRequest.getLinkHeaders().get(LinkRelations.TARGET_SHAPETREE.getValue()).get(0));
        }
        return null;
    }

    /**
     * Determines if a resource should be treated as a container based on its request Link headers
     * @param shapeTreeRequest Request
     * @return Is the resource a container?
     */
    protected Boolean getIsContainerFromIncomingLinkHeaders(ShapeTreeRequest<?> shapeTreeRequest) {
        if (shapeTreeRequest.getLinkHeaders() != null && shapeTreeRequest.getLinkHeaders().get(LinkRelations.TYPE.getValue()) != null) {
            return (shapeTreeRequest.getLinkHeaders().get(LinkRelations.TYPE.getValue()).contains(LdpVocabulary.CONTAINER) ||
                    shapeTreeRequest.getLinkHeaders().get(LinkRelations.TYPE.getValue()).contains(LdpVocabulary.BASIC_CONTAINER));
        }
        return false;
    }

    /**
     * Identifies the appropriate plant result to return based on a collection of plant results
     * @param plantResults Collection of ShapeTreePlantResult
     * @param request Request
     * @return ShapeTreeValidationResponse representing the appropriate plant response
     */
    protected static ShapeTreeValidationResponse createPlantResponse(List<ShapeTreePlantResult> plantResults, ShapeTreeRequest<?> request) {

        // As multiple ShapeTrees can be planted at once, if there is more than ShapeTree relation Link header,
        // the response to the POST will be for the ShapeTree that was requested first.
        ShapeTreePlantResult primaryPlantResult = null;
        if (plantResults.size() > 1) {
            Map<String, List<String>> linkHeaders = request.getLinkHeaders();
            if (linkHeaders.containsKey(LinkRelations.SHAPETREE.getValue())) {
                String primaryShapeTreeURI = linkHeaders.get(LinkRelations.SHAPETREE.getValue()).get(0);
                for (ShapeTreePlantResult plantResult : plantResults) {
                    if (plantResult.getShapeTreeURI().toString().equals(primaryShapeTreeURI)) {
                        primaryPlantResult = plantResult;
                        break;
                    }
                }
            }
        } else {
            primaryPlantResult = plantResults.get(0);
        }

        ShapeTreeValidationResponse response = new ShapeTreeValidationResponse();

        if (primaryPlantResult != null) {
            response.setStatusCode(201);
            response.addResponseHeader(HttpHeaders.LOCATION.getValue(), primaryPlantResult.getRootContainer().toString());
            response.addResponseHeader(HttpHeaders.LINK.getValue(), "<" + primaryPlantResult.getRootContainerMetadata().toString() + ">; rel=\"" + LinkRelations.SHAPETREE.getValue() + "\"");
            response.addResponseHeader(HttpHeaders.CONTENT_TYPE.getValue(), TEXT_TURTLE);
            response.setBody("");
        } else {
            log.error("Unable to find 'primary' plant result in createPlantResponse");
            response.setStatusCode(400);
        }

        return response;
    }

    /**
     * Determines whether a content type is a supported RDF type
     * @param incomingRequestContentType Content type to test
     * @return Boolean indicating whether it is RDF or not
     */
    protected boolean determineIsNonRdfSource(String incomingRequestContentType) {
        return !this.supportedRDFContentTypes.contains(incomingRequestContentType.toLowerCase());
    }

    /**
     * Returns parent container URI for a given resource
     * @param shapeTreeResource Resource
     * @return URI to the resource's parent container
     */
    protected URI getParentContainerURI(ShapeTreeResource shapeTreeResource) {
        return shapeTreeResource.getUri().resolve(shapeTreeResource.isContainer() ? ".." : ".");
    }

    /**
     * Returns resource name from a resource URI
     * @param shapeTreeResource Resource
     * @return Resource name
     */
    protected String getRequestResourceName(ShapeTreeResource shapeTreeResource) {
        return shapeTreeResource.getUri().toString().replace(getParentContainerURI(shapeTreeResource).toString(), "");
    }

    /**
     * Returns a ShapeTree from list of shape trees that has a validatedByShape predicate
     * @param shapeTreesToPlant List of shape trees to test
     * @return Shape tree that has a shape URI
     */
    protected ShapeTree getShapeTreeWithShapeURI(List<ShapeTree> shapeTreesToPlant) {
        for (ShapeTree shapeTree : shapeTreesToPlant) {
            if (shapeTree.getValidatedByShapeUri() != null) {
                return shapeTree;
            }
        }
        return null;
    }

    /**
     * Returns a ShapeTree from list of shape trees that has a a contents predicate
     * @param shapeTreesToPlant List of shape trees to test
     * @return Shape tree that has one or more contents
     */
    protected ShapeTree getShapeTreeWithContents(List<ShapeTree> shapeTreesToPlant) {
        for (ShapeTree shapeTree : shapeTreesToPlant) {
            if (shapeTree.getContains() != null && !shapeTree.getContains().isEmpty()) {
                return shapeTree;
            }
        }
        return null;
    }

    /**
     * Returns URI of shape tree auxiliary resource for a given resource
     * @param shapeTreeResource resource
     * @return URI to shape tree auxiliary resource
     * @throws ShapeTreeException ShapeTreeException
     */
    protected URI getShapeTreeMetadataURIForResource(ShapeTreeResource shapeTreeResource) throws ShapeTreeException {
        Map<String, List<String>> linkHeaders = HttpHeaderHelper.parseLinkHeadersToMap(shapeTreeResource.getAttributes().get(HttpHeaders.LINK.getValue()));

        if (!linkHeaders.containsKey(LinkRelations.SHAPETREE.getValue())) {
            log.error("The resource {} does not contain a link header of {}", shapeTreeResource.getUri(), LinkRelations.SHAPETREE.getValue());
            throw new ShapeTreeException(500, "No Link header with relation of " + LinkRelations.SHAPETREE.getValue() + " found");
        }
        String metaDataURIString = linkHeaders.get(LinkRelations.SHAPETREE.getValue()).stream().findFirst().orElse(null);
        if (metaDataURIString != null && metaDataURIString.startsWith("/")) {
            // If the header value doesn't include scheme/host, prefix it with the scheme & host from container
            URI shapeTreeContainerURI = shapeTreeResource.getUri();
            String portFragment;
            if (shapeTreeContainerURI.getPort() > 0) {
                portFragment = ":" + shapeTreeContainerURI.getPort();
            } else {
                portFragment = "";
            }
            metaDataURIString = shapeTreeContainerURI.getScheme() + "://" + shapeTreeContainerURI.getHost() + portFragment + metaDataURIString;
        }

        if (metaDataURIString == null) {
            throw new ShapeTreeException(500, "No Link header with relation of " + LinkRelations.SHAPETREE.getValue() + " found");
        }

        return URI.create(metaDataURIString);
    }

    /**
     * Returns shape tree auxiliary resource for a given resource
     * @param shapeTreeResource resource
     * @return shape tree auxiliary resource
     * @throws ShapeTreeException ShapeTreeException
     */
    protected ShapeTreeResource getShapeTreeMetadataResourceForResource(ShapeTreeContext shapeTreeContext, ShapeTreeResource shapeTreeResource) throws ShapeTreeException {
        return this.resourceAccessor.getResource(shapeTreeContext, getShapeTreeMetadataURIForResource(shapeTreeResource));
    }

    /**
     * Returns a graph representation of a resource
     * @param resource Resource to get graph of
     * @param baseURI BaseURI to use for triples
     * @return Graph representation of resource
     * @throws ShapeTreeException ShapeTreeException
     */
    protected Graph getGraphForResource(ShapeTreeResource resource, URI baseURI) throws ShapeTreeException {
        if (!resource.isExists()) return null;

        return GraphHelper.readStringIntoGraph(baseURI, resource.getBody(), resource.getFirstAttributeValue(HttpHeaders.CONTENT_TYPE.getValue()));
    }

    /**
     * Determines whether a graph is valid for a given parent container
     * @param shapeTreeContext ShapeTreeContext used for authentication
     * @param graphToValidate Graph contents to be validated
     * @param baseURI BaseURI used for RDF graph
     * @param parentContainer Parent container to use for basis of validation (the shape trees managing this container
     *                        will be used to determine if the graph is valid)
     * @param resourceName Name of resource
     * @param shapeTreeRequest Request
     * @return ValidationContext with details of the validation process
     * @throws IOException IOException
     * @throws URISyntaxException URISyntaxException
     */
    protected ValidationContext validateAgainstParentContainer(ShapeTreeContext shapeTreeContext, Graph graphToValidate, URI baseURI, ShapeTreeResource parentContainer, String resourceName, ShapeTreeRequest<?> shapeTreeRequest) throws IOException, URISyntaxException {
        ShapeTreeResource parentContainerMetadataResource = getShapeTreeMetadataResourceForResource(shapeTreeContext, parentContainer);
        // If there is no metadata for the parent container, it is not managed
        if (!parentContainerMetadataResource.isExists()) return null;

        Graph parentContainerMetadataGraph = getGraphForResource(parentContainerMetadataResource, parentContainer.getUri());

        //List<ShapeTreeLocator> locators = ShapeTreeLocator.getShapeTreeLocatorsFromGraph(parentContainerMetadataGraph);

        ShapeTreeLocator locator = ShapeTreeLocator.getShapeTreeLocatorFromGraph(parentContainerMetadataGraph);

        // If there are no ShapeTree locators in the metadata graph, it is not managed
        if (locator == null) return null;

        // This means the existing parent container has one or more ShapeTrees associated with it
        List<ShapeTree> existingShapeTrees = new ArrayList<>();
        for (ShapeTreeLocation locations : locator.getLocations()) {
            existingShapeTrees.add(ShapeTreeFactory.getShapeTree(new URI(locations.getShapeTree())));
        }

        ShapeTree shapeTreeWithContents = getShapeTreeWithContents(existingShapeTrees);

        URI targetShapeTreeHint = getIncomingTargetShapeTreeHint(shapeTreeRequest);
        ShapeTree targetShapeTree = shapeTreeWithContents.findMatchingContainsShapeTree(resourceName, targetShapeTreeHint, shapeTreeRequest.getResourceType());

        // If no targetShapeTree is returned, it can be assumed that no validation is required
        ValidationResult validationResult = null;
        if (targetShapeTree != null) {

            // If there is a graph to validate...and a ShapeTree indicates it wants to validate the container body
            if (graphToValidate != null && targetShapeTree.getValidatedByShapeUri() != null) {
                // ...and a focus node was provided via the focusNode header, then we perform our validation
                URI focusNodeURI = getIncomingResolvedFocusNode(shapeTreeRequest, baseURI);
                log.debug("Validating against parent container.  ST with Contents {}, Focus Node {}", shapeTreeWithContents.getURI(), focusNodeURI);
                validationResult = targetShapeTree.validateContent(graphToValidate, focusNodeURI, shapeTreeRequest.getResourceType());
            }

            // If there is a body graph and it did not pass validation, return an error
            if (graphToValidate != null && validationResult != null && Boolean.FALSE.equals(validationResult.getValid())) {
                throw new ShapeTreeException(422, "Payload did not meet requirements defined by ShapeTree " + targetShapeTree.getURI());
            }
        }

        return new ValidationContext(targetShapeTree, validationResult, locator);
    }

    protected ShapeTreePlantResult plantShapeTree(ShapeTreeContext shapeTreeContext, ShapeTreeResource parentContainer, Graph bodyGraph, ShapeTree rootShapeTree, String rootContainer, ShapeTree shapeTree, String requestedName) throws IOException, URISyntaxException {
        StringWriter sw = new StringWriter();
        if (bodyGraph != null) {
            RDFDataMgr.write(sw, bodyGraph, Lang.TURTLE);
        }
        return plantShapeTree(shapeTreeContext, parentContainer, sw.toString(), TEXT_TURTLE, rootShapeTree, rootContainer, shapeTree, requestedName);
    }

    protected ShapeTreePlantResult plantShapeTree(ShapeTreeContext shapeTreeContext, ShapeTreeResource parentContainer, String body, String contentType, ShapeTreeLocator locator, ShapeTree targetShapeTree, String requestedName) throws IOException, URISyntaxException {
        // TODO - Need to properly get at the shape tree location when we get back to planting
        ShapeTree rootShapeTree = ShapeTreeFactory.getShapeTree(new URI(locator.getLocations().get(0).getRootShapeTree()));
        if (rootShapeTree == null) {
            return null;
        }
        // TODO - Need to properly get at the shape tree location when we get back to planting
        return plantShapeTree(shapeTreeContext, parentContainer, body, contentType, rootShapeTree, locator.getLocations().get(0).getRootShapeTree(), targetShapeTree, requestedName);
    }

    protected ShapeTreePlantResult plantShapeTree(ShapeTreeContext shapeTreeContext, ShapeTreeResource parentContainer, String body, String contentType, ShapeTree rootShapeTree, String rootContainer, ShapeTree shapeTree, String requestedName) throws IOException, URISyntaxException {
        log.debug("plantShapeTree: parent [{}], root tree [{}], tree [{}], slug [{}], ", parentContainer.getUri(), rootShapeTree.getId(), shapeTree.getId(), requestedName);

        ShapeTreeResource plantedContainerResource = createOrReuseContainer(shapeTreeContext, parentContainer.getUri(), requestedName, body, contentType);
        ShapeTreeResource plantedContainerMetadataResource = getShapeTreeMetadataResourceForResource(shapeTreeContext, plantedContainerResource);

        // In a POST scenario where the container has not yet been created, it cannot be passed into plantShapeTree
        // hierarchy of recursive method calls.  So, if it is null, set it to the URI of the planted container.
        if (rootContainer == null) {
            rootContainer = plantedContainerResource.getUri().toString();
        }

        // Get the existing graph and reuse it, if possible, if not, create a new graph
        Graph plantedContainerMetadataGraph;
        if (plantedContainerMetadataResource.isExists()) {
            plantedContainerMetadataGraph = getGraphForResource(plantedContainerMetadataResource, plantedContainerResource.getUri());
        } else {
            plantedContainerMetadataGraph = ModelFactory.createDefaultModel().getGraph();
        }

        // Generate a UUID for the ShapeTree
        UUID shapeTreeLocatorUUID = UUID.randomUUID();

        List<Triple> triplesToAdd = new ArrayList<>();
        // Add the triple for the new st:hasShapeTreeLocator
        String plantedContainerURI = plantedContainerResource.getUri().toString() + (plantedContainerResource.getUri().toString().endsWith("/")? "":"/");
        String shapeTreeLocatorURI = plantedContainerURI + "#" + shapeTreeLocatorUUID;
        triplesToAdd.add(new Triple(NodeFactory.createURI(plantedContainerURI), NodeFactory.createURI(ShapeTreeVocabulary.HAS_SHAPE_TREE_LOCATOR), NodeFactory.createURI(shapeTreeLocatorURI)));

        // Add the triples for the locator itself
        triplesToAdd.add(new Triple(NodeFactory.createURI(shapeTreeLocatorURI), NodeFactory.createURI(ShapeTreeVocabulary.HAS_SHAPE_TREE), NodeFactory.createURI(shapeTree.getId())));
        triplesToAdd.add(new Triple(NodeFactory.createURI(shapeTreeLocatorURI), NodeFactory.createURI(ShapeTreeVocabulary.HAS_SHAPE_TREE_INSTANCE_ROOT), NodeFactory.createURI(rootContainer)));
        triplesToAdd.add(new Triple(NodeFactory.createURI(shapeTreeLocatorURI), NodeFactory.createURI(ShapeTreeVocabulary.HAS_ROOT_SHAPE_TREE), NodeFactory.createURI(rootShapeTree.getId())));
        GraphUtil.add(plantedContainerMetadataGraph, triplesToAdd);

        StringWriter sw = new StringWriter();
        RDFDataMgr.write(sw, plantedContainerMetadataGraph, Lang.TTL);

        plantedContainerMetadataResource.setBody(sw.toString());

        // Write the updates back to the resource
        resourceAccessor.updateResource(shapeTreeContext, plantedContainerMetadataResource);

        List<URI> nestedContainersCreated = new ArrayList<>();

        // Recursively call plantShapeTree for any static, nested container contents -- resources and dynamically named containers are ignored
        for (URI contentShapeTreeURI : shapeTree.getContains()) {
            ShapeTree contentShapeTree = ShapeTreeFactory.getShapeTree(contentShapeTreeURI);
            if (contentShapeTree != null && contentShapeTree.getLabel() != null) {
                ShapeTreePlantResult nestedResult = plantShapeTree(shapeTreeContext, plantedContainerResource, null, TEXT_TURTLE, rootShapeTree, rootContainer, contentShapeTree, contentShapeTree.getLabel());
                nestedContainersCreated.add(nestedResult.getRootContainer());
            }
        }

        return new ShapeTreePlantResult(shapeTree.getURI(), plantedContainerResource.getUri(), plantedContainerMetadataResource.getUri(), nestedContainersCreated);
    }

    private ShapeTreeResource createOrReuseContainer(ShapeTreeContext shapeTreeContext, URI parentContainerURI, String requestedName, String body, String contentType) throws IOException {
        // First determine if we're looking to plant a ShapeTree in an existing container
        ShapeTreeResource targetContainerResource = this.resourceAccessor.getResource(shapeTreeContext, URI.create(parentContainerURI.toString() + requestedName));
        if (targetContainerResource.isExists()) {
            // If the container already exists, it will not be created again
            return targetContainerResource;
        } else {
            // Create new container with the Slug/Requested Name
            Map<String, List<String>> headers = new HashMap<>();
            headers.put(HttpHeaders.SLUG.getValue(), List.of(requestedName));
            headers.put(HttpHeaders.LINK.getValue(), List.of(REL_TYPE_CONTAINER));
            headers.put(HttpHeaders.CONTENT_TYPE.getValue(), List.of(contentType));
            ShapeTreeResource shapeTreeContainerResource = this.resourceAccessor.createResource(shapeTreeContext, parentContainerURI, headers, body, contentType);

            // Depending on server implementation, after a POST the response header may pertain to the parent container (the URI)
            // as opposed to the newly created resource.  To ensure we get the proper headers, we reload the contents of the
            // newly created container with a GET.
            shapeTreeContainerResource = this.resourceAccessor.getResource(shapeTreeContext, shapeTreeContainerResource.getUri());
            return shapeTreeContainerResource;
        }
    }
}
