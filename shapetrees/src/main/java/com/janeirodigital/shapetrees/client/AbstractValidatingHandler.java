package com.janeirodigital.shapetrees.client;

import com.janeirodigital.shapetrees.*;
import com.janeirodigital.shapetrees.enums.HttpHeaders;
import com.janeirodigital.shapetrees.helper.GraphHelper;
import com.janeirodigital.shapetrees.helper.HttpClientHelper;
import com.janeirodigital.shapetrees.helper.HttpHeaderHelper;
import com.janeirodigital.shapetrees.model.*;
import lombok.extern.slf4j.Slf4j;
import okhttp3.*;
import okio.Buffer;
import org.apache.commons.lang3.StringUtils;
import org.apache.jena.graph.Graph;
import org.apache.jena.graph.GraphUtil;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Triple;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.io.StringWriter;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.*;

@Slf4j
public abstract class AbstractValidatingHandler {
    protected final Interceptor.Chain chain;
    protected final Request request;
    protected final String authorizationHeaderValue;
    protected final RemoteResource requestRemoteResource;
    protected final Map<String, List<String>> incomingRequestHeaders;
    protected final Map<String, List<String>> incomingRequestLinkHeaders;
    protected final String incomingRequestContentType;
    protected final String incomingRequestBody;
    protected final boolean isNonRdfSource;
    protected final Set<String> supportedRDFContentTypes;
    protected ShapeTreeEcosystem ecosystem;

    protected static final String LDP_CONTAINER = "http://www.w3.org/ns/ldp#Container";
    private static final String REL_DESCRIBEDBY = "describedby";
    protected static final String REL_TYPE = "type";
    protected static final String REL_SHAPE_TREE = "ShapeTree";
    private static final String REL_TYPE_CONTAINER = "<" + LDP_CONTAINER + ">; rel=\"" + REL_TYPE + "\"";
    protected static final String FOCUS_NODE = "focusNode";



    public AbstractValidatingHandler(Interceptor.Chain chain, ShapeTreeEcosystem ecosystem) throws IOException {
        this.chain = chain;
        this.ecosystem = ecosystem;
        this.request = chain.request();

        // Extract the authorization header so it can be used on other requests
        this.authorizationHeaderValue = this.request.header(HttpHeaders.AUTHORIZATION.getValue());

        // Dereference incoming URI
        this.requestRemoteResource = new RemoteResource(this.request.url().uri(), this.authorizationHeaderValue);

        // Parse the incoming headers
        this.incomingRequestHeaders = this.request.headers().toMultimap();

        // Parse the link headers
        this.incomingRequestLinkHeaders = HttpHeaderHelper.parseLinkHeadersToMap(this.request.headers(HttpHeaders.LINK.getValue()));

        this.supportedRDFContentTypes = Collections.unmodifiableSet(Set.of("text/turtle", "application/rdf+xml", "application/n-triples", "application/ld+json"));

        // Set the incoming content type
        if (this.incomingRequestHeaders.containsKey(HttpHeaders.CONTENT_TYPE.getValue())) {
            this.incomingRequestContentType = this.incomingRequestHeaders.get(HttpHeaders.CONTENT_TYPE.getValue()).stream().findFirst().orElse(null);
        } else {
            throw new ShapeTreeException(400, "Content-Type is required");
        }

        this.isNonRdfSource = determineIsNonRdfSource(this.incomingRequestContentType);

        // Set the incoming request body
        Buffer buffer = new Buffer();
        request.body().writeTo(buffer);
        this.incomingRequestBody = buffer.readUtf8();
    }

    protected void ensureRequestResourceExists(String message) throws ShapeTreeException {
        if (!this.requestRemoteResource.exists()) {
            throw new ShapeTreeException(404, message);
        }
    }

    protected String getIncomingHeaderValueWithDefault(String headerName, String defaultValue) {
        if (this.incomingRequestHeaders.containsKey(headerName)) {
            return this.incomingRequestHeaders.get(headerName).stream().findFirst().orElse(defaultValue);
        } else {
            return defaultValue;
        }
    }

    protected String getFirstIncomingLinkHeaderByRelationValue(String relation) {
        if (this.incomingRequestLinkHeaders.containsKey(relation)) {
            return this.incomingRequestLinkHeaders.get(relation).stream().findFirst().orElse(null);
        }
        return null;
    }

    protected List<String> getIncomingLinkHeaderByRelationValue(String relation) {
        if (this.incomingRequestLinkHeaders.containsKey(relation)) {
            return this.incomingRequestLinkHeaders.get(relation);
        }
        return null;
    }

    protected URI normalizeBaseURI(URI uri, String requestedName, Boolean isContainer) throws URISyntaxException {
        String uriString = uri.toString();
        if (requestedName != null) {
            uriString += requestedName;
        }
        if (isContainer && !uriString.endsWith("/")) {
            uriString += "/";
        }
        return new URI(uriString);
    }

    protected Graph getIncomingBodyGraph(URI baseURI) throws ShapeTreeException {
        if (this.incomingRequestBody != null && this.incomingRequestBody.length() > 0) {
            return GraphHelper.readStringIntoGraph(this.incomingRequestBody, baseURI, this.incomingRequestContentType);
        }
        return null;
    }

    protected URI getIncomingResolvedFocusNode(URI baseURI, boolean throwIfNotFound) throws IOException {
        if (this.incomingRequestLinkHeaders.get(FOCUS_NODE) != null) {
            String focusNode = this.incomingRequestLinkHeaders.get(FOCUS_NODE).get(0);
            return baseURI.resolve(focusNode);
        } else if (throwIfNotFound) {
            throw new ShapeTreeException(400, "No Link header with relation " + FOCUS_NODE + " supplied, unable to perform Shape validation");
        } else {
            return null;
        }
    }

    protected ShapeTreeContext getShapeTreeContext() {
        // TODO - need to not make these values hardcoded
        return new ShapeTreeContext(this.authorizationHeaderValue, "https://auth-agent.example", "https://ldp.local-ess.inrupt.com/aHR0cDovL2RldnNlcnZlcjozMDA4Mi9hdXRoL3JlYWxtcy9tYXN0ZXJiMzg4YmJlMV85ZjYzXzRlYmNfYmEzMF80MWY4ZmJjZmM0NTc/shapetree-testing/profile/id#me");
    }

    protected static Response createPlantResponse(List<ShapeTreePlantResult> plantResults, Request request, Map<String, List<String>> linkHeaders) {

        // As multiple ShapeTrees can be planted at once, if there is more than ShapeTree relation Link header,
        // the response to the POST will be for the ShapeTree that was requested first.
        ShapeTreePlantResult primaryPlantResult = null;
        if (plantResults.size() > 1) {
            if (linkHeaders.containsKey(REL_SHAPE_TREE)) {
                String primaryShapeTreeURI = linkHeaders.get(REL_SHAPE_TREE).get(0);
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

        if (primaryPlantResult == null) {
            log.error("Unable to find 'primary' plant result in createPlantResponse");
        }

        return new Response.Builder()
                .code(201)
                .addHeader(HttpHeaders.LOCATION.getValue(), primaryPlantResult.getRootContainer().toString())
                .addHeader(HttpHeaders.LINK.getValue(), "<" + primaryPlantResult.getRootContainerMetadata().toString() + ">; rel=\"" + REL_DESCRIBEDBY + "\"")
                .addHeader(HttpHeaders.CONTENT_TYPE.getValue(), "text/turtle")
                .body(ResponseBody.create("", MediaType.get("text/turtle")))
                .request(request)
                .protocol(Protocol.HTTP_1_1)
                .message("Created")
                .build();
    }

    protected static ShapeTreePlantResult plantShapeTree(String authorizationHeaderValue, RemoteResource parentContainer, Graph bodyGraph, ShapeTree rootShapeTree, ShapeTree shapeTree, String requestedName, String shapeTreePath, int depth) throws IOException, URISyntaxException {
        StringWriter sw = new StringWriter();
        if (bodyGraph != null) {
            RDFDataMgr.write(sw, bodyGraph, Lang.TURTLE);
        }
        return plantShapeTree(authorizationHeaderValue, parentContainer, sw.toString(), rootShapeTree, shapeTree, requestedName, shapeTreePath, depth);
    }

    protected static ShapeTreePlantResult plantShapeTree(String authorizationHeaderValue, RemoteResource parentContainer, String body, ShapeTree rootShapeTree, ShapeTree shapeTree, String requestedName, String shapeTreePath, int depth) throws IOException, URISyntaxException {
        log.debug("plantShapeTree: parent [{}], root tree [{}], tree [{}], slug [{}], path [{}], depth [{}]", parentContainer.getURI(), rootShapeTree.getId(), shapeTree.getId(), requestedName, shapeTreePath, depth);

        String metaDataURIString;
        RemoteResource plantedContainer;

        // First determine if we're looking to plant a ShapeTree in an existing container
        RemoteResource targetContainer = new RemoteResource(parentContainer.getURI() + requestedName, authorizationHeaderValue);
        if (targetContainer.exists()) {
            // If the container already exists, it will not be created again and we will plan
            // to use the metadata for that existing container for the planting
            metaDataURIString = getMetadataResourceURI(targetContainer);
            plantedContainer = targetContainer;
        } else {
            // Create new container with the Slug/Requested Name
            RemoteResource shapeTreeContainer = createContainer(authorizationHeaderValue, parentContainer.getURI(), requestedName, body);
            // Depending on server implementation, after a POST the response header may pertain to the parent container (the URI)
            // as opposed to the newly created resource.  To ensure we get the proper headers, we reload the contents of the
            // newly created container with a GET.
            shapeTreeContainer = new RemoteResource(shapeTreeContainer.getURI(), authorizationHeaderValue);

            metaDataURIString = getMetadataResourceURI(shapeTreeContainer);
            plantedContainer = shapeTreeContainer;
        }

        RemoteResource shapeTreeContainerMetadataResource = new RemoteResource(metaDataURIString, authorizationHeaderValue);

        // Get the existing graph and reuse it, if possible, if not, create a new graph
        Graph shapeTreeContainerMetadataGraph;
        if (shapeTreeContainerMetadataResource.exists()) {
            shapeTreeContainerMetadataGraph = shapeTreeContainerMetadataResource.getGraph(plantedContainer.getURI());
        } else {
            shapeTreeContainerMetadataGraph = ModelFactory.createDefaultModel().getGraph();
        }

        // Generate a UUID for the ShapeTree
        UUID shapeTreeLocatorUUID = UUID.randomUUID();

        List<Triple> triplesToAdd = new ArrayList<>();
        // Add the triple for the new tree:hasShapeTreeLocator
        String plantedContainerURI = plantedContainer.getURI().toString() + (plantedContainer.getURI().toString().endsWith("/")? "":"/");
        String shapeTreeLocatorURI = plantedContainerURI + "#" + shapeTreeLocatorUUID;
        triplesToAdd.add(new Triple(NodeFactory.createURI(plantedContainerURI), NodeFactory.createURI(ShapeTreeVocabulary.HAS_SHAPE_TREE_LOCATOR), NodeFactory.createURI(shapeTreeLocatorURI)));

        // Add the triples for the locator itself
        triplesToAdd.add(new Triple(NodeFactory.createURI(shapeTreeLocatorURI), NodeFactory.createURI(ShapeTreeVocabulary.HAS_SHAPE_TREE), NodeFactory.createURI(shapeTree.getId())));
        triplesToAdd.add(new Triple(NodeFactory.createURI(shapeTreeLocatorURI), NodeFactory.createURI(ShapeTreeVocabulary.HAS_SHAPE_TREE_INSTANCE_PATH), NodeFactory.createLiteral(shapeTreePath)));
        String relativePath = (depth==0) ? "./" : StringUtils.repeat("../", depth);
        triplesToAdd.add(new Triple(NodeFactory.createURI(shapeTreeLocatorURI), NodeFactory.createURI(ShapeTreeVocabulary.HAS_SHAPE_TREE_INSTANCE_ROOT), NodeFactory.createURI(relativePath)));
        triplesToAdd.add(new Triple(NodeFactory.createURI(shapeTreeLocatorURI), NodeFactory.createURI(ShapeTreeVocabulary.HAS_SHAPE_TREE_ROOT), NodeFactory.createURI(rootShapeTree.getId())));
        GraphUtil.add(shapeTreeContainerMetadataGraph, triplesToAdd);
        // Write the updates back to the resource
        shapeTreeContainerMetadataResource.updateGraph(shapeTreeContainerMetadataGraph,false, authorizationHeaderValue);

        List<URI> nestedContainersCreated = new ArrayList<>();

        depth++;
        // Recursively call plantShapeTree for any static, nested container contents -- resources and dynamically named containers are ignored
        for (URI contentShapeTreeURI : shapeTree.getContents()) {
            ShapeTree contentShapeTree = ShapeTreeFactory.getShapeTree(contentShapeTreeURI);
            if (contentShapeTree.getLabel() != null) {
                // the return URI is discarded for recursive calls
                // Add a trailing slash so recursion lines up nicely to paths
                if (shapeTreePath.equals(".")) shapeTreePath = "./";
                ShapeTreePlantResult nestedResult = plantShapeTree(authorizationHeaderValue, plantedContainer, (String)null, rootShapeTree, contentShapeTree, contentShapeTree.getLabel(), shapeTreePath + contentShapeTree.getLabel() +"/", depth);
                nestedContainersCreated.add(nestedResult.getRootContainer());
            }
        }

        return new ShapeTreePlantResult(shapeTree.getURI(), plantedContainer.getURI(), shapeTreeContainerMetadataResource.getURI(), nestedContainersCreated);
    }

    private static RemoteResource createContainer(String authorizationHeaderValue, URI parentURI, String requestedName, String body) throws IOException, URISyntaxException {
        log.debug("createContainer: parent [{}], slug [{}]", parentURI, requestedName);

        if (body == null) {
            body = "";
        }

        OkHttpClient httpClient = HttpClientHelper.getClient();
        Request createContainerPost = new Request.Builder()
                .addHeader(HttpHeaders.SLUG.getValue(), requestedName)
                .addHeader(HttpHeaders.LINK.getValue(), REL_TYPE_CONTAINER)
                .addHeader(HttpHeaders.CONTENT_TYPE.getValue(), "text/turtle")
                .addHeader(HttpHeaders.AUTHORIZATION.getValue(), authorizationHeaderValue)
                .post(RequestBody.create(body, MediaType.get("text/turtle")))
                .url(parentURI.toURL()).build();

        Response response = httpClient.newCall(createContainerPost).execute();
        return new RemoteResource(response);
    }

    private boolean determineIsNonRdfSource(String incomingRequestContentType) {
        return !this.supportedRDFContentTypes.contains(incomingRequestContentType.toLowerCase());
    }


    @NotNull
    protected static String getMetadataResourceURI(RemoteResource shapeTreeContainer) throws IOException {
        // This header approach is not currently working, instead, we're going to use a separate metadata file
        /*
        String metaDataURIString = shapeTreeContainer.getFirstLinkHeaderValueByRel(REL_DESCRIBEDBY);
        if (metaDataURIString.startsWith("/")) {
            // If the header value doesn't include scheme/host, prefix it with the scheme & host from container
            URI shapeTreeContainerURI = shapeTreeContainer.getURI();
            metaDataURIString = shapeTreeContainerURI.getScheme() + "://" + shapeTreeContainerURI.getHost() + shapeTreeContainer.getFirstLinkHeaderValueByRel(REL_DESCRIBEDBY);
        }*/

        String metaResourceName = ".meta";

        if (shapeTreeContainer.isContainer() && !shapeTreeContainer.getURI().toString().endsWith("/")) {
            metaResourceName = "/" + metaResourceName;
        }

        return shapeTreeContainer.getURI() + metaResourceName;
    }

    protected static String getValueFromGraphByPredicate(Graph graph, String predicate) throws ShapeTreeException {
        List<Triple> triples = graph.find(null, NodeFactory.createURI(predicate), null).toList();
        if (triples == null || triples.size() == 0) {
            throw new ShapeTreeException(500, "No triples containing " + predicate + " - one expected");
        }
        if (triples.size() > 1) {
            throw new ShapeTreeException(500, "Multiple triples containing " + predicate + " - only one expected");
        }
        if (triples.get(0).getObject().isURI()) {
            return triples.get(0).getObject().getURI();
        } else {
            return triples.get(0).getObject().getLiteralValue().toString();
        }
    }

    protected URI getParentContainerURI() throws IOException {
        return this.requestRemoteResource.getURI().resolve(requestRemoteResource.isContainer() ? ".." : ".");
    }

    protected String getRequestResourceName() throws IOException {
        return requestRemoteResource.getURI().toString().replace(getParentContainerURI().toString(), "");
    }

    protected List<ShapeTreeLocator> getShapeTreeLocators(Graph shapeTreeMetadataGraph) {
        List<ShapeTreeLocator> locators = new ArrayList<>();

        List<Triple> hasShapeTreeLocatorTriples = shapeTreeMetadataGraph.find(null, NodeFactory.createURI(ShapeTreeVocabulary.HAS_SHAPE_TREE_LOCATOR), null).toList();
        for (Triple hasShapeTreeLocatorTriple : hasShapeTreeLocatorTriples) {
            String locatorURI = hasShapeTreeLocatorTriple.getObject().getURI();

            List<Triple> locatorTriples = shapeTreeMetadataGraph.find(NodeFactory.createURI(locatorURI), null, null).toList();
            String instancePath = null, shapeTreeRoot = null, rootShapeTree = null, shapeTree = null;
            for (Triple locatorTriple : locatorTriples) {
                if (locatorTriple.getPredicate().getURI().equals(ShapeTreeVocabulary.HAS_SHAPE_TREE_INSTANCE_PATH)) {
                    instancePath = locatorTriple.getObject().getLiteralLexicalForm();
                } else if (locatorTriple.getPredicate().getURI().equals(ShapeTreeVocabulary.HAS_SHAPE_TREE)) {
                    shapeTree = locatorTriple.getObject().getURI();
                } else if (locatorTriple.getPredicate().getURI().equals(ShapeTreeVocabulary.HAS_SHAPE_TREE_INSTANCE_ROOT)) {
                    shapeTreeRoot = locatorTriple.getObject().getURI();
                } else if (locatorTriple.getPredicate().getURI().equals(ShapeTreeVocabulary.HAS_SHAPE_TREE_ROOT)) {
                    rootShapeTree = locatorTriple.getObject().getURI();
                }
            }
            locators.add(new ShapeTreeLocator(rootShapeTree, shapeTree, instancePath, shapeTreeRoot));
        }

        return locators;
    }

    protected ShapeTree getShapeTreeWithShapeURI(List<ShapeTree> shapeTreesToPlant) {
        for (ShapeTree shapeTree : shapeTreesToPlant) {
            if (shapeTree.getShapeUri() != null) {
                return shapeTree;
            }
        }
        return null;
    }

    protected ShapeTree getShapeTreeWithContents(List<ShapeTree> shapeTreesToPlant) {
        for (ShapeTree shapeTree : shapeTreesToPlant) {
            if (shapeTree.getContents() != null && shapeTree.getContents().size() > 0) {
                return shapeTree;
            }
        }
        return null;
    }

    protected List<ShapeTreeLocator> validateAgainstParentContainer(Graph graphToValidate, URI baseURI, RemoteResource parentContainer, String resourceName, Boolean isAContainer) throws IOException, URISyntaxException {

        String parentContainerMetadataURI = getMetadataResourceURI(parentContainer);
        RemoteResource parentContainerMetadata = new RemoteResource(parentContainerMetadataURI, this.authorizationHeaderValue);
        // If there is no metadata for the parent container, it is not managed
        if (!parentContainerMetadata.exists()) return null;

        List<ShapeTreeLocator> locators = getShapeTreeLocators(parentContainerMetadata.getGraph(parentContainer.getURI()));

        // If there are no ShapeTree locators in the metadata graph, it is not managed
        if (locators == null || locators.size() == 0) return null;

        // This means the existing parent container has one or more ShapeTrees associated with it
        List<ShapeTree> existingShapeTrees = new ArrayList<>();
        for (ShapeTreeLocator locator : locators) {
            existingShapeTrees.add(ShapeTreeFactory.getShapeTree(new URI(locator.getShapeTree())));
        }

        ShapeTree shapeTreeWithContents = getShapeTreeWithContents(existingShapeTrees);
        ShapeTree targetShapeTree = shapeTreeWithContents.findMatchingContainsShapeTree(resourceName, isAContainer, this.isNonRdfSource);

        // If no targetShapeTree is returned, it can be assumed that no validation is required
        if (targetShapeTree != null) {
            ValidationResult validationResult = null;
            // If there is a graph to validate...and a ShapeTree indicates it wants to validate the container body
            if (graphToValidate != null && targetShapeTree != null && targetShapeTree.getShapeUri() != null) {
                // ...and a focus node was provided via the focusNode header, then we perform our validation
                URI focusNodeURI = getIncomingResolvedFocusNode(baseURI, true);
                validationResult = targetShapeTree.validateContent(this.authorizationHeaderValue, graphToValidate, focusNodeURI, isAContainer);
            }

            // If there is a body graph and it did not pass validation, return an error
            if (graphToValidate != null && validationResult != null && !validationResult.getValid()) {
                throw new ShapeTreeException(422, "Payload did not meet requirements defined by ShapeTree " + targetShapeTree.getURI());
            }
        }

        return locators;
    }


}
