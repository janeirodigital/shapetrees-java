package com.janeirodigital.shapetrees.client;

import com.janeirodigital.shapetrees.RemoteResource;
import com.janeirodigital.shapetrees.ShapeTreeEcosystem;
import com.janeirodigital.shapetrees.ShapeTreeException;
import com.janeirodigital.shapetrees.ShapeTreeFactory;
import com.janeirodigital.shapetrees.enums.HttpHeaders;
import com.janeirodigital.shapetrees.enums.Namespaces;
import com.janeirodigital.shapetrees.helper.GraphHelper;
import com.janeirodigital.shapetrees.helper.HttpClientHelper;
import com.janeirodigital.shapetrees.helper.HttpHeaderHelper;
import com.janeirodigital.shapetrees.model.ShapeTreePlantResult;
import com.janeirodigital.shapetrees.model.ShapeTreeStep;
import lombok.extern.slf4j.Slf4j;
import okhttp3.*;
import okio.Buffer;
import org.apache.commons.lang3.StringUtils;
import org.apache.jena.graph.Graph;
import org.apache.jena.graph.GraphUtil;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Triple;
import org.apache.jena.rdf.model.ModelFactory;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

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
    protected ShapeTreeEcosystem ecosystem;

    protected static final String LDP_CONTAINER = "http://www.w3.org/ns/ldp#Container";
    private static final String REL_DESCRIBEDBY = "describedby";
    protected static final String REL_TYPE = "type";
    private static final String REL_TYPE_CONTAINER = "<" + LDP_CONTAINER + ">; rel=\"" + REL_TYPE + "\"";
    protected static final String SHAPE_TREE_ROOT_PREDICATE = Namespaces.SHAPETREE_NAMESPACE.getValue() + "shapeTreeRoot";
    protected static final String SHAPE_TREE_INSTANCE_PATH_PREDICATE = Namespaces.SHAPETREE_NAMESPACE.getValue() + "shapeTreeInstancePath";
    protected static final String SHAPE_TREE_INSTANCE_ROOT_PREDICATE = Namespaces.SHAPETREE_NAMESPACE.getValue() + "shapeTreeInstanceRoot";
    protected static final String SHAPE_TREE_STEP_PREDICATE = Namespaces.SHAPETREE_NAMESPACE.getValue() + "shapeTreeStep";
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

        // Set the incoming content type
        if (this.incomingRequestHeaders.containsKey(HttpHeaders.CONTENT_TYPE.getValue())) {
            this.incomingRequestContentType = this.incomingRequestHeaders.get(HttpHeaders.CONTENT_TYPE.getValue()).stream().findFirst().orElse(null);
        } else {
            incomingRequestContentType = null;
        }

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

    protected String getIncomingLinkHeaderByRelationValue(String relation) {
        if (this.incomingRequestLinkHeaders.containsKey(relation)) {
            return this.incomingRequestLinkHeaders.get(relation).stream().findFirst().orElse(null);
        }
        return null;
    }

    protected Graph getIncomingBodyGraph(URI baseURI) throws ShapeTreeException {
        if (this.incomingRequestBody != null && this.incomingRequestBody.length() > 0) {
            return GraphHelper.readStringIntoGraph(this.incomingRequestBody, baseURI, this.incomingRequestContentType);
        }
        return null;
    }

    protected static Response createPlantResponse(ShapeTreePlantResult plantResult, Request request) {
        return new Response.Builder()
                .code(201)
                .addHeader(HttpHeaders.LOCATION.getValue(), plantResult.getRootContainer().toString())
                .addHeader(HttpHeaders.LINK.getValue(), "<" + plantResult.getRootContainerMetadata().toString() + ">; rel=\"" + REL_DESCRIBEDBY + "\"")
                .addHeader(HttpHeaders.CONTENT_TYPE.getValue(), "text/turtle")
                .body(ResponseBody.create("", MediaType.get("text/turtle")))
                .request(request)
                .protocol(Protocol.HTTP_1_1)
                .message("Created")
                .build();
    }

    protected static ShapeTreePlantResult plantShapeTree(String authorizationHeaderValue, RemoteResource parentContainer, String incomingBody, ShapeTreeStep rootShapeTreeStep, ShapeTreeStep shapeTreeStep, String requestedName, String shapeTreePath, int depth) throws IOException, URISyntaxException {
        log.debug("plantShapeTree: parent [{}], root step [{}], step [{}], slug [{}], path [{}], depth [{}]", parentContainer.getURI(), rootShapeTreeStep.getId(), shapeTreeStep.getId(), requestedName, shapeTreePath, depth);

        // Create new container with the Slug/Requested Name
        RemoteResource shapeTreeContainer = createContainer(authorizationHeaderValue, parentContainer.getURI(), requestedName, incomingBody);
        // TODO!!!
        // TODO!!!  This next line is a work around.  As of 6/25/2020, ESS does not return the proper headers
        // TODO!!!  After creating a container, as a result, we are GETting the same URI again so we can get
        // TODO!!!  the appropriate headers.  For example, after creating a container the response headers
        // TODO!!!  include the acl/effectiveAcl headers for the parent container, not the newly created one
        // TODO!!!
        shapeTreeContainer = new RemoteResource(shapeTreeContainer.getURI(), authorizationHeaderValue);


        String metaDataURIString = getMetadataResourceURI(shapeTreeContainer);
        RemoteResource shapeTreeContainerMetadataResource = new RemoteResource(metaDataURIString, authorizationHeaderValue);

        // Get the existing graph
        Graph shapeTreeContainerMetadataGraph;
        if (shapeTreeContainerMetadataResource.exists()) {
            shapeTreeContainerMetadataGraph = shapeTreeContainerMetadataResource.getGraph();

            // Remove any previous triples for the shapetree planting metadata
            GraphUtil.remove(shapeTreeContainerMetadataGraph, NodeFactory.createURI(metaDataURIString), NodeFactory.createURI(SHAPE_TREE_ROOT_PREDICATE), null);
            GraphUtil.remove(shapeTreeContainerMetadataGraph, NodeFactory.createURI(metaDataURIString), NodeFactory.createURI(SHAPE_TREE_INSTANCE_PATH_PREDICATE), null);
            GraphUtil.remove(shapeTreeContainerMetadataGraph, NodeFactory.createURI(metaDataURIString), NodeFactory.createURI(SHAPE_TREE_INSTANCE_ROOT_PREDICATE), null);
            GraphUtil.remove(shapeTreeContainerMetadataGraph, NodeFactory.createURI(metaDataURIString), NodeFactory.createURI(SHAPE_TREE_STEP_PREDICATE), null);
        }

        shapeTreeContainerMetadataGraph = ModelFactory.createDefaultModel().getGraph();

        List<Triple> triplesToAdd = new ArrayList<>();
        triplesToAdd.add(new Triple(NodeFactory.createURI(metaDataURIString), NodeFactory.createURI(SHAPE_TREE_STEP_PREDICATE), NodeFactory.createURI(shapeTreeStep.getId())));
        triplesToAdd.add(new Triple(NodeFactory.createURI(metaDataURIString), NodeFactory.createURI(SHAPE_TREE_INSTANCE_PATH_PREDICATE), NodeFactory.createLiteral(shapeTreePath)));
        String relativePath = (depth==0) ? "./" : StringUtils.repeat("../", depth);
        triplesToAdd.add(new Triple(NodeFactory.createURI(metaDataURIString), NodeFactory.createURI(SHAPE_TREE_INSTANCE_ROOT_PREDICATE), NodeFactory.createURI(relativePath)));
        triplesToAdd.add(new Triple(NodeFactory.createURI(metaDataURIString), NodeFactory.createURI(SHAPE_TREE_ROOT_PREDICATE), NodeFactory.createURI(rootShapeTreeStep.getId())));
        GraphUtil.add(shapeTreeContainerMetadataGraph, triplesToAdd);
        // Write the updates back to the resource
        shapeTreeContainerMetadataResource.updateGraph(shapeTreeContainerMetadataGraph,false, authorizationHeaderValue);

        List<URI> nestedContainersCreated = new ArrayList<>();

        depth++;
        // Recursively call plantShapeTree for any static, nested container contents -- resources and dynamically named containers are ignored
        for (URI contentStepURI : shapeTreeStep.getContents()) {
            ShapeTreeStep contentStep = ShapeTreeFactory.getShapeTreeStep(contentStepURI);
            if (contentStep.getLabel() != null) {
                // the return URI is discarded for recursive calls
                // Add a trailing slash so recursion lines up nicely to paths
                if (shapeTreePath.equals(".")) shapeTreePath = "./";
                ShapeTreePlantResult nestedResult = plantShapeTree(authorizationHeaderValue, shapeTreeContainer, null, rootShapeTreeStep, contentStep, contentStep.getLabel(), shapeTreePath + contentStep.getLabel() +"/", depth);
                nestedContainersCreated.add(nestedResult.getRootContainer());
            }
        }

        return new ShapeTreePlantResult(shapeTreeContainer.getURI(), shapeTreeContainerMetadataResource.getURI(), nestedContainersCreated);
    }

    private static RemoteResource createContainer(String authorizationHeaderValue, URI parentURI, String requestedName, String incomingBody) throws IOException, URISyntaxException {
        log.debug("createContainer: parent [{}], slug [{}]", parentURI, requestedName);

        if (incomingBody == null) {
            incomingBody = "";
        }

        OkHttpClient httpClient = HttpClientHelper.getClient(true);
        Request createContainerPost = new Request.Builder()
                .addHeader(HttpHeaders.SLUG.getValue(), requestedName)
                .addHeader(HttpHeaders.LINK.getValue(), REL_TYPE_CONTAINER)
                .addHeader(HttpHeaders.CONTENT_TYPE.getValue(), "text/turtle")
                .addHeader(HttpHeaders.AUTHORIZATION.getValue(), authorizationHeaderValue)
                .post(RequestBody.create(incomingBody, MediaType.get("text/turtle")))
                .url(parentURI.toURL()).build();

        Response response = httpClient.newCall(createContainerPost).execute();
        return new RemoteResource(response);
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

        return shapeTreeContainer.getURI() + ".meta";
    }

    protected static ShapeTreeStep getShapeTreeStepFromGraphByPredicate(Graph graph, String predicate) throws ShapeTreeException, URISyntaxException {
        List<Triple> triples = graph.find(null, NodeFactory.createURI(predicate), null).toList();
        if (triples == null || triples.size() == 0) {
            throw new ShapeTreeException(500, "No triples containing " + predicate + " - one expected");
        }
        if (triples.size() > 1) {
            throw new ShapeTreeException(500, "Multiple triples containing " + predicate + " - only one expected");
        }
        String stepURI = triples.get(0).getObject().getURI();
        return ShapeTreeFactory.getShapeTreeStep(new URI(stepURI));
    }

    protected static String getValueFromGraphByPredicate(Graph graph, String predicate) throws ShapeTreeException, URISyntaxException {
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















}
