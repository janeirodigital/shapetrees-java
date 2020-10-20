package com.janeirodigital.shapetrees.client;

import com.janeirodigital.shapetrees.*;
import com.janeirodigital.shapetrees.enums.HttpHeaders;
import com.janeirodigital.shapetrees.enums.LinkRelations;
import com.janeirodigital.shapetrees.helper.GraphHelper;
import com.janeirodigital.shapetrees.helper.HttpHeaderHelper;
import com.janeirodigital.shapetrees.model.*;
import lombok.extern.slf4j.Slf4j;
import okhttp3.*;
import okio.Buffer;
import org.apache.jena.graph.Graph;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.*;

@Slf4j
public abstract class AbstractValidatingMethodHandler {
    protected final Interceptor.Chain chain;
    protected final Request request;
    protected final String authorizationHeaderValue;
    protected final RemoteResource requestRemoteResource;
    protected final Map<String, List<String>> incomingRequestHeaders;
    protected final Map<String, List<String>> incomingRequestLinkHeaders;
    protected String incomingRequestContentType = null;
    protected final String incomingRequestBody;
    protected final boolean isIncomingNonRdfSource;
    protected final Set<String> supportedRDFContentTypes;
    protected ShapeTreeEcosystem ecosystem;

    public AbstractValidatingMethodHandler(Interceptor.Chain chain, ShapeTreeEcosystem ecosystem) throws IOException {
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

        if (!this.request.method().equals("DELETE")) {
            // Set the incoming content type
            if (this.incomingRequestHeaders.containsKey(HttpHeaders.CONTENT_TYPE.getValue())) {
                this.incomingRequestContentType = this.incomingRequestHeaders.get(HttpHeaders.CONTENT_TYPE.getValue()).stream().findFirst().orElse(null);
            }
            // Ensure a content-type is present
            if (this.incomingRequestContentType == null) {
                throw new ShapeTreeException(400, "Content-Type is required");
            }

            this.isIncomingNonRdfSource = determineIsNonRdfSource(this.incomingRequestContentType);

            // Set the incoming request body
            Buffer buffer = new Buffer();
            if (this.request.body() != null) {
                this.request.body().writeTo(buffer);
            }

            this.incomingRequestBody = buffer.readUtf8();
        } else {
            this.incomingRequestBody = null;
            this.isIncomingNonRdfSource = false;
        }
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
        log.info("Reading request body into graph with baseURI {}", baseURI);
        if (!this.isIncomingNonRdfSource && this.incomingRequestBody != null && this.incomingRequestBody.length() > 0) {
            return GraphHelper.readStringIntoGraph(this.incomingRequestBody, baseURI, this.incomingRequestContentType);
        }
        return null;
    }

    protected URI getIncomingResolvedFocusNode(URI baseURI) throws IOException {
        if (this.incomingRequestLinkHeaders.get(LinkRelations.FOCUS_NODE.getValue()) != null) {
            String focusNode = this.incomingRequestLinkHeaders.get(LinkRelations.FOCUS_NODE.getValue()).get(0);
            return baseURI.resolve(focusNode);
        } else {
            throw new ShapeTreeException(400, "No Link header with relation " + LinkRelations.FOCUS_NODE.getValue() + " supplied, unable to perform Shape validation");
        }
    }

    protected URI getIncomingTargetShapeTreeHint() throws URISyntaxException {
        if (this.incomingRequestLinkHeaders.get(LinkRelations.TARGET_SHAPETREE.getValue()) != null) {
            return new URI(this.incomingRequestLinkHeaders.get(LinkRelations.TARGET_SHAPETREE.getValue()).get(0));
        }
        return null;
    }

    protected static Response createPlantResponse(List<ShapeTreePlantResult> plantResults, Request request, Map<String, List<String>> linkHeaders) {

        // As multiple ShapeTrees can be planted at once, if there is more than ShapeTree relation Link header,
        // the response to the POST will be for the ShapeTree that was requested first.
        ShapeTreePlantResult primaryPlantResult = null;
        if (plantResults.size() > 1) {
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

        if (primaryPlantResult == null) {
            log.error("Unable to find 'primary' plant result in createPlantResponse");
        }

        return new Response.Builder()
                .code(201)
                .addHeader(HttpHeaders.LOCATION.getValue(), primaryPlantResult.getRootContainer().toString())
                .addHeader(HttpHeaders.LINK.getValue(), "<" + primaryPlantResult.getRootContainerMetadata().toString() + ">; rel=\"" + LinkRelations.SHAPETREE.getValue() + "\"")
                .addHeader(HttpHeaders.CONTENT_TYPE.getValue(), "text/turtle")
                .body(ResponseBody.create("", MediaType.get("text/turtle")))
                .request(request)
                .protocol(Protocol.HTTP_1_1)
                .message("Created")
                .build();
    }

    protected boolean determineIsNonRdfSource(String incomingRequestContentType) {
        return !this.supportedRDFContentTypes.contains(incomingRequestContentType.toLowerCase());
    }

    protected URI getParentContainerURI() throws IOException {
        return this.requestRemoteResource.getURI().resolve(requestRemoteResource.isContainer() ? ".." : ".");
    }

    protected String getRequestResourceName() throws IOException {
        return requestRemoteResource.getURI().toString().replace(getParentContainerURI().toString(), "");
    }



    protected ShapeTree getShapeTreeWithShapeURI(List<ShapeTree> shapeTreesToPlant) {
        for (ShapeTree shapeTree : shapeTreesToPlant) {
            if (shapeTree.getValidatedByShapeUri() != null) {
                return shapeTree;
            }
        }
        return null;
    }

    protected ShapeTree getShapeTreeWithContents(List<ShapeTree> shapeTreesToPlant) {
        for (ShapeTree shapeTree : shapeTreesToPlant) {
            if (shapeTree.getContains() != null && shapeTree.getContains().size() > 0) {
                return shapeTree;
            }
        }
        return null;
    }

    protected ShapeTree getShapeTreeWithContentsFromShapeTreeLocators(List<ShapeTreeLocator> shapeTreeLocators) throws URISyntaxException, ShapeTreeException {
        List<ShapeTree> existingShapeTrees = new ArrayList<>();
        for (ShapeTreeLocator locator : shapeTreeLocators) {
            existingShapeTrees.add(ShapeTreeFactory.getShapeTree(new URI(locator.getShapeTree())));
        }

        for (ShapeTree shapeTree : existingShapeTrees) {
            if (shapeTree.getContains() != null && shapeTree.getContains().size() > 0) {
                return shapeTree;
            }
        }
        return null;
    }


    protected ValidationContext validateAgainstParentContainer(Graph graphToValidate, URI baseURI, RemoteResource parentContainer, String resourceName, Boolean isAContainer) throws IOException, URISyntaxException {
        RemoteResource parentContainerMetadata = parentContainer.getMetadataResource(this.authorizationHeaderValue);
        // If there is no metadata for the parent container, it is not managed
        if (!parentContainerMetadata.exists()) return null;

        List<ShapeTreeLocator> locators = ShapeTreeLocator.getShapeTreeLocatorsFromGraph(parentContainerMetadata.getGraph(parentContainer.getURI()));

        // If there are no ShapeTree locators in the metadata graph, it is not managed
        if (locators == null || locators.size() == 0) return null;

        // This means the existing parent container has one or more ShapeTrees associated with it
        List<ShapeTree> existingShapeTrees = new ArrayList<>();
        for (ShapeTreeLocator locator : locators) {
            existingShapeTrees.add(ShapeTreeFactory.getShapeTree(new URI(locator.getShapeTree())));
        }

        ShapeTree shapeTreeWithContents = getShapeTreeWithContents(existingShapeTrees);

        URI targetShapeTreeHint = getIncomingTargetShapeTreeHint();
        ShapeTree targetShapeTree = shapeTreeWithContents.findMatchingContainsShapeTree(resourceName, targetShapeTreeHint, isAContainer, this.isIncomingNonRdfSource);

        // If no targetShapeTree is returned, it can be assumed that no validation is required
        ValidationResult validationResult = null;
        if (targetShapeTree != null) {

            // If there is a graph to validate...and a ShapeTree indicates it wants to validate the container body
            if (graphToValidate != null && targetShapeTree.getValidatedByShapeUri() != null) {
                // ...and a focus node was provided via the focusNode header, then we perform our validation
                URI focusNodeURI = getIncomingResolvedFocusNode(baseURI);
                log.info("Validating against parent container.  ST with Contents {}, Focus Node {}", shapeTreeWithContents.getURI(), focusNodeURI);
                validationResult = targetShapeTree.validateContent(graphToValidate, focusNodeURI, isAContainer);
            }

            // If there is a body graph and it did not pass validation, return an error
            if (graphToValidate != null && validationResult != null && !validationResult.getValid()) {
                throw new ShapeTreeException(422, "Payload did not meet requirements defined by ShapeTree " + targetShapeTree.getURI());
            }
        }

        return new ValidationContext(targetShapeTree, validationResult, locators);
    }
}
