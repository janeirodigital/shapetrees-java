package com.janeirodigital.shapetrees.client.okhttp;

import com.janeirodigital.shapetrees.client.core.ShapeTreeClient;
import com.janeirodigital.shapetrees.client.core.ShapeTreeClientConfiguration;
import com.janeirodigital.shapetrees.core.ShapeTreeResponse;
import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.core.enums.LinkRelations;
import com.janeirodigital.shapetrees.core.helpers.GraphHelper;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;
import com.janeirodigital.shapetrees.core.models.ShapeTreeLocator;
import lombok.extern.slf4j.Slf4j;
import okhttp3.*;
import org.apache.jena.graph.Graph;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;

@Slf4j
public class ShapeTreeClientImpl implements ShapeTreeClient {

    private boolean skipValidation = false;
    private final ShapeTreeClientConfiguration validatingClientConfig;
    private final ShapeTreeClientConfiguration nonValidatingClientConfig;

    public ShapeTreeClientImpl() {
        this.validatingClientConfig = new ShapeTreeClientConfiguration(true, false);
        this.nonValidatingClientConfig = new ShapeTreeClientConfiguration(false, false);
    }

    @Override
    public boolean isSkipValidation() {
        return skipValidation;
    }

    @Override
    public void setSkipValidation(boolean skipValidation) {
        this.skipValidation = skipValidation;
    }

    @Override
    public List<ShapeTreeLocator> discoverShapeTree(ShapeTreeContext context, URI targetContainer) throws IOException {
        log.debug("Discovering Shape Trees present at {}", targetContainer);
        RemoteResource targetContainerResource = new RemoteResource(targetContainer, context.getAuthorizationHeaderValue());
        RemoteResource targetContainerMetadataResource = targetContainerResource.getMetadataResource(context.getAuthorizationHeaderValue());
        return ShapeTreeLocator.getShapeTreeLocatorsFromGraph(targetContainerMetadataResource.getGraph(targetContainerResource.getURI()));
    }

    @Override
    public URI plantShapeTree(ShapeTreeContext context, URI parentContainer, List<URI> shapeTreeURIs, String focusNode, URI shapeTreeHint, String proposedResourceName, Graph bodyGraph) throws IOException, URISyntaxException {
        StringBuilder shapeTreeCommaDelimited = new StringBuilder();
        if (shapeTreeURIs != null) {
            for(URI shapeTreeURI : shapeTreeURIs) {
                shapeTreeCommaDelimited.append(",").append(shapeTreeURI);
            }
        }

        log.debug("Planting shape tree [Parent container={}], [Shape Trees={}], [FocusNode={}], [ShapeTreeHint={}], [ProposedResourceName={}]", parentContainer, shapeTreeCommaDelimited.toString(), focusNode, shapeTreeHint, proposedResourceName);
        String turtleString = GraphHelper.writeGraphToTurtleString(bodyGraph);
        return plantShapeTree(context, parentContainer, shapeTreeURIs, focusNode, shapeTreeHint, proposedResourceName, turtleString, "text/turtle");
    }

    @Override
    public URI plantShapeTree(ShapeTreeContext context, URI parentContainer, List<URI> shapeTreeURIs, String focusNode, URI shapeTreeHint, String proposedResourceName, String bodyString, String contentType) throws IOException, URISyntaxException {

        OkHttpClient client = ShapeTreeHttpClientHolder.getForConfig(getConfiguration(this.skipValidation));

        byte[] bytes = new byte[]{};
        if (bodyString != null) {
            bytes = bodyString.getBytes();
        }

        Request.Builder builder = new Request.Builder()
                .url(parentContainer.toString());

        for (URI shapeTreeUri : shapeTreeURIs) {
            builder.addHeader(HttpHeaders.LINK.getValue(), "<" + shapeTreeUri.toString() + ">; rel=\"" + LinkRelations.SHAPETREE.getValue() + "\"");
        }

        applyCommonHeaders(context, builder, focusNode, shapeTreeHint, true, proposedResourceName, contentType);

        Request plantPost = builder
                .post(RequestBody.create(bytes))
                .build();

        Response response = client.newCall(plantPost).execute();
        if (response.isSuccessful()) {
            String locationHeader = response.header(HttpHeaders.LOCATION.getValue());
            if (locationHeader != null) {
                return new URI(locationHeader);
            } else {
                throw new IOException(response.code() + " No Location Header provided");
            }
        } else {
            String responseBodyString = null;
            try (ResponseBody body = response.body()) {
                if (body != null) {
                    responseBodyString = body.string();
                    body.close();
                }
            }
            throw new IOException(response.code() + " " + responseBodyString);
        }
    }

    @Override
    public ShapeTreeResponse createDataInstance(ShapeTreeContext context, URI parentContainer, String focusNode, URI shapeTreeHint, String proposedResourceName, Boolean isContainer, String bodyString, String contentType) throws IOException {
        log.debug("Creating data instance {} in {} with hint {}", parentContainer, proposedResourceName, shapeTreeHint);
        OkHttpClient client = ShapeTreeHttpClientHolder.getForConfig(getConfiguration(this.skipValidation));

        byte[] bytes = new byte[]{};
        if (bodyString != null) {
            bytes = bodyString.getBytes();
        }

        String resourceURI = parentContainer.toString();
        if (!resourceURI.endsWith("/")) {
            resourceURI += "/";
        }
        resourceURI += proposedResourceName;
        log.debug("Build Resource URI {}", resourceURI);

        Request.Builder putBuilder = new Request.Builder()
                .url(resourceURI)
                .put(RequestBody.create(bytes));

        // proposed resource is name is nulled since a Slug will not be used
        applyCommonHeaders(context, putBuilder, focusNode, shapeTreeHint, isContainer, null, contentType);

        return OkHttpHelper.mapOkHttpResponseToShapeTreeResponse(client.newCall(putBuilder.build()).execute());
    }

    @Override
    public ShapeTreeResponse updateDataInstance(ShapeTreeContext context, URI resourceURI, String focusNode, URI shapeTreeHint, String bodyString, String contentType) throws IOException {
        OkHttpClient client = ShapeTreeHttpClientHolder.getForConfig(getConfiguration(this.skipValidation));

        byte[] bytes = new byte[]{};
        if (bodyString != null) {
            bytes = bodyString.getBytes();
        }

        Request.Builder putBuilder = new Request.Builder()
                .url(resourceURI.toString())
                .put(RequestBody.create(bytes));

        applyCommonHeaders(context, putBuilder, focusNode, shapeTreeHint, null, null, contentType);

        return OkHttpHelper.mapOkHttpResponseToShapeTreeResponse(client.newCall(putBuilder.build()).execute());
    }

    @Override
    public ShapeTreeResponse updateDataInstanceWithPatch(ShapeTreeContext context, URI resourceURI, String focusNode, URI shapeTreeHint, String bodyString, String contentType) throws IOException {
        OkHttpClient client = ShapeTreeHttpClientHolder.getForConfig(getConfiguration(this.skipValidation));

        byte[] sparqlUpdateBytes = bodyString.getBytes();

        Request.Builder patchBuilder = new Request.Builder()
                .url(resourceURI.toString())
                .patch(RequestBody.create(sparqlUpdateBytes));

        applyCommonHeaders(context, patchBuilder, focusNode, shapeTreeHint, null, null, contentType);

        return OkHttpHelper.mapOkHttpResponseToShapeTreeResponse(client.newCall(patchBuilder.build()).execute());
    }

    @Override
    public ShapeTreeResponse deleteDataInstance(ShapeTreeContext context, URI resourceURI, URI shapeTreeURI) throws IOException {
        OkHttpClient client = ShapeTreeHttpClientHolder.getForConfig(getConfiguration(this.skipValidation));

        Request.Builder deleteBuilder = new Request.Builder()
                .url(resourceURI.toString())
                .delete();

        applyCommonHeaders(context, deleteBuilder, null, shapeTreeURI, null, null, null);

        return OkHttpHelper.mapOkHttpResponseToShapeTreeResponse(client.newCall(deleteBuilder.build()).execute());
    }

    @Override
    public void unplantShapeTree(ShapeTreeContext context, URI containerURI, URI shapeTreeURI) {

    }

    private ShapeTreeClientConfiguration getConfiguration(boolean skipValidation) {
        if (skipValidation) {
            return this.nonValidatingClientConfig;
        } else {
            return this.validatingClientConfig;
        }
    }

    private void applyCommonHeaders(ShapeTreeContext context, Request.Builder builder,  String focusNode, URI shapeTreeHint, Boolean isContainer, String proposedResourceName, String contentType) {

        if (context.getAuthorizationHeaderValue() != null) {
            builder.addHeader(HttpHeaders.AUTHORIZATION.getValue(), context.getAuthorizationHeaderValue());
        }

        if (isContainer != null) {
            String resourceTypeUri = isContainer ? "http://www.w3.org/ns/ldp#Container" : "http://www.w3.org/ns/ldp#Resource";
            builder.addHeader(HttpHeaders.LINK.getValue(), "<" + resourceTypeUri + ">; rel=\"type\"");
        }

        if (focusNode != null) {
            builder.addHeader(HttpHeaders.LINK.getValue(), "<" + focusNode + ">; rel=\"" + LinkRelations.FOCUS_NODE.getValue() + "\"");
        }

        if (shapeTreeHint != null) {
            builder.addHeader(HttpHeaders.LINK.getValue(), "<" + shapeTreeHint + ">; rel=\"" + LinkRelations.TARGET_SHAPETREE.getValue() + "\"");
        }

        if (proposedResourceName != null) {
            builder.addHeader(HttpHeaders.SLUG.getValue(), proposedResourceName);
        }

        if (contentType != null) {
            builder.addHeader(HttpHeaders.CONTENT_TYPE.getValue(), contentType);
        }

        if (context.getWebID() != null) {
            builder.addHeader(HttpHeaders.INTEROP_WEBID.getValue(), context.getWebID());
        }

        if (context.getOriginatorIRI() != null) {
            builder.addHeader(HttpHeaders.INTEROP_ORIGINATOR.getValue(), context.getOriginatorIRI());
        }
    }
}
