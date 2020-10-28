package com.janeirodigital.shapetrees.client.impl;

import com.janeirodigital.shapetrees.RemoteResource;
import com.janeirodigital.shapetrees.ShapeTreeEcosystem;
import com.janeirodigital.shapetrees.client.ShapeTreeClient;
import com.janeirodigital.shapetrees.enums.HttpHeaders;
import com.janeirodigital.shapetrees.enums.LinkRelations;
import com.janeirodigital.shapetrees.helper.GraphHelper;
import com.janeirodigital.shapetrees.model.ShapeTreeContext;
import com.janeirodigital.shapetrees.model.ShapeTreeLocator;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import okhttp3.*;
import org.apache.jena.graph.Graph;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;

@Slf4j
public class ShapeTreeClientImpl implements ShapeTreeClient {

    @Getter
    private final ShapeTreeEcosystem ecosystem;

    @Getter
    private final ShapeTreeContext context;

    private boolean skipValidation = false;

    public ShapeTreeClientImpl(ShapeTreeEcosystem ecosystem, ShapeTreeContext context) {
        this.ecosystem = ecosystem;
        this.context = context;
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
    public List<ShapeTreeLocator> discoverShapeTree(URI targetContainer) throws IOException {
        log.info("Discovering Shape Trees present at {}", targetContainer);
        RemoteResource targetContainerResource = new RemoteResource(targetContainer, context.getAuthorizationHeaderValue());
        RemoteResource targetContainerMetadataResource = targetContainerResource.getMetadataResource(context.getAuthorizationHeaderValue());
        return ShapeTreeLocator.getShapeTreeLocatorsFromGraph(targetContainerMetadataResource.getGraph(targetContainerResource.getURI()));
    }

    @Override
    public URI plantShapeTree(URI parentContainer, List<URI> shapeTreeURIs, String focusNode, URI shapeTreeHint, String proposedResourceName, Graph bodyGraph) throws IOException, URISyntaxException {
        String shapeTreeCommaDelimited = "";
        if (shapeTreeURIs != null) {
            for(URI shapeTreeURI : shapeTreeURIs) {
                shapeTreeCommaDelimited += "," + shapeTreeURI;
            }
        }

        log.info("Planting shape tree [Parent container={}], [Shape Trees={}], [FocusNode={}], [ShapeTreeHint={}], [ProposedResourceName={}]", parentContainer, shapeTreeCommaDelimited, focusNode, shapeTreeHint, proposedResourceName);
        String turtleString = GraphHelper.writeGraphToTurtleString(bodyGraph);
        return plantShapeTree(parentContainer, shapeTreeURIs, focusNode, shapeTreeHint, proposedResourceName, turtleString, "text/turtle");
    }

    @Override
    public URI plantShapeTree(URI parentContainer, List<URI> shapeTreeURIs, String focusNode, URI shapeTreeHint, String proposedResourceName, String bodyString, String contentType) throws IOException, URISyntaxException {
        OkHttpClient client = new ShapeTreeValidatingClientBuilder(this.ecosystem, this.skipValidation).get();

        byte[] bytes = new byte[]{};
        if (bodyString != null) {
            bytes = bodyString.getBytes();
        }

        Request.Builder builder = new Request.Builder()
                .url(parentContainer.toString());

        for (URI shapeTreeUri : shapeTreeURIs) {
            builder.addHeader(HttpHeaders.LINK.getValue(), "<" + shapeTreeUri.toString() + ">; rel=\"" + LinkRelations.SHAPETREE.getValue() + "\"");
        }

        applyCommonHeaders(builder, focusNode, shapeTreeHint, true, proposedResourceName, contentType);

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
            if (response.body() != null) {
                responseBodyString = response.body().string();
            }
            throw new IOException(response.code() + " " + responseBodyString);
        }
    }

    @Override
    public Response createDataInstance(URI parentContainer, String focusNode, URI shapeTreeHint, String proposedResourceName, Boolean isContainer, String bodyString, String contentType) throws IOException {
        log.info("Creating data instance {} in {} with hint {}", parentContainer, proposedResourceName, shapeTreeHint);
        OkHttpClient client = new ShapeTreeValidatingClientBuilder(this.ecosystem, this.skipValidation).get();

        byte[] bytes = new byte[]{};
        if (bodyString != null) {
            bytes = bodyString.getBytes();
        }

        String resourceURI = parentContainer.toString();
        if (!resourceURI.endsWith("/")) {
            resourceURI += "/";
        }
        resourceURI += proposedResourceName;
        log.info("Build Resource URI {}", resourceURI);

        Request.Builder putBuilder = new Request.Builder()
                .url(resourceURI)
                .put(RequestBody.create(bytes));

        // proposed resource is name is nulled since a Slug will not be used
        applyCommonHeaders(putBuilder, focusNode, shapeTreeHint, isContainer, null, contentType);

        Response response = client.newCall(putBuilder.build()).execute();
        return response;
    }

    @Override
    public Response updateDataInstance(URI resourceURI, String focusNode, URI shapeTreeHint, String bodyString, String contentType) throws IOException {
        OkHttpClient client = new ShapeTreeValidatingClientBuilder(this.ecosystem, this.skipValidation).get();

        byte[] bytes = new byte[]{};
        if (bodyString != null) {
            bytes = bodyString.getBytes();
        }

        Request.Builder putBuilder = new Request.Builder()
                .url(resourceURI.toString())
                .put(RequestBody.create(bytes));

        applyCommonHeaders(putBuilder, focusNode, shapeTreeHint, null, null, contentType);

        Response response = client.newCall(putBuilder.build()).execute();
        return response;
    }

    @Override
    public Response updateDataInstanceWithPatch(URI resourceURI, String focusNode, URI shapeTreeHint, String bodyString, String contentType) throws IOException, URISyntaxException {
        OkHttpClient client = new ShapeTreeValidatingClientBuilder(this.ecosystem, this.skipValidation).get();

        byte[] sparqlUpdateBytes = bodyString.getBytes();

        Request.Builder patchBuilder = new Request.Builder()
                .url(resourceURI.toString())
                .patch(RequestBody.create(sparqlUpdateBytes));

        applyCommonHeaders(patchBuilder, focusNode, shapeTreeHint, null, null, contentType);

        Response response = client.newCall(patchBuilder.build()).execute();
        return response;
    }

    @Override
    public Response deleteDataInstance(URI resourceURI, URI shapeTreeURI) throws IOException {
        OkHttpClient client = new ShapeTreeValidatingClientBuilder(this.ecosystem, this.skipValidation).get();

        Request.Builder deleteBuilder = new Request.Builder()
                .url(resourceURI.toString())
                .delete();

        applyCommonHeaders(deleteBuilder, null, null, null, null, null);

        Response response = client.newCall(deleteBuilder.build()).execute();
        return response;
    }

    @Override
    public void unplantShapeTree(URI containerURI, URI shapeTreeURI) {

    }

    private void applyCommonHeaders(Request.Builder builder,  String focusNode, URI shapeTreeHint, Boolean isContainer, String proposedResourceName, String contentType) {

        builder.addHeader(HttpHeaders.AUTHORIZATION.getValue(), this.context.getAuthorizationHeaderValue());

        if (isContainer != null) {
            String resourceTypeUri = isContainer ? "http://www.w3.org/ns/ldp#Container" : "http://www.w3.org/ns/ldp#Resource";
            builder.addHeader(HttpHeaders.LINK.getValue(), "<" + resourceTypeUri + ">; rel=\"type\"");
        }

        if (focusNode != null) {
            builder.addHeader(HttpHeaders.LINK.getValue(), "<" + focusNode + ">; rel=\"" + LinkRelations.FOCUS_NODE.getValue() + "\"");
        }

        if (shapeTreeHint != null) {
            builder.addHeader(HttpHeaders.LINK.getValue(), "<" + shapeTreeHint + ">; rel=\"" + LinkRelations.TARGET_SHAPETREE + "\"");
        }

        if (proposedResourceName != null) {
            builder.addHeader(HttpHeaders.SLUG.getValue(), proposedResourceName);
        }

        if (contentType != null) {
            builder.addHeader(HttpHeaders.CONTENT_TYPE.getValue(), contentType);
        }
    }
}
