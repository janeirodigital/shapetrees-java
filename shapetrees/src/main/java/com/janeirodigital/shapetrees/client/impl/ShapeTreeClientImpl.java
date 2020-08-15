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
import okhttp3.*;
import org.apache.jena.graph.Graph;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;

public class ShapeTreeClientImpl implements ShapeTreeClient {

    @Getter
    private final ShapeTreeEcosystem ecosystem;

    @Getter
    private final ShapeTreeContext context;

    public ShapeTreeClientImpl(ShapeTreeEcosystem ecosystem, ShapeTreeContext context) {
        this.ecosystem = ecosystem;
        this.context = context;
    }

    @Override
    public List<ShapeTreeLocator> discoverShapeTree(URI targetContainer) throws IOException {
        RemoteResource targetContainerResource = new RemoteResource(targetContainer, context.getAuthorizationHeaderValue());
        RemoteResource targetContainerMetadataResource = targetContainerResource.getMetadataResource(context.getAuthorizationHeaderValue());
        return ShapeTreeLocator.getShapeTreeLocatorsFromGraph(targetContainerMetadataResource.getGraph(targetContainerResource.getURI()));
    }

    @Override
    public URI plantShapeTree(URI parentContainer, List<URI> shapeTreeURIs, String focusNode, URI shapeTreeHint, String proposedResourceName, Graph bodyGraph) throws IOException, URISyntaxException {
        String turtleString = GraphHelper.writeGraphToTurtleString(bodyGraph);
        return plantShapeTree(parentContainer, shapeTreeURIs, focusNode, shapeTreeHint, proposedResourceName, turtleString, "text/turtle");
    }

    @Override
    public URI plantShapeTree(URI parentContainer, List<URI> shapeTreeURIs, String focusNode, URI shapeTreeHint, String proposedResourceName, String bodyString, String contentType) throws IOException, URISyntaxException {
        OkHttpClient client = new ShapeTreeValidatingClientBuilder(this.ecosystem).get();

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
    public void createDataInstance(URI parentContainer, String focusNode, URI shapeTreeHint, String proposedResourceName, Boolean isContainer, String bodyString, String contentType) throws IOException {
        OkHttpClient client = new ShapeTreeValidatingClientBuilder(this.ecosystem).get();

        byte[] bytes = new byte[]{};
        if (bodyString != null) {
            bytes = bodyString.getBytes();
        }

        Request.Builder postBuilder = new Request.Builder()
                .url(parentContainer.toString())
                .post(RequestBody.create(bytes));

        applyCommonHeaders(postBuilder, focusNode, shapeTreeHint, isContainer, proposedResourceName, contentType);

        client.newCall(postBuilder.build()).execute();
    }

    @Override
    public void updateDataInstance(URI resourceURI, String focusNode, URI shapeTreeHint, String bodyString, String contentType) throws IOException {
        OkHttpClient client = new ShapeTreeValidatingClientBuilder(this.ecosystem).get();

        byte[] bytes = new byte[]{};
        if (bodyString != null) {
            bytes = bodyString.getBytes();
        }

        Request.Builder putBuilder = new Request.Builder()
                .url(resourceURI.toString())
                .put(RequestBody.create(bytes));

        applyCommonHeaders(putBuilder, focusNode, shapeTreeHint, null, null, contentType);

        client.newCall(putBuilder.build()).execute();
    }

    @Override
    public void updateDataInstanceWithPatch(URI resourceURI, String focusNode, URI shapeTreeHint, String bodyString, String contentType) throws IOException, URISyntaxException {
        OkHttpClient client = new ShapeTreeValidatingClientBuilder((this.ecosystem)).get();

        byte[] sparqlUpdateBytes = bodyString.getBytes();

        Request.Builder patchBuilder = new Request.Builder()
                .url(resourceURI.toString())
                .patch(RequestBody.create(sparqlUpdateBytes));

        applyCommonHeaders(patchBuilder, focusNode, shapeTreeHint, null, null, contentType);

        client.newCall(patchBuilder.build()).execute();
    }

    @Override
    public void deleteDataInstance(URI resourceURI, URI shapeTreeURI) {

    }

    @Override
    public void unplantShapeTree(URI containerURI, URI shapeTreeURI) {

    }

    private void applyCommonHeaders(Request.Builder builder,  String focusNode, URI shapeTreeHint, Boolean isContainer, String proposedResourceName, String contentType) {

        builder.addHeader(HttpHeaders.AUTHORIZATION.getValue(), this.context.getAuthorizationHeaderValue());

        if (focusNode != null) {
            builder.addHeader(HttpHeaders.LINK.getValue(), "<" + focusNode + ">; rel=\"" + LinkRelations.FOCUS_NODE.getValue() + "\"");
        }

        if (shapeTreeHint != null) {
            builder.addHeader(HttpHeaders.LINK.getValue(), "<" + shapeTreeHint + ">; rel=\"" + LinkRelations.TARGET_SHAPETREE + "\"");
        }

        if (isContainer != null) {
            String resourceTypeUri = isContainer ? "http://www.w3.org/ns/ldp#Container" : "http://www.w3.org/ns/ldp#Resource";
            builder.addHeader(HttpHeaders.LINK.getValue(), "<" + resourceTypeUri + ">; rel=\"type\"");
        }

        if (proposedResourceName != null) {
            builder.addHeader(HttpHeaders.SLUG.getValue(), proposedResourceName);
        }

        if (contentType != null) {
            builder.addHeader(HttpHeaders.CONTENT_TYPE.getValue(), contentType);
        }
    }
}
