package com.janeirodigital.shapetrees.client.impl;

import com.janeirodigital.shapetrees.RemoteResource;
import com.janeirodigital.shapetrees.ShapeTreeEcosystem;
import com.janeirodigital.shapetrees.client.ShapeTreeClient;
import com.janeirodigital.shapetrees.enums.HttpHeaders;
import com.janeirodigital.shapetrees.enums.LinkRelations;
import com.janeirodigital.shapetrees.helper.GraphHelper;
import com.janeirodigital.shapetrees.model.ShapeTreeContext;
import com.janeirodigital.shapetrees.model.ShapeTreeLocator;
import lombok.extern.slf4j.Slf4j;
import okhttp3.*;
import org.apache.jena.graph.Graph;
import org.apache.jena.query.Dataset;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.sparql.core.DatasetOne;
import org.apache.jena.update.UpdateAction;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.util.List;

@Slf4j
public class ShapeTreeClientImpl implements ShapeTreeClient {

    private boolean skipValidation = false;
    private final ShapeTreeClientConfiguration validatingClientConfig;
    private final ShapeTreeClientConfiguration nonValidatingClientConfig;

    public ShapeTreeClientImpl(ShapeTreeEcosystem ecosystem) {
        this.validatingClientConfig = new ShapeTreeClientConfiguration(ecosystem, true, false);
        this.nonValidatingClientConfig = new ShapeTreeClientConfiguration(ecosystem, false, false);
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
    public Response  createDataInstance(ShapeTreeContext context, URI parentContainer, String focusNode, URI shapeTreeHint, String proposedResourceName, Boolean isContainer, String bodyString, String contentType) throws IOException {
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

        return client.newCall(putBuilder.build()).execute();
    }

    @Override
    public Response updateDataInstance(ShapeTreeContext context, URI resourceURI, String focusNode, URI shapeTreeHint, String bodyString, String contentType) throws IOException {
        OkHttpClient client = ShapeTreeHttpClientHolder.getForConfig(getConfiguration(this.skipValidation));

        byte[] bytes = new byte[]{};
        if (bodyString != null) {
            bytes = bodyString.getBytes();
        }

        Request.Builder putBuilder = new Request.Builder()
                .url(resourceURI.toString())
                .put(RequestBody.create(bytes));

        applyCommonHeaders(context, putBuilder, focusNode, shapeTreeHint, null, null, contentType);

        return client.newCall(putBuilder.build()).execute();
    }

    @Override
    public Response updateDataInstanceWithPatch(ShapeTreeContext context, URI resourceURI, String focusNode, URI shapeTreeHint, String bodyString, String contentType) throws IOException {
        OkHttpClient client = ShapeTreeHttpClientHolder.getForConfig(getConfiguration(this.skipValidation));

        byte[] sparqlUpdateBytes = bodyString.getBytes();

        Request.Builder patchBuilder = new Request.Builder()
                .url(resourceURI.toString())
                .patch(RequestBody.create(sparqlUpdateBytes));

        applyCommonHeaders(context, patchBuilder, focusNode, shapeTreeHint, null, null, contentType);

        return client.newCall(patchBuilder.build()).execute();
    }

    /**
     * To pass the limitation of CSS around SPARQL request with WHERE statements. This method can be used to execute a sparql query locally
     * which then will pass the result to the `updateDataInstance` handler to make the request as a PUT to the server.
     * @param context
     * @param resourceURI
     * @param focusNode
     * @param shapeTreeHint
     * @param bodyString The serialized version of the resource to update.
     *                   TODO: Perhaps it's better to fetch the resource with the resourceURI parameter here instead of
     *                         expecting the caller to have fetched the resource already.
     * @param queryString The query to execute over the dataset. Should be equivalent to the query that was expected to
     *                    be processed by the server (ESS/CSS)
     * @param contentType
     * @return
     * @throws IOException
     * @throws URISyntaxException
     */
    @Override
    public Response updateDataInstanceWithLocalPatch(ShapeTreeContext context, URI resourceURI, String focusNode, URI shapeTreeHint, String bodyString, String queryString, String contentType) throws IOException, URISyntaxException {
        Model model = ModelFactory.createDefaultModel();
        model.read(new ByteArrayInputStream(bodyString.getBytes(StandardCharsets.UTF_8)), null, "TURTLE"); // assuming the input is TTL
        Dataset dataset = new DatasetOne(model);
        UpdateAction.parseExecute(queryString, dataset);

        OutputStream os = new ByteArrayOutputStream();
        model.write(os, "TURTLE");

        return this.updateDataInstance(context, resourceURI, focusNode, shapeTreeHint, os.toString(), contentType);
    }

    @Override
    public Response deleteDataInstance(ShapeTreeContext context, URI resourceURI, URI shapeTreeURI) throws IOException {
        OkHttpClient client = ShapeTreeHttpClientHolder.getForConfig(getConfiguration(this.skipValidation));

        Request.Builder deleteBuilder = new Request.Builder()
                .url(resourceURI.toString())
                .delete();

        applyCommonHeaders(context, deleteBuilder, null, null, null, null, null);

        return client.newCall(deleteBuilder.build()).execute();
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
            builder.addHeader(HttpHeaders.LINK.getValue(), "<" + shapeTreeHint + ">; rel=\"" + LinkRelations.TARGET_SHAPETREE + "\"");
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
