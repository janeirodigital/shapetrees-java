package com.janeirodigital.shapetrees.client;

import com.janeirodigital.shapetrees.model.ShapeTreeContext;
import com.janeirodigital.shapetrees.model.ShapeTreeLocator;
import okhttp3.Response;
import org.apache.jena.graph.Graph;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;

public interface ShapeTreeClient {
    List<ShapeTreeLocator> discoverShapeTree(ShapeTreeContext context, URI targetContainer) throws IOException;
    URI plantShapeTree(ShapeTreeContext context, URI parentContainer, List<URI> shapeTreeURIs, String focusNode, URI shapeTreeHint, String proposedResourceName, Graph body) throws IOException, URISyntaxException;
    URI plantShapeTree(ShapeTreeContext context, URI parentContainer, List<URI> shapeTreeURIs, String focusNode, URI shapeTreeHint, String proposedResourceName, String bodyString, String contentType) throws IOException, URISyntaxException;
    Response createDataInstance(ShapeTreeContext context, URI parentContainer, String focusNode, URI shapeTreeHint, String proposedResourceName, Boolean isContainer, String bodyString, String contentType) throws IOException, URISyntaxException;
    Response updateDataInstance(ShapeTreeContext context, URI resourceURI, String focusNode, URI shapeTreeHint, String bodyString, String contentType) throws IOException, URISyntaxException;
    Response updateDataInstanceWithPatch(ShapeTreeContext context, URI resourceURI, String focusNode, URI shapeTreeHint, String bodyString, String contentType) throws IOException, URISyntaxException;
    Response updateDataInstanceWithLocalPatch(ShapeTreeContext context, URI resourceURI, String focusNode, URI shapeTreeHint, String bodyString, String queryString, String contentType) throws IOException, URISyntaxException;
    Response deleteDataInstance(ShapeTreeContext context, URI resourceURI, URI shapeTreeURI) throws IOException;
    void unplantShapeTree(ShapeTreeContext context, URI containerURI, URI shapeTreeURI);
    boolean isSkipValidation();
    void setSkipValidation(boolean skipValidation);
}
