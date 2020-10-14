package com.janeirodigital.shapetrees.client;

import com.janeirodigital.shapetrees.model.ShapeTreeLocator;
import okhttp3.Response;
import org.apache.jena.graph.Graph;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;

public interface ShapeTreeClient {
    List<ShapeTreeLocator> discoverShapeTree(URI targetContainer) throws IOException;
    URI plantShapeTree(URI parentContainer, List<URI> shapeTreeURIs, String focusNode, URI shapeTreeHint, String proposedResourceName, Graph body) throws IOException, URISyntaxException;
    URI plantShapeTree(URI parentContainer, List<URI> shapeTreeURIs, String focusNode, URI shapeTreeHint, String proposedResourceName, String bodyString, String contentType) throws IOException, URISyntaxException;
    Response createDataInstance(URI parentContainer, String focusNode, URI shapeTreeHint, String proposedResourceName, Boolean isContainer, String bodyString, String contentType) throws IOException, URISyntaxException;
    Response updateDataInstance(URI resourceURI, String focusNode, URI shapeTreeHint, String bodyString, String contentType) throws IOException, URISyntaxException;
    Response updateDataInstanceWithPatch(URI resourceURI, String focusNode, URI shapeTreeHint, String bodyString, String contentType) throws IOException, URISyntaxException;
    Response deleteDataInstance(URI resourceURI, URI shapeTreeURI) throws IOException;
    void unplantShapeTree(URI containerURI, URI shapeTreeURI);
    boolean isSkipValidation();
    void setSkipValidation(boolean skipValidation);
}
