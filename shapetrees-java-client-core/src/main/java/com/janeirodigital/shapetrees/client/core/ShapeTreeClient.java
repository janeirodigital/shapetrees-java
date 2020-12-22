package com.janeirodigital.shapetrees.client.core;

import com.janeirodigital.shapetrees.core.ShapeTreeResponse;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;
import com.janeirodigital.shapetrees.core.models.ShapeTreeLocator;
import org.apache.jena.graph.Graph;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;

public interface ShapeTreeClient {
    List<ShapeTreeLocator> discoverShapeTree(ShapeTreeContext context, URI targetContainer) throws IOException;
    URI plantShapeTree(ShapeTreeContext context, URI parentContainer, List<URI> shapeTreeURIs, String focusNode, URI shapeTreeHint, String proposedResourceName, Graph body) throws IOException, URISyntaxException;
    URI plantShapeTree(ShapeTreeContext context, URI parentContainer, List<URI> shapeTreeURIs, String focusNode, URI shapeTreeHint, String proposedResourceName, String bodyString, String contentType) throws IOException, URISyntaxException;
    ShapeTreeResponse createDataInstance(ShapeTreeContext context, URI parentContainer, String focusNode, URI shapeTreeHint, String proposedResourceName, Boolean isContainer, String bodyString, String contentType) throws IOException, URISyntaxException;
    ShapeTreeResponse updateDataInstance(ShapeTreeContext context, URI resourceURI, String focusNode, URI shapeTreeHint, String bodyString, String contentType) throws IOException, URISyntaxException;
    ShapeTreeResponse updateDataInstanceWithPatch(ShapeTreeContext context, URI resourceURI, String focusNode, URI shapeTreeHint, String bodyString, String contentType) throws IOException, URISyntaxException;
    ShapeTreeResponse deleteDataInstance(ShapeTreeContext context, URI resourceURI, URI shapeTreeURI) throws IOException;
    void unplantShapeTree(ShapeTreeContext context, URI containerURI, URI shapeTreeURI);
    boolean isSkipValidation();
    void setSkipValidation(boolean skipValidation);
}
