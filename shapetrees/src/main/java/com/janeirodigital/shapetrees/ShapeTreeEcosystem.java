package com.janeirodigital.shapetrees;

import com.janeirodigital.shapetrees.model.ShapeTreeContext;
import com.janeirodigital.shapetrees.model.ShapeTreePlantResult;
import org.apache.jena.graph.Graph;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;

public interface ShapeTreeEcosystem {
    void initializeEcosystem();
    ShapeTreePlantResult getExistingShapeTreeFromContainer(URI parentContainer, URI shapeTreeURI);
    // TODO add a pre-plant method
    // Graph beforePlant(Graph graph);
    void indexShapeTree(ShapeTreeContext context, URI parentContainer, URI shapeTreeURI, URI plantedShapeTreeURI) throws IOException, URISyntaxException;
    void indexShapeTreeDataInstance(URI shapeTreeURI, URI instanceURI);
    void unIndexShapeTree(URI parentContainer, URI shapeTreeURI, URI plantedShapeTreeURI);
    void unIndexShapeTreeDataInstance(URI shapeTreeURI, URI instanceURI);
}
