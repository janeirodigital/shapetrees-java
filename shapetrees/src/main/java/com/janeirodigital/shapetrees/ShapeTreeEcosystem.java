package com.janeirodigital.shapetrees;

import com.janeirodigital.shapetrees.model.ShapeTreeContext;
import com.janeirodigital.shapetrees.model.ShapeTreePlantResult;
import com.janeirodigital.shapetrees.model.ShapeTreeStep;
import org.apache.jena.graph.Graph;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;
import java.util.Map;

public interface ShapeTreeEcosystem {
    void initializeEcosystem();
    ShapeTreePlantResult getExistingShapeTreeFromContainer(URI parentContainer, URI shapeTreeURI);
    // TODO add a pre-plant method
    Graph beforePlantShapeTree(ShapeTreeContext context, URI expectedURI, Graph graph, List<ShapeTreeStep> shapeTreesToPlant, Map<String, List<String>> linkHeaders) throws URISyntaxException;
    void indexShapeTree(ShapeTreeContext context, URI parentContainer, URI shapeTreeURI, URI plantedShapeTreeURI, Map<String, List<String>> linkHeaders) throws IOException, URISyntaxException;
    void indexShapeTreeDataInstance(URI shapeTreeURI, URI instanceURI);
    void unIndexShapeTree(URI parentContainer, URI shapeTreeURI, URI plantedShapeTreeURI);
    void unIndexShapeTreeDataInstance(URI shapeTreeURI, URI instanceURI);
}
