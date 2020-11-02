package com.janeirodigital.shapetrees;

import com.janeirodigital.shapetrees.model.ShapeTree;
import com.janeirodigital.shapetrees.model.ShapeTreeContext;
import com.janeirodigital.shapetrees.model.ShapeTreePlantResult;
import org.apache.jena.graph.Graph;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;
import java.util.Map;

public interface ShapeTreeEcosystem {
    void initializeEcosystem();
    ShapeTreePlantResult getExistingShapeTreeFromContainer(ShapeTreeContext context, URI parentContainer, List<ShapeTree> shapeTreesToPlant, String requestedName);
    Graph beforePlantShapeTree(ShapeTreeContext context, URI expectedURI, Graph graph, List<ShapeTree> shapeTreesToPlant, Map<String, List<String>> linkHeaders) throws URISyntaxException;
    void indexShapeTree(ShapeTreeContext context, URI parentContainer, URI shapeTreeURI, URI plantedShapeTreeURI, Map<String, List<String>> linkHeaders) throws IOException, URISyntaxException;
    void indexShapeTreeDataInstance(ShapeTreeContext context, URI parentContainer, URI shapeTreeURI, URI instanceURI) throws IOException;
    void unIndexShapeTree(ShapeTreeContext context, URI parentContainer, URI shapeTreeURI, URI plantedShapeTreeURI);
    void unIndexShapeTreeDataInstance(ShapeTreeContext context, URI shapeTreeURI, URI instanceURI) throws IOException;
}
