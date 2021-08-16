package com.janeirodigital.shapetrees.test;

import com.janeirodigital.shapetrees.ShapeTreeEcosystem;
import com.janeirodigital.shapetrees.model.ShapeTree;
import com.janeirodigital.shapetrees.model.ShapeTreeContext;
import com.janeirodigital.shapetrees.model.ShapeTreePlantResult;
import lombok.EqualsAndHashCode;
import lombok.extern.slf4j.Slf4j;
import org.apache.jena.graph.Graph;


import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;
import java.util.Map;

@Slf4j @EqualsAndHashCode
public class MockEcosystem implements ShapeTreeEcosystem {
    @Override
    public void initializeEcosystem() {

    }

    @Override
    public ShapeTreePlantResult getExistingShapeTreeFromContainer(ShapeTreeContext context, URI parentContainer, List<ShapeTree> shapeTreesToPlant, String requestedName) {
        return new ShapeTreePlantResult();
    }

    @Override
    public Graph beforePlantShapeTree(ShapeTreeContext context, URI expectedURI, Graph graph, List<ShapeTree> shapeTreesToPlant, Map<String, List<String>> linkHeaders) throws URISyntaxException {
        return graph;
    }

    @Override
    public void indexShapeTree(ShapeTreeContext context, URI parentContainer, URI shapeTreeURI, URI plantedShapeTreeURI, Map<String, List<String>> linkHeaders) throws IOException, URISyntaxException {

    }

    @Override
    public void indexShapeTreeDataInstance(ShapeTreeContext context, URI parentContainer, URI shapeTreeURI, URI instanceURI) {

    }

    @Override
    public void unIndexShapeTree(ShapeTreeContext context, URI parentContainer, URI shapeTreeURI, URI plantedShapeTreeURI) {

    }

    @Override
    public void unIndexShapeTreeDataInstance(ShapeTreeContext context, URI shapeTreeURI, URI instanceURI) {

    }
}