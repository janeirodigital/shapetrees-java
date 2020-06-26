package com.janeirodigital.shapetrees;

import com.janeirodigital.shapetrees.model.ShapeTreePlantResult;

import java.net.URI;

public interface ShapeTreeEcosystem {
    void initializeEcosystem();
    ShapeTreePlantResult getExistingShapeTreeFromContainer(URI parentContainer, URI shapeTreeURI);
    void indexShapeTree(URI parentContainer, URI shapeTreeURI, URI plantedShapeTreeURI);
    void unIndexShapeTree(URI parentContainer, URI shapeTreeURI, URI plantedShapeTreeURI);
}
