package com.janeirodigital.shapetrees.test;

import com.janeirodigital.shapetrees.ShapeTreeEcosystem;
import com.janeirodigital.shapetrees.model.ShapeTreePlantResult;

import java.net.URI;

public class MockEcosystem implements ShapeTreeEcosystem {
    @Override
    public void initializeEcosystem() {

    }

    @Override
    public ShapeTreePlantResult getExistingShapeTreeFromContainer(URI parentContainer, URI shapeTreeURI) {
        return new ShapeTreePlantResult();
    }

    @Override
    public void indexShapeTree(URI parentContainer, URI shapeTreeURI, URI plantedShapeTreeURI) {

    }

    @Override
    public void unIndexShapeTree(URI parentContainer, URI shapeTreeURI, URI plantedShapeTreeURI) {

    }

    @Override
    public void indexShapeTreeDataInstance(URI shapeTreeURI, URI instanceURI) {

    }

    @Override
    public void unIndexShapeTreeDataInstance(URI shapeTreeURI, URI instanceURI) {

    }
}
