package com.janeirodigital.shapetrees.test;

import com.janeirodigital.shapetrees.ShapeTreeEcosystem;

import java.net.URI;

public class MockEcosystem implements ShapeTreeEcosystem {
    @Override
    public void initializeEcosystem() {

    }

    @Override
    public Boolean containerIsShapeTree(URI parentContainer) {
        return false;
    }

    @Override
    public Boolean containerIsShapeTree(URI parentContainer, URI shapeTreeURI) {
        return false;
    }

    @Override
    public void indexShapeTree(URI parentContainer, URI shapeTreeURI, URI plantedShapeTreeURI) {

    }

    @Override
    public void unIndexShapeTree(URI parentContainer, URI shapeTreeURI, URI plantedShapeTreeURI) {

    }
}
