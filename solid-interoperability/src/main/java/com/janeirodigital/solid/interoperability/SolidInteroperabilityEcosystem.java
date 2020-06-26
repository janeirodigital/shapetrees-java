package com.janeirodigital.solid.interoperability;

import com.janeirodigital.shapetrees.ShapeTreeEcosystem;
import com.janeirodigital.shapetrees.model.ShapeTreePlantResult;

import java.net.URI;

public class SolidInteroperabilityEcosystem implements ShapeTreeEcosystem {

    public void initializeEcosystem() {
        // TODO Implement this
    }

    public ShapeTreePlantResult getExistingShapeTreeFromContainer(URI parentContainer, URI shapeTreeURI) {
        // TODO Implement this
        return new ShapeTreePlantResult();
    }

    public void indexShapeTree(URI parentContainer, URI shapeTreeURI, URI plantedShapeTreeURI) {
        // TODO Implement this
    }

    public void unIndexShapeTree(URI parentContainer, URI shapeTreeURI, URI plantedShapeTreeURI) {
        // TODO Implement this
    }
}
