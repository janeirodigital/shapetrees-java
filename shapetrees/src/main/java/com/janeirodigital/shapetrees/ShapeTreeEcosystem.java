package com.janeirodigital.shapetrees;

import java.net.URI;

public interface ShapeTreeEcosystem {
    void initializeEcosystem();
    Boolean containerIsShapeTree(URI parentContainer);
    Boolean containerIsShapeTree(URI parentContainer, URI shapeTreeURI);
    void indexShapeTree(URI parentContainer, URI shapeTreeURI, URI plantedShapeTreeURI);
    void unIndexShapeTree(URI parentContainer, URI shapeTreeURI, URI plantedShapeTreeURI);
}
