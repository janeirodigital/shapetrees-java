package com.janeirodigital.shapetrees.model;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.net.URI;
import java.util.List;

@Getter @AllArgsConstructor @NoArgsConstructor
public class ShapeTreePlantResult {
    private URI shapeTreeURI;
    private URI rootContainer;
    private URI rootContainerMetadata;
    private List<URI> createdChildren;
}
