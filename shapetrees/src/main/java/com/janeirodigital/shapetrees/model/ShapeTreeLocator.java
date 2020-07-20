package com.janeirodigital.shapetrees.model;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter @AllArgsConstructor
public class ShapeTreeLocator {
    private final String rootShapeTree;
    private final String shapeTree;
    private final String shapeTreeInstancePath;
    private final String shapeTreeRoot;
}
