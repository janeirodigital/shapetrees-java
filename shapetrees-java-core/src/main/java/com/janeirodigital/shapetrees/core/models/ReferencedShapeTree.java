package com.janeirodigital.shapetrees.core.models;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.net.URL;

@Getter @AllArgsConstructor
public class ReferencedShapeTree {
    URL referencedShapeTreeURI;
    String traverseViaShapePath;
}
