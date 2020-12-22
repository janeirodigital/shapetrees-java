package com.janeirodigital.shapetrees.core.models;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.net.URI;

@Getter @AllArgsConstructor
public class ReferencedShapeTree {
    URI referencedShapeTree;
    String traverseViaShapePath;
}
