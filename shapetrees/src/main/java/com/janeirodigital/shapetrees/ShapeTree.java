package com.janeirodigital.shapetrees;

import lombok.Getter;
import org.apache.jena.graph.Graph;

import java.net.URI;
import java.net.URL;
import java.util.Iterator;

@Getter
public class ShapeTree {
    private final Graph shapeTreeGraph;
    private final URI definitionURI;

    public ShapeTree(URI definitionURI, Graph shapeTreeGraph) {
        this.shapeTreeGraph = shapeTreeGraph;
        this.definitionURI = definitionURI;
    }



    public Iterator<ShapeTree> getReferencedShapeTrees() {
        // TODO
        return null;
    }

    public Iterator<URL> getReferencedResources(URI dataInstanceURI) {
        // TODO
        return null;
    }
}
