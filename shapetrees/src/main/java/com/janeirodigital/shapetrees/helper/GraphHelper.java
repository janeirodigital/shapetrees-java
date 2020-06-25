package com.janeirodigital.shapetrees.helper;

import com.janeirodigital.shapetrees.enums.Namespaces;
import org.apache.jena.graph.Graph;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;

import java.io.InputStream;
import java.io.StringReader;

public class GraphHelper {

    public static Lang getLangForContentType(String contentType) {
        switch (contentType) {
            case "application/ld+json":
                return Lang.JSONLD;
            case "text/turtle":
                return Lang.TURTLE;
            default:
                return Lang.TURTLE;
        }
    }

    public static Graph readStringIntoGraph(String rawContent, String contentType) {
        Model model = ModelFactory.createDefaultModel();
        StringReader reader = new StringReader(rawContent);
        RDFDataMgr.read(model.getGraph(), reader, Namespaces.SHAPETREE_NAMESPACE.getValue(), GraphHelper.getLangForContentType(contentType));
        return model.getGraph();
    }

    public static Graph readStreamIntoGraph(InputStream inputStream, String contentType) {
        Model model = ModelFactory.createDefaultModel();
        RDFDataMgr.read(model.getGraph(), inputStream, Namespaces.SHAPETREE_NAMESPACE.getValue(), GraphHelper.getLangForContentType(contentType));
        return model.getGraph();
    }

}
