package com.janeirodigital.shapetrees.helper;

import com.janeirodigital.shapetrees.ShapeTreeException;
import com.janeirodigital.shapetrees.enums.Namespaces;
import org.apache.jena.graph.Graph;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.riot.RiotException;

import java.io.StringReader;
import java.net.URI;
import java.net.URISyntaxException;

public class GraphHelper {

    public static Lang getLangForContentType(String contentType) {
        if (contentType == null) {
            return Lang.TURTLE;
        }
        switch (contentType) {
            case "application/ld+json":
                return Lang.JSONLD;
            default:
                return Lang.TURTLE;
        }
    }

    public static Graph readStringIntoGraph(String rawContent, String contentType) throws URISyntaxException, ShapeTreeException {
        return readStringIntoGraph(rawContent, new URI(Namespaces.SHAPETREE_NAMESPACE.getValue()), contentType);
    }

    public static Graph readStringIntoGraph(String rawContent, URI baseURI, String contentType) throws ShapeTreeException {
        try {
            Model model = ModelFactory.createDefaultModel();
            StringReader reader = new StringReader(rawContent);
            RDFDataMgr.read(model.getGraph(), reader, baseURI.toString(), GraphHelper.getLangForContentType(contentType));
            return model.getGraph();
        } catch (RiotException rex) {
            throw new ShapeTreeException(422, "Error processing input - " + rex.getMessage());
        }
    }

}
