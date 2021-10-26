package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.contentloaders.DocumentLoaderManager;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.helpers.GraphHelper;
import com.janeirodigital.shapetrees.core.models.ReferencedShapeTree;
import com.janeirodigital.shapetrees.core.models.ShapeTree;
import com.janeirodigital.shapetrees.core.vocabularies.ShapeTreeVocabulary;
import lombok.extern.slf4j.Slf4j;
import org.apache.jena.graph.Node;
import org.apache.jena.graph.Node_URI;
import org.apache.jena.rdf.model.*;
import org.apache.jena.riot.RiotNotFoundException;

import java.net.URL;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static com.janeirodigital.shapetrees.core.helpers.GraphHelper.urlToUri;

@Slf4j
public class ShapeTreeFactory {
    private ShapeTreeFactory() {
    }

    private static final String RDFS_LABEL = "http://www.w3.org/2000/01/rdf-schema#label";
    private static final Map<URL, ShapeTree> localShapeTreeCache = new HashMap<>();

    public static ShapeTree getShapeTree(URL shapeTreeUrl) throws ShapeTreeException {

        if (localShapeTreeCache.containsKey(shapeTreeUrl)) {
            log.debug("[{}] previously cached -- returning", shapeTreeUrl.toString());
            return localShapeTreeCache.get(shapeTreeUrl);
        }

        dereferenceAndParseShapeTreeResource(shapeTreeUrl);

        return localShapeTreeCache.get(shapeTreeUrl);
    }

    private static void dereferenceAndParseShapeTreeResource(URL shapeTreeUrl) throws ShapeTreeException {
        try {
            DocumentResponse contents = DocumentLoaderManager.getLoader().loadExternalDocument(shapeTreeUrl);
            Model model = GraphHelper.readStringIntoModel(urlToUri(shapeTreeUrl), contents.getBody(), contents.getContentType().orElse("text/turtle"));
            Resource resource = model.getResource(shapeTreeUrl.toString());
            recursivelyParseShapeTree(model, resource);
        } catch (RiotNotFoundException rnfe) {
            log.error("Unable to load graph at URL {}", shapeTreeUrl);
        }
    }

    private static void recursivelyParseShapeTree(Model model, Resource resource) throws ShapeTreeException {
        // Set the URL as the ID (string representation)
        final URL shapeTreeUrl;
        try {
            shapeTreeUrl = new URL(resource.getURI());
        } catch (MalformedURLException ex) {
            throw new ShapeTreeException(500, "Error reporting validation success on malformed URL <" + resource.getURI() + ">: " + ex.getMessage());
        }
        log.debug("Entering recursivelyParseShapeTree for [{}]", shapeTreeUrl);

        if (localShapeTreeCache.containsKey(shapeTreeUrl)) {
            log.debug("[{}] previously cached -- returning", shapeTreeUrl);
            return;
        }
        // Set the expected resource type
        String expectsType = getStringValue(model, resource, ShapeTreeVocabulary.EXPECTS_TYPE);
        if (expectsType == null) throw new ShapeTreeException(500, "Shape Tree :expectsType not found");

        // Set Shape URL
        final String shape = getStringValue(model, resource, ShapeTreeVocabulary.SHAPE);
        // Set Label
        final String label = getStringValue(model, resource, RDFS_LABEL);
        // Set Supports
        final String supports = getStringValue(model, resource, ShapeTreeVocabulary.SUPPORTS);
        // Set Reference collection
        final ArrayList<ReferencedShapeTree> references = new ArrayList<>();
        final List<URL> contains;
        try {
            contains = getURLListValue(model, resource, ShapeTreeVocabulary.CONTAINS);
        } catch (MalformedURLException ex) {
            throw new ShapeTreeException(500, "List <"+ shapeTreeUrl +"> containes malformed URL: " + ex.getMessage());
        }

        ShapeTree shapeTree = new ShapeTree(
                DocumentLoaderManager.getLoader(),
                shapeTreeUrl,
                expectsType,
                label,
                shape,
                supports,
                references,
                contains);

        // Add the shapeTree to the cache before any of the recursive processing
        localShapeTreeCache.put(shapeTreeUrl, shapeTree);

        Property referencesProperty = model.createProperty(ShapeTreeVocabulary.REFERENCES);
        if (resource.hasProperty(referencesProperty)) {
            List<Statement> referenceStatements = resource.listProperties(referencesProperty).toList();
            for (Statement referenceStatement : referenceStatements) {

                Resource referenceResource = referenceStatement.getObject().asResource();
                final String referencedShapeTreeUrlString = getStringValue(model, referenceResource, ShapeTreeVocabulary.HAS_SHAPE_TREE);
                final URL referencedShapeTreeUrl;
                try {
                    referencedShapeTreeUrl = new URL(referencedShapeTreeUrlString);
                } catch (MalformedURLException ex) {
                    throw new ShapeTreeException(500, "ShapeTree <" + shapeTreeUrl + "> references malformed URL <" + referencedShapeTreeUrlString + ">: " + ex.getMessage());
                }
                String shapePath = getStringValue(model, referenceResource, ShapeTreeVocabulary.VIA_SHAPE_PATH);
                if (!localShapeTreeCache.containsKey(referencedShapeTreeUrl)) {
                    // If the model contains the referenced ShapeTree, go ahead and parse and cache it
                    recursivelyParseShapeTree(model, model.getResource(referencedShapeTreeUrl.toString()));
                }

                // Create the object that defines there relation between a ShapeTree and its children
                ReferencedShapeTree referencedShapeTree = new ReferencedShapeTree(referencedShapeTreeUrl, shapePath);
                references.add(referencedShapeTree);
            }
        }

        // Containers are expected to have contents
        if (resource.hasProperty(model.createProperty(ShapeTreeVocabulary.CONTAINS)) && !expectsType.equals(ShapeTreeVocabulary.CONTAINER)) {
            throw new ShapeTreeException(400, "Contents predicate not expected outside of st:Container Types");
        }
        if (expectsType.equals(ShapeTreeVocabulary.CONTAINER)) {
            for (URL url : contains) {
                if (!localShapeTreeCache.containsKey(url)) {
                    recursivelyParseShapeTree(model, model.getResource(url.toString()));
                }
            }
        }
    }

    private static String getStringValue(Model model, Resource resource, String predicate) {
        Property property = model.createProperty(predicate);
        if (resource.hasProperty(property)) {
            Statement statement = resource.getProperty(property);
            if (statement.getObject().isLiteral()) {
                return statement.getObject().asLiteral().getString();
            } else if (statement.getObject().isURIResource()) {
                return statement.getObject().asResource().getURI();
            } else {
                log.error("In getStringValue for predicate [{}] unable to value of Node", predicate);
            }

        }
        return null;
    }

    private static List<URL> getURLListValue(Model model, Resource resource, String predicate) throws MalformedURLException {
        List<URL> urls = new ArrayList<>();
        Property property = model.createProperty(predicate);
        if (resource.hasProperty(property)) {
            List<Statement> propertyStatements = resource.listProperties(property).toList();
            for (Statement propertyStatement : propertyStatements) {
                Node propertyNode = propertyStatement.getObject().asNode();
                if (propertyNode instanceof Node_URI) {
                    URL contentUrl = new URL(propertyNode.getURI());
                    urls.add(contentUrl);
                }
            }
        }
        return urls;
    }
}
