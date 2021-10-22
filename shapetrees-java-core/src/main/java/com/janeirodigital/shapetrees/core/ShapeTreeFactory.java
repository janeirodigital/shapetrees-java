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

import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Slf4j
public class ShapeTreeFactory {
    private ShapeTreeFactory() {
    }

    private static final String RDFS_LABEL = "http://www.w3.org/2000/01/rdf-schema#label";
    private static final Map<URI, ShapeTree> localShapeTreeCache = new HashMap<>();

    public static ShapeTree getShapeTree(URI shapeTreeURI) throws URISyntaxException, ShapeTreeException {

        if (localShapeTreeCache.containsKey(shapeTreeURI)) {
            log.debug("[{}] previously cached -- returning", shapeTreeURI.toString());
            return localShapeTreeCache.get(shapeTreeURI);
        }

        dereferenceAndParseShapeTreeResource(shapeTreeURI);

        return localShapeTreeCache.get(shapeTreeURI);
    }

    private static void dereferenceAndParseShapeTreeResource(URI shapeTreeURI) throws URISyntaxException, ShapeTreeException {
        try {
            DocumentResponse contents = DocumentLoaderManager.getLoader().loadExternalDocument(shapeTreeURI);
            Model model = GraphHelper.readStringIntoModel(shapeTreeURI, contents.getBody(), contents.getContentType().orElse("text/turtle"));
            Resource resource = model.getResource(shapeTreeURI.toString());
            recursivelyParseShapeTree(model, resource);
        } catch (RiotNotFoundException rnfe) {
            log.error("Unable to load graph at URI {}", shapeTreeURI);
        }
    }

    private static void recursivelyParseShapeTree(Model model, Resource resource) throws URISyntaxException, ShapeTreeException {
        // Set the URI as the ID (string representation)
        String shapeTreeURIString = resource.getURI();
        log.debug("Entering recursivelyParseShapeTree for [{}]", shapeTreeURIString);
        URI shapeTreeURI = new URI(shapeTreeURIString);

        if (localShapeTreeCache.containsKey(shapeTreeURI)) {
            log.debug("[{}] previously cached -- returning", shapeTreeURIString);
            return;
        }
        // Set the expected resource type
        String expectsType = getStringValue(model, resource, ShapeTreeVocabulary.EXPECTS_TYPE);
        if (expectsType == null) throw new ShapeTreeException(500, "Shape Tree :expectsType not found");

        // Set Shape URI
        final String shape = getStringValue(model, resource, ShapeTreeVocabulary.SHAPE);
        // Set Label
        final String label = getStringValue(model, resource, RDFS_LABEL);
        // Set Supports
        final String supports = getStringValue(model, resource, ShapeTreeVocabulary.SUPPORTS);
        // Set Reference collection
        final ArrayList<ReferencedShapeTree> references = new ArrayList<>();
        List<URI> contains = getURLListValue(model, resource, ShapeTreeVocabulary.CONTAINS);

        ShapeTree shapeTree = new ShapeTree(
                DocumentLoaderManager.getLoader(),
                shapeTreeURIString,
                expectsType,
                label,
                shape,
                supports,
                references,
                contains);

        // Add the shapeTree to the cache before any of the recursive processing
        localShapeTreeCache.put(shapeTreeURI, shapeTree);

        Property referencesProperty = model.createProperty(ShapeTreeVocabulary.REFERENCES);
        if (resource.hasProperty(referencesProperty)) {
            List<Statement> referenceStatements = resource.listProperties(referencesProperty).toList();
            for (Statement referenceStatement : referenceStatements) {

                Resource referenceResource = referenceStatement.getObject().asResource();
                URI referenceShapeTreeUri = new URI(getStringValue(model, referenceResource, ShapeTreeVocabulary.HAS_SHAPE_TREE));
                String shapePath = getStringValue(model, referenceResource, ShapeTreeVocabulary.VIA_SHAPE_PATH);
                if (!localShapeTreeCache.containsKey(referenceShapeTreeUri)) {
                    // If the model contains the referenced ShapeTree, go ahead and parse and cache it
                    recursivelyParseShapeTree(model, model.getResource(referenceShapeTreeUri.toString()));
                }

                // Create the object that defines there relation between a ShapeTree and its children
                ReferencedShapeTree referencedShapeTree = new ReferencedShapeTree(referenceShapeTreeUri, shapePath);
                references.add(referencedShapeTree);
            }
        }

        // Containers are expected to have contents
        if (resource.hasProperty(model.createProperty(ShapeTreeVocabulary.CONTAINS)) && !expectsType.equals(ShapeTreeVocabulary.CONTAINER)) {
            throw new ShapeTreeException(400, "Contents predicate not expected outside of st:Container Types");
        }
        if (expectsType.equals(ShapeTreeVocabulary.CONTAINER)) {
            for (URI uri : contains) {
                if (!localShapeTreeCache.containsKey(uri)) {
                    recursivelyParseShapeTree(model, model.getResource(uri.toString()));
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

    private static List<URI> getURLListValue(Model model, Resource resource, String predicate) throws URISyntaxException {
        List<URI> uris = new ArrayList<>();
        Property property = model.createProperty(predicate);
        if (resource.hasProperty(property)) {
            List<Statement> propertyStatements = resource.listProperties(property).toList();
            for (Statement propertyStatement : propertyStatements) {
                Node propertyNode = propertyStatement.getObject().asNode();
                if (propertyNode instanceof Node_URI) {
                    URI contentURI = new URI(propertyNode.getURI());
                    uris.add(contentURI);
                }
            }
        }
        return uris;
    }
}
