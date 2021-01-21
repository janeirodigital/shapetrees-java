package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.contentloaders.DocumentContentsLoader;
import com.janeirodigital.shapetrees.core.contentloaders.HttpDocumentContentsLoader;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.helpers.GraphHelper;
import com.janeirodigital.shapetrees.core.models.DocumentContents;
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
    private static DocumentContentsLoader contentsLoader = new HttpDocumentContentsLoader(null, null);

    private static final Map<URI, ShapeTree> localShapeTreeCache = new HashMap<>();

    public static void setContentsLoader(DocumentContentsLoader contentsLoader) {
        ShapeTreeFactory.contentsLoader = contentsLoader;
    }

    public static ShapeTree getShapeTree(URI shapeTreeURI) throws URISyntaxException, ShapeTreeException {
        if (isShapeTreeAllowIRI(shapeTreeURI)) {
            return null;
        }

        if (localShapeTreeCache.containsKey(shapeTreeURI)) {
            log.debug("[{}] previously cached -- returning", shapeTreeURI.toString());
            return localShapeTreeCache.get(shapeTreeURI);
        }

        dereferenceAndParseShapeTreeResource(shapeTreeURI);

        return localShapeTreeCache.get(shapeTreeURI);
    }

    private static void dereferenceAndParseShapeTreeResource(URI shapeTreeURI) throws URISyntaxException, ShapeTreeException {
        try {
            DocumentContents contents = contentsLoader.loadDocumentContents(shapeTreeURI);
            // @@ Plan to migrate to Graph interface and remove
            Model model = GraphHelper.readStringIntoModel(shapeTreeURI, contents.getBody(), contents.getContentType());
            Resource resource = model.getResource(shapeTreeURI.toString());
            recursivelyParseShapeTree(model, resource);
        } catch (RiotNotFoundException rnfe) {
            log.error("Unable to load graph at URI {}", shapeTreeURI);
        }
    }

    private static void recursivelyParseShapeTree(Model model, Resource resource) throws URISyntaxException, ShapeTreeException {
        String shapeTreeURIString = resource.getURI();
        log.debug("Entering recursivelyParseShapeTree for [{}]", shapeTreeURIString);
        URI shapeTreeURI = new URI(shapeTreeURIString);

        if (localShapeTreeCache.containsKey(shapeTreeURI)) {
            log.debug("[{}] previously cached -- returning", shapeTreeURIString);
            return;
        }

        ShapeTree shapeTree = new ShapeTree(contentsLoader);
        // Set the URI as the ID (string representation)
        shapeTree.setId(shapeTreeURIString);
        // Set the expected resource type
        String expectsType = getStringValue(model, resource, ShapeTreeVocabulary.EXPECTS_TYPE);
        if (expectsType == null) throw new ShapeTreeException(500, "Shape Tree :expectsType not found");
        shapeTree.setExpectedResourceType(expectsType);
        // Set Shape URI
        shapeTree.setValidatedByShapeUri(getStringValue(model, resource, ShapeTreeVocabulary.VALIDATED_BY));
        // Set Label
        shapeTree.setLabel(getStringValue(model, resource, RDFS_LABEL));
        // Set Supports
        shapeTree.setSupports(getStringValue(model, resource, ShapeTreeVocabulary.SUPPORTS));
        // Set Reference collection
        shapeTree.setReferences(new ArrayList<>());

        // Add the shapeTree to the cache before any of the recursive processing
        localShapeTreeCache.put(shapeTreeURI, shapeTree);

        Property referencesProperty = model.createProperty(ShapeTreeVocabulary.REFERENCES);
        if (resource.hasProperty(referencesProperty)) {
            List<Statement> referenceStatements = resource.listProperties(referencesProperty).toList();
            for (Statement referenceStatement : referenceStatements) {

                Resource referenceResource = referenceStatement.getObject().asResource();
                URI referenceShapeTreeUri = new URI(getStringValue(model, referenceResource, ShapeTreeVocabulary.HAS_SHAPE_TREE));
                String shapePath = getStringValue(model, referenceResource, ShapeTreeVocabulary.TRAVERSE_VIA_SHAPE_PATH);
                if (!localShapeTreeCache.containsKey(referenceShapeTreeUri)) {
                    // If the model contains the referenced ShapeTree, go ahead and parse and cache it
                    recursivelyParseShapeTree(model, model.getResource(referenceShapeTreeUri.toString()));
                }

                // Create the object that defines there relation between a ShapeTree and its children
                ReferencedShapeTree referencedShapeTree = new ReferencedShapeTree(referenceShapeTreeUri, shapePath);
                shapeTree.getReferences().add(referencedShapeTree);
            }
        }

        // Containers are expected to have contents
        if (resource.hasProperty(model.createProperty(ShapeTreeVocabulary.CONTAINS)) && !shapeTree.getExpectedResourceType().equals(ShapeTreeVocabulary.SHAPETREE_CONTAINER)) {
            throw new ShapeTreeException(400, "Contents predicate not expected outside of #ShapeTreeContainer Types");
        }
        if (shapeTree.getExpectedResourceType().equals(ShapeTreeVocabulary.SHAPETREE_CONTAINER)) {
            List<URI> uris = getURLListValue(model, resource, ShapeTreeVocabulary.CONTAINS);
            shapeTree.setContains(uris);
            for (URI uri : uris) {
                if (!localShapeTreeCache.containsKey(uri) && !isShapeTreeAllowIRI(uri)) {
                    recursivelyParseShapeTree(model, model.getResource(uri.toString()));
                }
            }
        }
    }

    private static boolean isShapeTreeAllowIRI(URI uri) throws URISyntaxException {
        return uri.equals(new URI(ShapeTreeVocabulary.ALLOW_ALL)) ||
                uri.equals(new URI(ShapeTreeVocabulary.ALLOW_NONE)) ||
                uri.equals(new URI(ShapeTreeVocabulary.ALLOW_RESOURCES)) ||
                uri.equals(new URI(ShapeTreeVocabulary.ALLOW_CONTAINERS)) ||
                uri.equals(new URI(ShapeTreeVocabulary.ALLOW_NON_RDF_SOURCES));
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
