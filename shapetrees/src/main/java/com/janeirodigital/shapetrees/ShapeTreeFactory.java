package com.janeirodigital.shapetrees;

import com.janeirodigital.shapetrees.model.ReferencedShapeTree;
import com.janeirodigital.shapetrees.model.ShapeTree;
import lombok.extern.slf4j.Slf4j;
import org.apache.jena.graph.Node;
import org.apache.jena.graph.Node_URI;
import org.apache.jena.rdf.model.*;
import org.apache.jena.riot.RDFDataMgr;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Slf4j
public class ShapeTreeFactory {
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
        Model model = RDFDataMgr.loadModel(shapeTreeURI.toString());

        Resource resource = model.getResource(shapeTreeURI.toString());
        if (resource != null) {
            recursivelyParseShapeTree(model, resource);
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

        ShapeTree shapeTree = new ShapeTree();
        // Set the URI as the ID (string representation)
        shapeTree.setId(shapeTreeURIString);
        // Set the expected resource type
        shapeTree.setRdfResourceType(getStringValue(model, resource, ShapeTreeVocabulary.EXPECTS_TYPE));
        // Set URI Template
        shapeTree.setUriTemplate(getStringValue(model, resource, ShapeTreeVocabulary.MATCHES_URI_TEMPLATE));
        // Set Shape URI
        shapeTree.setShapeUri(getStringValue(model, resource, ShapeTreeVocabulary.VALIDATED_BY));
        // Set Label
        shapeTree.setLabel(getStringValue(model, resource, RDFS_LABEL));
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
                    if (model.getResource(referenceShapeTreeUri.toString()) != null) {
                        recursivelyParseShapeTree(model, model.getResource(referenceShapeTreeUri.toString()));
                    }
                }

                // Create the object that defines there relation between a ShapeTree and its children
                ReferencedShapeTree referencedShapeTree = new ReferencedShapeTree(referenceShapeTreeUri, shapePath);
                shapeTree.getReferences().add(referencedShapeTree);
            }
        }

        // Containers are expected to have contents
        if (resource.hasProperty(model.createProperty(ShapeTreeVocabulary.CONTAINS)) && !shapeTree.getRdfResourceType().contains("Container")) {
            throw new ShapeTreeException(400, "Contents predicate not expected outside of #Container RDF Types");
        }
        if (shapeTree.getRdfResourceType().contains("Container")) {
            List<URI> uris = getURLListValue(model, resource, ShapeTreeVocabulary.CONTAINS);
            shapeTree.setContents(uris);
            if (uris != null) {
                for (URI uri : uris) {
                    if (!localShapeTreeCache.containsKey(uri)) {
                        recursivelyParseShapeTree(model, model.getResource(uri.toString()));
                    }
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
