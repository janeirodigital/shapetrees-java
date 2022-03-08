package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.vocabularies.ShapeTreeVocabulary;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.apache.jena.graph.Node;
import org.apache.jena.graph.Node_URI;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;

import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static com.janeirodigital.shapetrees.core.helpers.GraphHelper.urlToUri;

/**
 * Provides a factory to look up and initialize ShapeTrees.
 * Includes a simple in-memory local cache to avoid repeated fetching of
 * remote shape tree resources.
 */
@Slf4j
public class ShapeTreeFactory {

    private ShapeTreeFactory() { }

    private static final String RDFS_LABEL = "http://www.w3.org/2000/01/rdf-schema#label";
    @Getter
    private static final Map<URI, ShapeTree> localShapeTreeCache = new HashMap<>();

    /**
     * Looks up and parses the shape tree at <code>shapeTreeUrl</code>.
     * Shape trees linked via st:contains and st:references are parsed
     * recursively. Maintains a cache to avoid parsing the same shape tree
     * more than once.
     * @param shapeTreeUrl URL of the shape tree to get
     * @return Parsed and initialized shape tree
     * @throws ShapeTreeException
     */
    public static ShapeTree
    getShapeTree(URL shapeTreeUrl) throws ShapeTreeException {

        log.debug("Parsing shape tree: <{}>", shapeTreeUrl);

        if (localShapeTreeCache.containsKey(urlToUri(shapeTreeUrl))) {
            log.debug("<{}> previously cached -- returning", shapeTreeUrl.toString());
            return localShapeTreeCache.get(urlToUri(shapeTreeUrl));
        }

        // Load the entire shape tree resource (which may contain multiple shape trees)
        ShapeTreeResource shapeTreeResource = ShapeTreeResource.getShapeTreeResource(shapeTreeUrl);
        Model resourceModel = shapeTreeResource.getModel();
        Resource shapeTreeNode = resourceModel.getResource(shapeTreeUrl.toString());

        // Load and set the expected resource type
        final URL expectsType = getUrlValue(resourceModel, shapeTreeNode, ShapeTreeVocabulary.EXPECTS_TYPE, shapeTreeUrl);
        if (expectsType == null) throw new ShapeTreeException(500, "Shape Tree :expectsType not found");
        // Load and set the Shape URL
        final URL shape = getUrlValue(resourceModel, shapeTreeNode, ShapeTreeVocabulary.SHAPE, shapeTreeUrl);
        // Load and set Label
        final String label = getStringValue(resourceModel, shapeTreeNode, RDFS_LABEL);
        // Load and set contains list
        final List<URL> contains = getContains(resourceModel, shapeTreeNode, shapeTreeUrl);
        contains.sort((URL l, URL r) -> l.toString().compareTo(r.toString()));
        // Load and set references list
        final List<ShapeTreeReference> references = getReferences(resourceModel, shapeTreeNode, shapeTreeUrl);
        references.sort((ShapeTreeReference l, ShapeTreeReference r) -> l.getReferenceUrl().toString().compareTo(r.getReferenceUrl().toString()));

        if (!contains.isEmpty() && !expectsType.toString().equals(ShapeTreeVocabulary.CONTAINER)) {
            throw new ShapeTreeException(400, "Only a container can be expected to have st:contains");
        }

        ShapeTree shapeTree = new ShapeTree(shapeTreeUrl, expectsType, label, shape, references, contains);

        localShapeTreeCache.put(urlToUri(shapeTreeUrl), shapeTree);

        // Recursively parse contained shape trees
        for (URL containedUrl : contains) { getShapeTree(containedUrl); }

        // Recursively parse referenced shape trees
        for (ShapeTreeReference reference : references) { getShapeTree(reference.getReferenceUrl()); }

        return shapeTree;

    }

    /**
     * Get the list of URLs linked via st:contains by the shape tree being parsed.
     * @param resourceModel RDF Model representing the shape tree resource
     * @param shapeTreeNode RDF Node of the shape tree
     * @param shapeTreeUrl URL of the shape tree
     * @return List of URLs linked via st:contains
     * @throws ShapeTreeException
     */
    private static List<URL>
    getContains(Model resourceModel, Resource shapeTreeNode, URL shapeTreeUrl) throws ShapeTreeException {
        try {
            return getURLListValue(resourceModel, shapeTreeNode, ShapeTreeVocabulary.CONTAINS);
        } catch (MalformedURLException | ShapeTreeException ex) {
            throw new ShapeTreeException(500, "List <"+ shapeTreeUrl +"> contains malformed URL: " + ex.getMessage());
        }
    }

    /**
     * Get the list of ShapeTreeReferences linked via st:references by the shape tree being parsed.
     * @param resourceModel RDF Model representing the shape tree resource
     * @param shapeTreeNode RDF Node of the shape tree
     * @param shapeTreeUrl URL of the shape tree
     * @return List of ShapeTreeReferences linked via st:references
     * @throws ShapeTreeException
     */
    private static List<ShapeTreeReference>
    getReferences(Model resourceModel, Resource shapeTreeNode, URL shapeTreeUrl) throws ShapeTreeException {

        ArrayList<ShapeTreeReference> references = new ArrayList<>();
        Property referencesProperty = resourceModel.createProperty(ShapeTreeVocabulary.REFERENCES);

        if (shapeTreeNode.hasProperty(referencesProperty)) { // TODO: arbitrarily pics from n objects where 1 expected
            List<Statement> referenceStatements = shapeTreeNode.listProperties(referencesProperty).toList();
            for (Statement referenceStatement : referenceStatements) {

                Resource referenceResource = referenceStatement.getObject().asResource();

                final URL referencedShapeTreeUrl = getUrlValue(resourceModel, referenceResource, ShapeTreeVocabulary.REFERENCES_SHAPE_TREE, shapeTreeUrl);
                if (referencedShapeTreeUrl == null) {
                    throw new ShapeTreeException(400, "expected <" + shapeTreeUrl + "> reference " + referenceResource.toString() + " to have one <" + ShapeTreeVocabulary.REFERENCES_SHAPE_TREE + "> property");
                }

                String viaShapePath = getStringValue(resourceModel, referenceResource, ShapeTreeVocabulary.VIA_SHAPE_PATH);
                URL viaPredicate = getUrlValue(resourceModel, referenceResource, ShapeTreeVocabulary.VIA_PREDICATE, shapeTreeUrl);
                references.add(new ShapeTreeReference(referencedShapeTreeUrl, viaShapePath, viaPredicate));
            }
        }
        return references;
    }

    /**
     * Validate and get a single URL value linked to a shape tree by the supplied <code>predicate</code>.
     * @param model RDF Model representing the shape tree resource
     * @param resource RDF Node of the shape tree
     * @param predicate Predicate to match
     * @param shapeTreeUrl URL of the shape tree
     * @return URL value linked via <code>predicate</code>
     * @throws ShapeTreeException
     */
    private static URL getUrlValue(Model model, Resource resource, String predicate, URL shapeTreeUrl) throws ShapeTreeException {
        Property property = model.createProperty(predicate);
        if (resource.hasProperty(property)) {
            Statement statement = resource.getProperty(property);
            final RDFNode object = statement.getObject();
            if (object.isURIResource()) {
                try {
                    return new URL(object.asResource().getURI());
                } catch (MalformedURLException ex) {
                    throw new IllegalStateException("Malformed ShapeTree <" + shapeTreeUrl + ">: Jena URIResource <" + object + "> didn't parse as URL - " + ex.getMessage());
                }
            } else {
                throw new ShapeTreeException(500, "Malformed ShapeTree <" + shapeTreeUrl + ">: expected " + object + " to be a URL");
            }
        }
        return null;
    }

    /**
     * Validate and get a single String value linked to a shape tree by the supplied <code>predicate</code>.
     * @param model RDF Model representing the shape tree resource
     * @param resource RDF Node of the shape tree
     * @param predicate Predicate to match
     * @return String value linked via <code>predicate</code>
     * @throws ShapeTreeException
     */
    private static String getStringValue(Model model, Resource resource, String predicate) throws ShapeTreeException {
        Property property = model.createProperty(predicate);
        if (resource.hasProperty(property)) {
            Statement statement = resource.getProperty(property);
            if (statement.getObject().isLiteral()) {
                return statement.getObject().asLiteral().getString();
            } else if (statement.getObject().isURIResource()) {
                return statement.getObject().asResource().getURI();
            } else {
                throw new ShapeTreeException(500, "Cannot determine object type when converting from string for: " + predicate);
            }

        }
        return null;
    }

    /**
     * Validate and get list of URLs linked to a shape tree by the supplied <code>predicate</code>.
     * @param model RDF Model representing the shape tree resource
     * @param resource RDF Node of the shape tree
     * @param predicate Predicate to match
     * @return List of URLs linked via <code>predicate</code>
     * @throws MalformedURLException
     * @throws ShapeTreeException
     */
    private static List<URL> getURLListValue(Model model, Resource resource, String predicate) throws MalformedURLException, ShapeTreeException {
        List<URL> urls = new ArrayList<>();
        Property property = model.createProperty(predicate);
        if (resource.hasProperty(property)) {
            List<Statement> propertyStatements = resource.listProperties(property).toList();
            for (Statement propertyStatement : propertyStatements) {
                Node propertyNode = propertyStatement.getObject().asNode();
                if (propertyNode instanceof Node_URI) {
                    URL contentUrl = new URL(propertyNode.getURI());
                    urls.add(contentUrl);
                } else {
                    throw new ShapeTreeException(500, "Must provide a valid URI in URI listing");
                }
            }
        }
        return urls;
    }

    /**
     * Clears the local shape tree cache
     */
    public static void
    clearCache() {
        localShapeTreeCache.clear();
    }

}
