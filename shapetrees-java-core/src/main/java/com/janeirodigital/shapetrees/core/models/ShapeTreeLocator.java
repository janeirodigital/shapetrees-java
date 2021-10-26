package com.janeirodigital.shapetrees.core.models;

import com.janeirodigital.shapetrees.core.ShapeTreeFactory;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.helpers.GraphHelper;
import com.janeirodigital.shapetrees.core.vocabularies.RdfVocabulary;
import com.janeirodigital.shapetrees.core.vocabularies.ShapeTreeVocabulary;
import lombok.Getter;
import org.apache.commons.lang3.RandomStringUtils;
import org.apache.jena.graph.Graph;
import org.apache.jena.graph.Node;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Triple;
import org.apache.jena.vocabulary.RDF;

import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import static com.janeirodigital.shapetrees.core.helpers.GraphHelper.urlToUri;


/**
 * ShapeTreeLocator
 *
 * Shape Trees, §3:  A shape tree locator associates a managed resource with one or more shape trees. No more
 * than one shape tree locator may be associated with a managed resource. A shape tree locator includes
 * one or more shape tree locations via st:location.
 * https://shapetrees.org/TR/specification/#locator
*/
@Getter
public class ShapeTreeLocator {

    private URL id;
    private List<ShapeTreeLocation> locations = new ArrayList<>();   // Each ShapeTreeLocator has one or more ShapeTreeLocations

    public ShapeTreeLocator(URL id) {
        this.id = id;
    }

    protected URL getUrl() {
        return this.id;
    }

    public Graph getGraph() throws ShapeTreeException {

        Graph locatorGraph = GraphHelper.getEmptyGraph();
        String locatorSubject = this.getUrl().toString();

        // <> a st:ShapeTreeLocator
        locatorGraph.add(GraphHelper.newTriple(locatorSubject, RDF.type.toString(), GraphHelper.knownUrl(ShapeTreeVocabulary.SHAPETREE_LOCATOR)));

        // For each location create a blank node and populate
        for (ShapeTreeLocation location : this.locations) {

            // <#locator> st:contains <location1>, <location2>
            locatorGraph.add(GraphHelper.newTriple(locatorSubject, ShapeTreeVocabulary.LOCATION, location.getUrl()));

            final URI subject = urlToUri(location.getUrl());
            locatorGraph.add(GraphHelper.newTriple(subject, URI.create(ShapeTreeVocabulary.HAS_SHAPE_TREE), location.getShapeTree()));
            locatorGraph.add(GraphHelper.newTriple(subject, URI.create(ShapeTreeVocabulary.HAS_MANAGED_RESOURCE), location.getManagedResource()));
            locatorGraph.add(GraphHelper.newTriple(subject, URI.create(ShapeTreeVocabulary.HAS_ROOT_SHAPE_TREE_LOCATION), location.getRootShapeTreeLocation()));

            if (location.getShape() != null) {
                locatorGraph.add(GraphHelper.newTriple(subject, URI.create(ShapeTreeVocabulary.SHAPE), location.getShape()));
            }

            if (location.getFocusNode() != null) {
                locatorGraph.add(GraphHelper.newTriple(subject, URI.create(ShapeTreeVocabulary.FOCUS_NODE), location.getFocusNode()));
            }

        }

        return locatorGraph;
    }

    public void addShapeTreeLocation(ShapeTreeLocation location) throws ShapeTreeException {

        if (this.locations == null || location == null) {
            throw new ShapeTreeException(500, "Must provide a non-null location to an initialized List of ShapeTreeLocations");
        }

        if (location.getUrl() == null) {
            location.setUrl(this.mintLocation());
        }

        if (!this.locations.isEmpty()) {
            for (ShapeTreeLocation existingLocation : this.locations) {
                if (existingLocation.equals(location)) {
                    throw new ShapeTreeException(422, "Identical shape tree location cannot be added to Shape Tree Locator: " + this.id);
                }
            }
        }

        this.locations.add(location);

    }

    // Generates or "mints" a URL for a new location contained in the locator
    public URL mintLocation() {

        String fragment = RandomStringUtils.random(8, true, true);
        String locationString = this.getUrl().toString() + "#" + fragment;

        final URL locationUrl;
        try {
            locationUrl = new URL(locationString);
        } catch (MalformedURLException ex) {
            throw new IllegalStateException("Minted illegal URL <" + locationString + "> - " + ex.getMessage());
        }

        for (ShapeTreeLocation location : this.locations) {
            if (location.getUrl() != null && location.getUrl().equals(locationUrl)) {
                // If we somehow managed to randomly generate a location URL that already exists, generate another
                return mintLocation();
            }
        }
        return locationUrl;
    }

    public ShapeTreeLocation getContainingShapeTreeLocation() throws ShapeTreeException {

        for (ShapeTreeLocation location : this.locations) {
            ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(location.getShapeTree());
            if (shapeTree.getContains() != null && !shapeTree.getContains().isEmpty()) {
                return location;
            }
        }

        return null;
    }

    public static ShapeTreeLocator getShapeTreeLocatorFromGraph(URL id, Graph shapeTreeMetadataGraph) throws ShapeTreeException {

         ShapeTreeLocator locator = new ShapeTreeLocator(id);

        // Look up the ShapeTreeLocator in the Metadata Graph via (any subject node, rdf:type, st:ShapeTreeLocator)
        List<Triple> shapeTreeLocatorTriples = shapeTreeMetadataGraph.find(Node.ANY,
                                                                           NodeFactory.createURI(RdfVocabulary.TYPE),
                                                                           NodeFactory.createURI(ShapeTreeVocabulary.SHAPETREE_LOCATOR)).toList();

        // Shape Trees, §3: No more than one shape tree locator may be associated with a managed resource.
        // https://shapetrees.org/TR/specification/#locator
        if (shapeTreeLocatorTriples.size() > 1) {
            throw new IllegalStateException("Multiple ShapeTreeLocator instances found: " + shapeTreeLocatorTriples.size());
        } else if (shapeTreeLocatorTriples.isEmpty()) {
            // Given the fact that a locator resource exists, there should never be a case where the locator resource
            // exists but no locator is found inside of it.
            throw new IllegalStateException("No ShapeTreeLocator instances found: " + shapeTreeLocatorTriples.size());
        }

        // Get the URL of the ShapeTreeLocator subject node
        String locatorUrl = shapeTreeLocatorTriples.get(0).getSubject().getURI();

        // Look up ShapeTreeLocation nodes (locator subject node, st:location, any st:location nodes).
        // There should be one result per nested ShapeTreeLocation, each identified by a unique url.
        // Shape Trees, §3: A shape tree locator includes one or more shape tree locations via st:location
        // https://shapetrees.org/TR/specification/#locator
        final Node s = NodeFactory.createURI(locatorUrl);
        final Node stLocation = NodeFactory.createURI(ShapeTreeVocabulary.LOCATION);
        List<Triple> locationNodes = shapeTreeMetadataGraph.find(s, stLocation, Node.ANY).toList();

        // For each st:location node, extract a new ShapeTreeLocation
        for (Triple locationNode : locationNodes) {
            ShapeTreeLocation location = null;
            try {
                location = ShapeTreeLocation.getShapeTreeLocationFromGraph(new URL(locationNode.getObject().getURI()), shapeTreeMetadataGraph);
            } catch (MalformedURLException e) {
                throw new ShapeTreeException(500, "Object of { "+s+" "+ stLocation +" "+locationNode.getObject()+" } must be a URL.");
            }
            locator.locations.add(location);
        }

        return locator;

    }

    public ShapeTreeLocation getShapeTreeLocationForShapeTree(URL shapeTreeUrl) {

        if (this.locations == null || this.locations.isEmpty()) { return null; }

        for (ShapeTreeLocation location : this.locations) {
            if (location.getShapeTree().equals(shapeTreeUrl)) { return location; }
        }
        return null;
    }

    public void removeShapeTreeLocation(ShapeTreeLocation removeLocation) {

        if (removeLocation == null) {
            throw new IllegalStateException("Cannot remove a null ShapeTreeLocation");
        }

        if (this.locations == null || this.locations.isEmpty()) {
            throw new IllegalStateException("Cannot remove ShapeTreeLocations from empty set");
        }

        if (!this.locations.remove(removeLocation)) {
            throw new IllegalStateException("Cannot remove ShapeTreeLocation that does not exist in set");
        }

    }

    public void removeShapeTreeLocationForShapeTree(URL shapeTreeUrl) {
        removeShapeTreeLocation(getShapeTreeLocationForShapeTree(shapeTreeUrl));
    }
}
