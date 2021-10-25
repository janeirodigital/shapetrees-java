package com.janeirodigital.shapetrees.core.models;

import com.janeirodigital.shapetrees.core.DocumentResponse;
import com.janeirodigital.shapetrees.core.SchemaCache;
import com.janeirodigital.shapetrees.core.ShapeTreeFactory;
import com.janeirodigital.shapetrees.core.ShapeTreeResource;
import com.janeirodigital.shapetrees.core.contentloaders.ExternalDocumentLoader;
import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.core.enums.RecursionMethods;
import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.helpers.GraphHelper;
import fr.inria.lille.shexjava.GlobalFactory;
import fr.inria.lille.shexjava.schema.Label;
import fr.inria.lille.shexjava.schema.ShexSchema;
import fr.inria.lille.shexjava.schema.parsing.ShExCParser;
import fr.inria.lille.shexjava.validation.RecursiveValidation;
import fr.inria.lille.shexjava.validation.ValidationAlgorithm;
import lombok.Getter;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.rdf.api.IRI;
import org.apache.commons.rdf.jena.JenaRDF;
import org.apache.jena.graph.Graph;
import org.apache.jena.graph.GraphUtil;
import org.apache.jena.graph.Node;
import org.jetbrains.annotations.NotNull;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.Serializable;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.*;

@Getter
@Slf4j
public class ShapeTree {
    private ExternalDocumentLoader externalDocumentLoader;
    @NotNull
    final private String id;
    @NotNull
    final private String expectedResourceType;
    final private String shape;
    final private String label;
    final private String supports;
    @NotNull
    final private List<URL> contains;
    @NotNull
    final private List<ReferencedShapeTree> references;

    public ShapeTree(ExternalDocumentLoader externalDocumentLoader,
                     @NotNull String id,
                     @NotNull String expectedResourceType,
                     String label,
                     String shape,
                     String supports,
                     @NotNull List<ReferencedShapeTree> references,
                     @NotNull List<URL> contains) {
        this.externalDocumentLoader = externalDocumentLoader;
        this.id = id;
        this.expectedResourceType = expectedResourceType;
        this.label = label;
        this.shape = shape;
        this.supports = supports;
        this.references = references;
        this.contains = contains;
    }

    public URL getURL() throws MalformedURLException {
        return new URL(this.id);
    }

    public ValidationResult validateResource(ShapeTreeResource.Primary targetResource) throws ShapeTreeException, MalformedURLException, URISyntaxException {
        return validateResource(targetResource, null);
    }

    public ValidationResult validateResource(ShapeTreeResource.Primary targetResource, URL focusNodeURL) throws ShapeTreeException, MalformedURLException, URISyntaxException {
        Graph bodyGraph = null;

        if (targetResource.getResourceType() != ShapeTreeResourceType.NON_RDF) {
            bodyGraph = GraphHelper.readStringIntoGraph(targetResource.getUrl(),
                    targetResource.getBody(),
                    targetResource.getAttributes().firstValue(HttpHeaders.CONTENT_TYPE.getValue()).orElse(null));
        }
        
        return validateResource(targetResource.getName(), targetResource.getResourceType(), bodyGraph, focusNodeURL);
    }

    public ValidationResult validateResource(String requestedName, ShapeTreeResourceType resourceType, Graph bodyGraph, URL focusNodeURL) throws ShapeTreeException, MalformedURLException, URISyntaxException {

        // Check whether the proposed resource is the same type as what is expected by the shape tree
        if (!this.expectedResourceType.equals(resourceType.getValue())) {
            return new ValidationResult(false, this, "Resource type " + resourceType + " is invalid. Expected " + this.expectedResourceType);
        }

        // If a label is specified, check if the proposed name is the same
        if (this.label != null && !this.label.equals(requestedName)) {
            return new ValidationResult(false, this, "Proposed resource name " + requestedName + " is invalid. Expected " + this.label);
        }

        // If the shape tree specifies a shape to validate, perform shape validation
        if (this.shape != null) {
            return this.validateGraph(bodyGraph, focusNodeURL);
        }

        // Allow if we fall through to here. Focus node is set to null because we only get here if no shape validation was performed
        return new ValidationResult(true, this, this, null);

    }

    public ValidationResult validateGraph(Graph graph, URL focusNodeURL) throws ShapeTreeException, MalformedURLException, URISyntaxException {
        // if (true) return new ValidationResult(true, this, this, focusNodeURL); // [debug] ShExC parser brings debugger to its knees
        if (this.shape == null) {
            throw new ShapeTreeException(400, "Attempting to validate a shape for ShapeTree " + this.id + "but it doesn't specify one");
        }

        URI resolvedShapeURI = URI.create(this.id).resolve(this.shape);

        URI shapeResourceURI = resolvedShapeURI;
        if (shapeResourceURI.getFragment() != null) {
            shapeResourceURI = new URI(shapeResourceURI.getScheme(), shapeResourceURI.getSchemeSpecificPart(), null);
        }

        URL shapeResourceURL = shapeResourceURI.toURL();
        ShexSchema schema;
        if (SchemaCache.isInitialized() && SchemaCache.containsSchema(shapeResourceURL)) {
            log.debug("Found cached schema {}", shapeResourceURL);
            schema = SchemaCache.getSchema(shapeResourceURL);
        } else {
            log.debug("Did not find schema in cache {} will retrieve and parse", shapeResourceURL);
            DocumentResponse shexShapeContents = this.externalDocumentLoader.loadExternalDocument(shapeResourceURL);
            if (shexShapeContents == null || shexShapeContents.getBody() == null || shexShapeContents.getBody().isEmpty()) {
                throw new ShapeTreeException(400, "Attempting to validate a ShapeTree (" + this.id + ") - Shape at (" + shapeResourceURL + ") is not found or is empty");
            }

            String shapeBody = shexShapeContents.getBody();
            InputStream stream = new ByteArrayInputStream(shapeBody.getBytes(StandardCharsets.UTF_8));
            ShExCParser shexCParser = new ShExCParser();
            try {
                schema = new ShexSchema(GlobalFactory.RDFFactory,shexCParser.getRules(stream),shexCParser.getStart());
                if (SchemaCache.isInitialized()) {
                    SchemaCache.putSchema(shapeResourceURL, schema);
                }
            } catch (Exception ex) {
                throw new ShapeTreeException(500, "Error parsing ShEx schema - " + ex.getMessage());
            }
        }

        // Tell ShExJava we want to use Jena as our graph library
        JenaRDF jenaRDF = new org.apache.commons.rdf.jena.JenaRDF();
        GlobalFactory.RDFFactory = jenaRDF;

        ValidationAlgorithm validation = new RecursiveValidation(schema, jenaRDF.asGraph(graph));
        Label shapeLabel = new Label(GlobalFactory.RDFFactory.createIRI(this.shape));

        if (focusNodeURL != null) {

            IRI focusNode = GlobalFactory.RDFFactory.createIRI(focusNodeURL.toString());
            log.debug("Validating Shape Label = {}, Focus Node = {}", shapeLabel.toPrettyString(), focusNode.getIRIString());
            validation.validate(focusNode, shapeLabel);
            boolean valid = validation.getTyping().isConformant(focusNode, shapeLabel);
            if (valid) {
                return new ValidationResult(valid, this, this, focusNodeURL);
            } else {
                return new ValidationResult(valid, this, "Failed to validate: " + shapeLabel.toPrettyString());
            }


        } else {

            List<Node> evaluateNodes = GraphUtil.listSubjects(graph, Node.ANY, Node.ANY).toList();

            for (Node evaluateNode : evaluateNodes) {

                IRI node = GlobalFactory.RDFFactory.createIRI(evaluateNode.getURI());
                validation.validate(node, shapeLabel);
                boolean valid = validation.getTyping().isConformant(node, shapeLabel);

                if (valid) { return new ValidationResult(valid, this, this, new URL(evaluateNode.getURI())); }

            }

            return new ValidationResult(false, null, "Failed to validate: " + shapeLabel.toPrettyString());

        }
    }

    public ValidationResult validateContainedResource(ShapeTreeResource.Primary containedResource) throws ShapeTreeException, MalformedURLException, URISyntaxException {

        if (this.contains == null || this.contains.isEmpty()) {
            // The contained resource is permitted because this shape tree has no restrictions on what it contains
            return new ValidationResult(true, this, this, null);
        }

        return validateContainedResource(containedResource, null, null);

    }

    public ValidationResult validateContainedResource(ShapeTreeResource.Primary containedResource, URL targetShapeTreeURL, URL focusNodeURL) throws ShapeTreeException, MalformedURLException, URISyntaxException {

        Graph containedResourceGraph = null;
// !! containedResource.getGraph().get();
        if (containedResource.getResourceType() != ShapeTreeResourceType.NON_RDF) {
            containedResourceGraph = GraphHelper.readStringIntoGraph(containedResource.getUrl(),
                    containedResource.getBody(),
                    containedResource.getAttributes().firstValue(HttpHeaders.CONTENT_TYPE.getValue()).orElse(null));
        }

        return validateContainedResource(containedResource.getName(), containedResource.getResourceType(), targetShapeTreeURL, containedResourceGraph, focusNodeURL);

    }

    public ValidationResult validateContainedResource(String requestedName, ShapeTreeResourceType resourceType, URL targetShapeTreeURL, Graph bodyGraph, URL focusNodeURL) throws ShapeTreeException, MalformedURLException, URISyntaxException {

        if (this.contains == null || this.contains.isEmpty()) {
            // The contained resource is permitted because this shape tree has no restrictions on what it contains
            return new ValidationResult(true, this, this, null);
        }

        if (targetShapeTreeURL != null) {
            // And if it exists in st:contains
            if (this.contains.contains(targetShapeTreeURL)) {
                ShapeTree targetShapeTree = ShapeTreeFactory.getShapeTree(targetShapeTreeURL);
                // Evaluate the shape tree against the attributes of the proposed resources
                ValidationResult result = targetShapeTree.validateResource(requestedName, resourceType, bodyGraph, focusNodeURL);
                // Continue if the proposed attributes were not a match
                if (Boolean.FALSE.equals(result.getValid())) {
                    return new ValidationResult(false, null, "Failed to validate " + targetShapeTree.getId());
                }
                // Return the successful validation result, including the matching shape tree
                return new ValidationResult(true, this, targetShapeTree, result.getMatchingFocusNode());
            } else {
                // We have a misunderstanding if a target shape tree is supplied but doesn't exist in st:contains
                return new ValidationResult(false, this, "Target shape tree " +
                        targetShapeTreeURL + "was provided but not found in st:contains");
            }

        } else {
            // For each shape tree in st:contains
            for (URL containsShapeTreeURL : getPrioritizedContains()) {

                ShapeTree containsShapeTree = ShapeTreeFactory.getShapeTree(containsShapeTreeURL);
                if (containsShapeTree == null) { continue; } // Continue if the shape tree isn't gettable

                // Evaluate the shape tree against the attributes of the proposed resources
                ValidationResult result = containsShapeTree.validateResource(requestedName, resourceType, bodyGraph, focusNodeURL);
                // Continue if the proposed attributes were not a match
                if (Boolean.FALSE.equals(result.getValid())) { continue; }
                // Return the successful validation result
                return new ValidationResult(true, this, containsShapeTree, result.getMatchingFocusNode());
            }
        }

        return new ValidationResult(false, null, "Failed to validate shape tree: " + this.id);

    }

    public Iterator<ReferencedShapeTree> getReferencedShapeTrees() throws MalformedURLException, ShapeTreeException {
        return getReferencedShapeTrees(RecursionMethods.DEPTH_FIRST);
    }

    public Iterator<ReferencedShapeTree> getReferencedShapeTrees(RecursionMethods recursionMethods) throws MalformedURLException, ShapeTreeException {
        return getReferencedShapeTreesList(recursionMethods).iterator();
    }

    // Return the list of shape tree contains by priority from most to least strict
    public List<URL> getPrioritizedContains() {

        List<URL> prioritized = new ArrayList<>(this.contains);
        Collections.sort(prioritized, new SortByShapeTreeContainsPriority());
        return prioritized;

    }

    private List<ReferencedShapeTree> getReferencedShapeTreesList(RecursionMethods recursionMethods) throws MalformedURLException, ShapeTreeException {
        if (recursionMethods.equals(RecursionMethods.BREADTH_FIRST)) {
            return getReferencedShapeTreesListBreadthFirst();
        } else {
            List<ReferencedShapeTree> referencedShapeTrees = new ArrayList<>();
            return getReferencedShapeTreesListDepthFirst(this.getReferences(), referencedShapeTrees);
        }
    }

    private List<ReferencedShapeTree> getReferencedShapeTreesListBreadthFirst() throws MalformedURLException, ShapeTreeException {
        List<ReferencedShapeTree> referencedShapeTrees = new ArrayList<>();
        Queue<ReferencedShapeTree> queue = new LinkedList<>(this.getReferences());

        while (!queue.isEmpty()) {
            ReferencedShapeTree currentShapeTree = queue.poll();
            referencedShapeTrees.add(currentShapeTree);
            ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(currentShapeTree.getReferencedShapeTreeURL());
            if (shapeTree != null) {
                List<ReferencedShapeTree> currentReferencedShapeTrees = shapeTree.getReferences();
                if (currentReferencedShapeTrees != null) {
                    queue.addAll(currentReferencedShapeTrees);
                }
            }
        }
        return referencedShapeTrees;
    }

    private List<ReferencedShapeTree> getReferencedShapeTreesListDepthFirst(List<ReferencedShapeTree> currentReferencedShapeTrees, List<ReferencedShapeTree> referencedShapeTrees) throws MalformedURLException, ShapeTreeException {
        for (ReferencedShapeTree currentShapeTreeReference : currentReferencedShapeTrees) {
            referencedShapeTrees.add(currentShapeTreeReference);
            ShapeTree currentReferencedShapeTree = ShapeTreeFactory.getShapeTree(currentShapeTreeReference.getReferencedShapeTreeURL());
            if (currentReferencedShapeTree != null) {
                referencedShapeTrees = getReferencedShapeTreesListDepthFirst(currentReferencedShapeTree.getReferences(), referencedShapeTrees);
            }
        }
        return referencedShapeTrees;
    }

    public void setExternalDocumentLoader(ExternalDocumentLoader externalDocumentLoader) {
        this.externalDocumentLoader = externalDocumentLoader;
    }
}

class SortByShapeTreeContainsPriority implements Comparator<URL>, Serializable
{
    // Used for sorting shape trees in st:contains by most to least strict
    @SneakyThrows
    @Override
    public int compare(URL stUri1, URL stUri2) {

        ShapeTree st1 = ShapeTreeFactory.getShapeTree(stUri1);
        ShapeTree st2 = ShapeTreeFactory.getShapeTree(stUri2);

        Integer st1Priority = 0;
        Integer st2Priority = 0;

        if (st1.getShape() != null) { st1Priority += 2; }
        if (st1.getLabel() != null) { st1Priority++; }
        if (st1.getExpectedResourceType() != null) { st1Priority++; }

        if (st2.getShape() != null) { st2Priority += 2; }
        if (st2.getLabel() != null) { st2Priority++; }
        if (st2.getExpectedResourceType() != null) { st2Priority++; }

        // Reversed to ensure ordering goes from most strict to least
        return Integer.compare(st2Priority, st1Priority);

    }

}
