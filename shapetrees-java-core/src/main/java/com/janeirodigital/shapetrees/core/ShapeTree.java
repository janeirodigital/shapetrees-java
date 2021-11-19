package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.comparators.ShapeTreeContainsPriority;
import com.janeirodigital.shapetrees.core.contentloaders.DocumentLoaderManager;
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
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.rdf.api.IRI;
import org.apache.commons.rdf.jena.JenaRDF;
import org.apache.jena.graph.Graph;
import org.apache.jena.graph.GraphUtil;
import org.apache.jena.graph.Node;
import org.jetbrains.annotations.NotNull;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.*;

import static com.janeirodigital.shapetrees.core.helpers.GraphHelper.urlToUri;

@Getter
@Slf4j
public class ShapeTree {
    @NotNull
    private final URL id;
    @NotNull
    private final URL expectedResourceType;
    private final URL shape;
    private final String label;
    @NotNull
    private final List<URL> contains;
    @NotNull
    private final List<ShapeTreeReference> references;

    public ShapeTree(@NotNull URL id,
                     @NotNull URL expectedResourceType,
                     String label,
                     URL shape,
                     @NotNull List<ShapeTreeReference> references,
                     @NotNull List<URL> contains) {
        this.id = id;
        this.expectedResourceType = expectedResourceType;
        this.label = label;
        this.shape = shape;
        this.references = references;
        this.contains = contains;
    }

    public ValidationResult validateResource(ManageableResource targetResource) throws ShapeTreeException {
        return validateResource(targetResource, null);
    }

    public ValidationResult validateResource(ManageableResource targetResource, List<URL> focusNodeUrls) throws ShapeTreeException {
        Graph bodyGraph = null;

        if (targetResource.getResourceType() != ShapeTreeResourceType.NON_RDF) {
            bodyGraph = GraphHelper.readStringIntoGraph(urlToUri(targetResource.getUrl()),
                    targetResource.getBody(),
                    targetResource.getAttributes().firstValue(HttpHeaders.CONTENT_TYPE.getValue()).orElse(null));
        }
        
        return validateResource(targetResource.getName(), targetResource.getResourceType(), bodyGraph, focusNodeUrls);
    }

    public ValidationResult validateResource(String requestedName, ShapeTreeResourceType resourceType, Graph bodyGraph, List<URL> focusNodeUrls) throws ShapeTreeException {

        // Check whether the proposed resource is the same type as what is expected by the shape tree
        if (!this.expectedResourceType.toString().equals(resourceType.getValue())) {
            return new ValidationResult(false, this, "Resource type " + resourceType + " is invalid. Expected " + this.expectedResourceType);
        }

        // If a label is specified, check if the proposed name is the same
        if (this.label != null && !this.label.equals(requestedName)) {
            return new ValidationResult(false, this, "Proposed resource name " + requestedName + " is invalid. Expected " + this.label);
        }

        // If the shape tree specifies a shape to validate, perform shape validation
        if (this.shape != null) {
            if (focusNodeUrls == null) { focusNodeUrls = Collections.emptyList(); }
            return this.validateGraph(bodyGraph, focusNodeUrls);
        }

        // Allow if we fall through to here. Focus node is set to null because we only get here if no shape validation was performed
        return new ValidationResult(true, this, this, null);

    }

    public ValidationResult validateGraph(Graph graph, List<URL> focusNodeUrls) throws ShapeTreeException {
        // if (true) return new ValidationResult(true, this, this, focusNodeUrl); // [debug] ShExC parser brings debugger to its knees
        if (this.shape == null) {
            throw new ShapeTreeException(400, "Attempting to validate a shape for ShapeTree " + this.id + "but it doesn't specify one");
        }

        ShexSchema schema;
        if (SchemaCache.isInitialized() && SchemaCache.containsSchema(this.shape)) {
            log.debug("Found cached schema {}", this.shape);
            schema = SchemaCache.getSchema(this.shape);
        } else {
            log.debug("Did not find schema in cache {} will retrieve and parse", this.shape);
            DocumentResponse shexShapeContents = DocumentLoaderManager.getLoader().loadExternalDocument(this.shape);
            if (shexShapeContents == null || shexShapeContents.getBody() == null || shexShapeContents.getBody().isEmpty()) {
                throw new ShapeTreeException(400, "Attempting to validate a ShapeTree (" + this.id + ") - Shape at (" + this.shape + ") is not found or is empty");
            }

            String shapeBody = shexShapeContents.getBody();
            InputStream stream = new ByteArrayInputStream(shapeBody.getBytes(StandardCharsets.UTF_8));
            ShExCParser shexCParser = new ShExCParser();
            try {
                schema = new ShexSchema(GlobalFactory.RDFFactory,shexCParser.getRules(stream),shexCParser.getStart());
                if (SchemaCache.isInitialized()) {
                    SchemaCache.putSchema(this.shape, schema);
                }
            } catch (Exception ex) {
                throw new ShapeTreeException(500, "Error parsing ShEx schema - " + ex.getMessage());
            }
        }

        // Tell ShExJava we want to use Jena as our graph library
        JenaRDF jenaRDF = new org.apache.commons.rdf.jena.JenaRDF();
        GlobalFactory.RDFFactory = jenaRDF;

        ValidationAlgorithm validation = new RecursiveValidation(schema, jenaRDF.asGraph(graph));
        Label shapeLabel = new Label(GlobalFactory.RDFFactory.createIRI(this.shape.toString()));
        ValidationResult validationResult;

        if (!focusNodeUrls.isEmpty()) {  // One or more focus nodes were provided for validation

            for (URL focusNodeUrl : focusNodeUrls) {
                // Evaluate each provided focus node
                IRI focusNode = GlobalFactory.RDFFactory.createIRI(focusNodeUrl.toString());
                log.debug("Validating Shape Label = {}, Focus Node = {}", shapeLabel.toPrettyString(), focusNode.getIRIString());
                validation.validate(focusNode, shapeLabel);
                boolean valid = validation.getTyping().isConformant(focusNode, shapeLabel);
                if (valid) { return new ValidationResult(valid, this, this, focusNodeUrl); }
            }

            // None of the provided focus nodes were valid - this will return the last failure
            return new ValidationResult(false, this, "Failed to validate: " + shapeLabel.toPrettyString());

        } else {  // No focus nodes were provided for validation, so all subject nodes will be evaluated

            List<Node> evaluateNodes = GraphUtil.listSubjects(graph, Node.ANY, Node.ANY).toList();

            for (Node evaluateNode : evaluateNodes) {

                final String focusUriString = evaluateNode.getURI();
                IRI node = GlobalFactory.RDFFactory.createIRI(focusUriString);
                validation.validate(node, shapeLabel);
                boolean valid = validation.getTyping().isConformant(node, shapeLabel);

                if (valid) {
                    final URL matchingFocusNode;
                    try {
                        matchingFocusNode = new URL(focusUriString);
                    } catch (MalformedURLException ex) {
                        throw new ShapeTreeException(500, "Error reporting validation success on malformed URL <" + focusUriString + ">: " + ex.getMessage());
                    }
                    return new ValidationResult(valid, this, this, matchingFocusNode);
                }

            }

            return new ValidationResult(false, this, "Failed to validate: " + shapeLabel.toPrettyString());

        }
    }

    public ValidationResult validateContainedResource(ManageableResource containedResource) throws ShapeTreeException {

        if (this.contains == null || this.contains.isEmpty()) { // TODO: say it can't be null?
            // The contained resource is permitted because this shape tree has no restrictions on what it contains
            return new ValidationResult(true, this, this, null);
        }

        return validateContainedResource(containedResource, Collections.emptyList(), Collections.emptyList());

    }

    public ValidationResult validateContainedResource(ManageableResource containedResource, List<URL> targetShapeTreeUrls, List<URL> focusNodeUrls) throws ShapeTreeException {

        Graph containedResourceGraph = null;

        if (containedResource.getResourceType() != ShapeTreeResourceType.NON_RDF) {
            containedResourceGraph = GraphHelper.readStringIntoGraph(urlToUri(containedResource.getUrl()),
                    containedResource.getBody(),
                    containedResource.getAttributes().firstValue(HttpHeaders.CONTENT_TYPE.getValue()).orElse(null));
        }

        return validateContainedResource(containedResource.getName(), containedResource.getResourceType(), targetShapeTreeUrls, containedResourceGraph, focusNodeUrls);

    }

    public ValidationResult validateContainedResource(String requestedName, ShapeTreeResourceType resourceType, List<URL> targetShapeTreeUrls, Graph bodyGraph, List<URL> focusNodeUrls) throws ShapeTreeException {

        if (this.contains == null || this.contains.isEmpty()) {
            // The contained resource is permitted because this shape tree has no restrictions on what it contains
            return new ValidationResult(true, this, this, null);
        }

        // If one or more target shape trees have been supplied
        if (!targetShapeTreeUrls.isEmpty()) {
            // Test each supplied target shape tree
            for (URL targetShapeTreeUrl : targetShapeTreeUrls) {
                // Check if it exists in st:contains
                if (this.contains.contains(targetShapeTreeUrl)) {
                    ShapeTree targetShapeTree = ShapeTreeFactory.getShapeTree(targetShapeTreeUrl);
                    // Evaluate the shape tree against the attributes of the proposed resources
                    ValidationResult result = targetShapeTree.validateResource(requestedName, resourceType, bodyGraph, focusNodeUrls);
                    if (Boolean.TRUE.equals(result.getValid())) {
                        // Return a successful validation result, including the matching shape tree
                        return new ValidationResult(true, this, targetShapeTree, result.getMatchingFocusNode());
                    }
                }
            }
            // None of the provided target shape trees matched
            return new ValidationResult(false, null, "Failed to validate " + targetShapeTreeUrls);
        } else {
            // For each shape tree in st:contains
            for (URL containsShapeTreeUrl : getPrioritizedContains()) {

                ShapeTree containsShapeTree = ShapeTreeFactory.getShapeTree(containsShapeTreeUrl);
                if (containsShapeTree == null) { continue; } // Continue if the shape tree isn't gettable

                // Evaluate the shape tree against the attributes of the proposed resources
                ValidationResult result = containsShapeTree.validateResource(requestedName, resourceType, bodyGraph, focusNodeUrls);
                // Continue if the proposed attributes were not a match
                if (Boolean.FALSE.equals(result.getValid())) { continue; }
                // Return the successful validation result
                return new ValidationResult(true, this, containsShapeTree, result.getMatchingFocusNode());
            }
        }

        return new ValidationResult(false, null, "Failed to validate shape tree: " + this.id);

    }

    public Iterator<ShapeTreeReference> getReferencedShapeTrees() throws ShapeTreeException {
        return getReferencedShapeTrees(RecursionMethods.DEPTH_FIRST);
    }

    public Iterator<ShapeTreeReference> getReferencedShapeTrees(RecursionMethods recursionMethods) throws ShapeTreeException {
        return getReferencedShapeTreesList(recursionMethods).iterator();
    }

    // Return the list of shape tree contains by priority from most to least strict
    public List<URL> getPrioritizedContains() {

        List<URL> prioritized = new ArrayList<>(this.contains);
        Collections.sort(prioritized, new ShapeTreeContainsPriority());
        return prioritized;

    }

    private List<ShapeTreeReference> getReferencedShapeTreesList(RecursionMethods recursionMethods) throws ShapeTreeException {
        if (recursionMethods.equals(RecursionMethods.BREADTH_FIRST)) {
            return getReferencedShapeTreesListBreadthFirst();
        } else {
            List<ShapeTreeReference> referencedShapeTrees = new ArrayList<>();
            return getReferencedShapeTreesListDepthFirst(this.getReferences(), referencedShapeTrees);
        }
    }

    private List<ShapeTreeReference> getReferencedShapeTreesListBreadthFirst() throws ShapeTreeException {
        List<ShapeTreeReference> referencedShapeTrees = new ArrayList<>();
        Queue<ShapeTreeReference> queue = new LinkedList<>(this.getReferences());

        while (!queue.isEmpty()) {
            ShapeTreeReference currentShapeTree = queue.poll();
            referencedShapeTrees.add(currentShapeTree);
            ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(currentShapeTree.getReferenceUrl());
            if (shapeTree != null) {
                List<ShapeTreeReference> currentReferencedShapeTrees = shapeTree.getReferences();
                if (currentReferencedShapeTrees != null) {
                    queue.addAll(currentReferencedShapeTrees);
                }
            }
        }
        return referencedShapeTrees;
    }

    private List<ShapeTreeReference> getReferencedShapeTreesListDepthFirst(List<ShapeTreeReference> currentReferencedShapeTrees, List<ShapeTreeReference> referencedShapeTrees) throws ShapeTreeException {
        for (ShapeTreeReference currentShapeTreeReference : currentReferencedShapeTrees) {
            referencedShapeTrees.add(currentShapeTreeReference);
            ShapeTree currentReferencedShapeTree = ShapeTreeFactory.getShapeTree(currentShapeTreeReference.getReferenceUrl());
            if (currentReferencedShapeTree != null) {
                referencedShapeTrees = getReferencedShapeTreesListDepthFirst(currentReferencedShapeTree.getReferences(), referencedShapeTrees);
            }
        }
        return referencedShapeTrees;
    }

}

