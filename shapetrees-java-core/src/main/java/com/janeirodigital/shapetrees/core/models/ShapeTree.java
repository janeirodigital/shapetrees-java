package com.janeirodigital.shapetrees.core.models;

import com.janeirodigital.shapetrees.core.SchemaCache;
import com.janeirodigital.shapetrees.core.ShapeTreeFactory;
import com.janeirodigital.shapetrees.core.contentloaders.DocumentContentsLoader;
import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.vocabularies.ShapeTreeVocabulary;
import com.janeirodigital.shapetrees.core.enums.RecursionMethods;
import fr.inria.lille.shexjava.GlobalFactory;
import fr.inria.lille.shexjava.schema.Label;
import fr.inria.lille.shexjava.schema.ShexSchema;
import fr.inria.lille.shexjava.schema.parsing.ShExCParser;
import fr.inria.lille.shexjava.validation.RecursiveValidation;
import fr.inria.lille.shexjava.validation.ValidationAlgorithm;
import lombok.*;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.rdf.api.IRI;
import org.apache.commons.rdf.jena.JenaRDF;
import org.apache.jena.graph.Graph;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.*;

@Getter @Setter
@Slf4j
public class ShapeTree {
    private DocumentContentsLoader documentContentsLoader;
    private String id;
    private String expectedResourceType;
    private String validatedByShapeUri;
    private String label;
    private String supports;
    private List<URI> contains = new ArrayList<>();
    private List<ReferencedShapeTree> references;

    public ShapeTree(DocumentContentsLoader documentContentsLoader) {
        this.documentContentsLoader = documentContentsLoader;
    }

    public URI getURI() throws URISyntaxException {
        return new URI(this.id);
    }

    public ValidationResult validateContent(Graph graph, URI focusNodeURI, ShapeTreeResourceType resourceType) throws IOException, URISyntaxException {
        if (Boolean.TRUE.equals(this.expectsContainer()) != (resourceType == ShapeTreeResourceType.CONTAINER)) {
            throw new ShapeTreeException(400, "The resource type being validated does not match the type expected by the ShapeTree");
        }

        if (this.validatedByShapeUri == null) {
            throw new ShapeTreeException(400, "Attempting to validate a ShapeTree (" + id + ") that does not have an associated Shape");
        }
        URI resolvedShapeURI = URI.create(this.id).resolve(this.validatedByShapeUri);

        URI shapeResourceURI = resolvedShapeURI;
        if (resolvedShapeURI.toString().contains("#")) {
            shapeResourceURI = new URI(shapeResourceURI.toString().substring(0, shapeResourceURI.toString().indexOf("#")));
        }

        ShexSchema schema;
        if (SchemaCache.isInitialized() && SchemaCache.containsSchema(shapeResourceURI)) {
            log.debug("Found cached schema {}", shapeResourceURI);
            schema = SchemaCache.getSchema(shapeResourceURI);
        } else {
            log.debug("Did not find schema in cache {} will retrieve and parse", shapeResourceURI);
            DocumentContents shexShapeContents = documentContentsLoader.loadDocumentContents(shapeResourceURI);
            if (shexShapeContents == null || shexShapeContents.getBody() == null || shexShapeContents.getBody().isEmpty()) {
                throw new ShapeTreeException(400, "Attempting to validate a ShapeTree (" + id + ") - Shape at (" + resolvedShapeURI + ") is not found or is empty");
            }

            String shapeBody = shexShapeContents.getBody();
            InputStream stream = new ByteArrayInputStream(shapeBody.getBytes());
            ShExCParser shexCParser = new ShExCParser();
            try {
                schema = new ShexSchema(GlobalFactory.RDFFactory,shexCParser.getRules(stream),shexCParser.getStart());
                if (SchemaCache.isInitialized()) {
                    SchemaCache.putSchema(shapeResourceURI, schema);
                }
            } catch (Exception ex) {
                throw new ShapeTreeException(500, "Error parsing ShEx schema - " + ex.getMessage());
            }
        }

        // Tell ShExJava we want to use Jena as our graph library
        JenaRDF jenaRDF = new org.apache.commons.rdf.jena.JenaRDF();
        GlobalFactory.RDFFactory = jenaRDF;

        ValidationAlgorithm validation = new RecursiveValidation(schema, jenaRDF.asGraph(graph));
        Label shapeLabel = new Label(GlobalFactory.RDFFactory.createIRI(this.validatedByShapeUri));
        IRI focusNode = GlobalFactory.RDFFactory.createIRI(focusNodeURI.toString());
        log.debug("Validating Shape Label = {}, Focus Node = {}", shapeLabel.toPrettyString(), focusNode.getIRIString());
        validation.validate(focusNode, shapeLabel);

        boolean valid = validation.getTyping().isConformant(focusNode, shapeLabel);
        return new ValidationResult(valid);
    }

    public ShapeTree findMatchingContainsShapeTree(String requestedName, URI targetShapeTreeHint, ShapeTreeResourceType resourceType) throws URISyntaxException, ShapeTreeException {
        if (this.contains == null || this.contains.isEmpty()) {
            if (this.getExpectedResourceType().equals(ShapeTreeVocabulary.SHAPETREE_RESOURCE)) {
                return this;
            } else {
                return null;
            }
        }

        for (URI childShapeTreeURI : this.contains) {
            ShapeTree childShapeTree = ShapeTreeFactory.getShapeTree(childShapeTreeURI);
            if (childShapeTree == null) {
                continue;
            }

            // Allow a target shape tree hint to be passed into matching to skip URITemplate matching
            if (targetShapeTreeHint != null) {
                if (this.contains.contains(targetShapeTreeHint)) {
                    return childShapeTree;
                } else {
                    throw new ShapeTreeException(422, "A target shape tree hint was provided (" + targetShapeTreeHint + ") but it did not exist within :contains");
                }
            }
        }

        // Within this block of code, it means a shape tree hint was not provided
        // At this point it must be decided if an unexpected resource is allowed to be created
        // Default behavior is assumed to be ALLOW_NONE

        // If ALLOW_NONE is explicitly set, reject
        if (this.contains.contains(new URI(ShapeTreeVocabulary.ALLOW_NONE))) {
            throw new ShapeTreeException(422, exceptionMessage(requestedName, this.getId(), "Further, the :AllowNone was specified within :contents"));
        }

        // If none of the other ALLOW_* predicates are present, reject by default
        if (!this.contains.contains(new URI(ShapeTreeVocabulary.ALLOW_ALL)) &&
                !this.contains.contains(new URI(ShapeTreeVocabulary.ALLOW_NON_RDF_SOURCES)) &&
                !this.contains.contains(new URI(ShapeTreeVocabulary.ALLOW_CONTAINERS)) &&
                !this.contains.contains(new URI(ShapeTreeVocabulary.ALLOW_RESOURCES))
        ) {
            throw new ShapeTreeException(422, exceptionMessage(requestedName, this.getId(), "Further, no :Allows* are specified to mitigate"));
        }

        // If it is a non-RDF source and non-RDF sources are not explicitly allowed for...
        if (resourceType == ShapeTreeResourceType.NON_RDF &&
                !this.contains.contains(new URI(ShapeTreeVocabulary.ALLOW_ALL)) &&
                !this.contains.contains(new URI(ShapeTreeVocabulary.ALLOW_NON_RDF_SOURCES))) {
            throw new ShapeTreeException(422, exceptionMessage(requestedName, this.getId(), "Further, the requested resource is a NonRDFSource and :AllowNonRDFSources was not specified within :contents"));
        }
        // if it is a Container source and Container sources are not explicitly allowed for...
        if (resourceType == ShapeTreeResourceType.CONTAINER &&
                !this.contains.contains(new URI(ShapeTreeVocabulary.ALLOW_ALL)) &&
                !this.contains.contains(new URI(ShapeTreeVocabulary.ALLOW_CONTAINERS))) {
            throw new ShapeTreeException(422, exceptionMessage(requestedName, this.getId(),"Further, the requested resource is a Container and :AllowContainers was not specified within :contents"));
        }
        //
        if (resourceType == ShapeTreeResourceType.RESOURCE &&
                !this.contains.contains(new URI(ShapeTreeVocabulary.ALLOW_ALL)) &&
                !this.contains.contains(new URI(ShapeTreeVocabulary.ALLOW_RESOURCES))) {
            throw new ShapeTreeException(422, exceptionMessage(requestedName, this.getId(), "Further, the requested resource is a Resource and :AllowResources was not specified within :contents"));
        }
        // If we return null, it will indicate there is nothing to validate against, and that's okay
        // because we've already validated if the type of incoming Resource is allowed in the absence of a
        // URI template match
        return null;
    }

    public Iterator<ReferencedShapeTree> getReferencedShapeTrees() throws URISyntaxException, ShapeTreeException {
        return getReferencedShapeTrees(RecursionMethods.DEPTH_FIRST);
    }

    public Iterator<ReferencedShapeTree> getReferencedShapeTrees(RecursionMethods recursionMethods) throws URISyntaxException, ShapeTreeException {
        return getReferencedShapeTreesList(recursionMethods).iterator();
    }

    private Boolean expectsContainer() {
        return this.getExpectedResourceType() != null && this.getExpectedResourceType().equals(ShapeTreeVocabulary.SHAPETREE_CONTAINER);
    }

    private List<ReferencedShapeTree> getReferencedShapeTreesList(RecursionMethods recursionMethods) throws URISyntaxException, ShapeTreeException {
        if (recursionMethods.equals(RecursionMethods.BREADTH_FIRST)) {
            return getReferencedShapeTreesListBreadthFirst();
        } else {
            List<ReferencedShapeTree> referencedShapeTrees = new ArrayList<>();
            return getReferencedShapeTreesListDepthFirst(this.getReferences(), referencedShapeTrees);
        }
    }

    private List<ReferencedShapeTree> getReferencedShapeTreesListBreadthFirst() throws URISyntaxException, ShapeTreeException {
        List<ReferencedShapeTree> referencedShapeTrees = new ArrayList<>();
        Queue<ReferencedShapeTree> queue = new LinkedList<>(this.getReferences());

        while (!queue.isEmpty()) {
            ReferencedShapeTree currentShapeTree = queue.poll();
            referencedShapeTrees.add(currentShapeTree);
            ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(currentShapeTree.getReferencedShapeTreeURI());
            if (shapeTree != null) {
                List<ReferencedShapeTree> currentReferencedShapeTrees = shapeTree.getReferences();
                if (currentReferencedShapeTrees != null) {
                    queue.addAll(currentReferencedShapeTrees);
                }
            }
        }
        return referencedShapeTrees;
    }

    private List<ReferencedShapeTree> getReferencedShapeTreesListDepthFirst(List<ReferencedShapeTree> currentReferencedShapeTrees, List<ReferencedShapeTree> referencedShapeTrees) throws URISyntaxException, ShapeTreeException {
        for (ReferencedShapeTree currentShapeTreeReference : currentReferencedShapeTrees) {
            referencedShapeTrees.add(currentShapeTreeReference);
            ShapeTree currentReferencedShapeTree = ShapeTreeFactory.getShapeTree(currentShapeTreeReference.getReferencedShapeTreeURI());
            if (currentReferencedShapeTree != null) {
                referencedShapeTrees = getReferencedShapeTreesListDepthFirst(currentReferencedShapeTree.getReferences(), referencedShapeTrees);
            }
        }
        return referencedShapeTrees;
    }

    private String exceptionMessage(String requestedName, String id, String customMessage){
        return  "Failed to match ["+ requestedName +"] against any :contents for [" + id +"]. " + customMessage;
    }
}
