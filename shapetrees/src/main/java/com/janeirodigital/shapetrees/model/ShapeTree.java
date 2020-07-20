package com.janeirodigital.shapetrees.model;

import com.janeirodigital.shapetrees.RemoteResource;
import com.janeirodigital.shapetrees.ShapeTreeException;
import com.janeirodigital.shapetrees.ShapeTreeFactory;
import com.janeirodigital.shapetrees.vocabulary.ShapeTreeVocabulary;
import com.janeirodigital.shapetrees.enums.RecursionMethods;
import com.sun.jersey.api.uri.UriTemplate;
import fr.inria.lille.shexjava.GlobalFactory;
import fr.inria.lille.shexjava.schema.Label;
import fr.inria.lille.shexjava.schema.ShexSchema;
import fr.inria.lille.shexjava.schema.parsing.ShExCParser;
import fr.inria.lille.shexjava.util.Pair;
import fr.inria.lille.shexjava.validation.RecursiveValidation;
import fr.inria.lille.shexjava.validation.Status;
import fr.inria.lille.shexjava.validation.ValidationAlgorithm;
import lombok.*;
import org.apache.commons.rdf.api.IRI;
import org.apache.commons.rdf.api.RDFTerm;
import org.apache.commons.rdf.jena.JenaRDF;
import org.apache.jena.graph.Graph;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.*;
import java.util.stream.Collectors;

@Getter @Setter @NoArgsConstructor @AllArgsConstructor
public class ShapeTree {
    private String id;
    private String expectedResourceType;
    private String validatedByShapeUri;
    private String label;
    private String matchesUriTemplate;
    private String supports;
    private List<URI> contains = new ArrayList<>();
    private List<ReferencedShapeTree> references;

    public URI getURI() throws URISyntaxException {
        return new URI(this.id);
    }

    public Boolean isContainer() {
        return this.getExpectedResourceType() != null && this.getExpectedResourceType().equals(ShapeTreeVocabulary.SHAPETREE_CONTAINER);
    }

    public ValidationResult validateContent(String authorizationHeaderValue, Graph graph, URI focusNodeURI, Boolean isAContainer) throws IOException {
        if (this.isContainer() != isAContainer) {
            throw new ShapeTreeException(400, "The resource type being validated does not match the type expected by the ShapeTree");
        }

        if (this.validatedByShapeUri == null) {
            throw new ShapeTreeException(400, "Attempting to validate a ShapeTree (" + id + ") that does not have an associated Shape");
        }
        URI resolvedShapeURI = URI.create(this.id).resolve(this.validatedByShapeUri);
        RemoteResource shexShapeSchema = new RemoteResource(resolvedShapeURI, authorizationHeaderValue);
        if (!shexShapeSchema.exists() || shexShapeSchema.getBody() == null) {
            throw new ShapeTreeException(400, "Attempting to validate a ShapeTree (" + id + ") - Shape at (" + this.validatedByShapeUri + ") is not found or is empty");
        }

        // Tell ShExJava we want to use Jena as our graph library
        JenaRDF jenaRDF = new org.apache.commons.rdf.jena.JenaRDF();
        GlobalFactory.RDFFactory = jenaRDF;
        String shapeBody = shexShapeSchema.getBody();
        InputStream stream = new ByteArrayInputStream(shapeBody.getBytes());
        ShExCParser shexCParser = new ShExCParser();
        ShexSchema schema;
        try {
            schema = new ShexSchema(GlobalFactory.RDFFactory,shexCParser.getRules(stream),shexCParser.getStart());
        } catch (Exception ex) {
            throw new ShapeTreeException(500, "Error parsing ShEx schema - " + ex.getMessage());
        }

        ValidationAlgorithm validation = new RecursiveValidation(schema, jenaRDF.asGraph(graph));
        Label shapeLabel = new Label(GlobalFactory.RDFFactory.createIRI(this.validatedByShapeUri));
        IRI focusNode = GlobalFactory.RDFFactory.createIRI(focusNodeURI.toString());
        validation.validate(focusNode, shapeLabel);

        boolean valid = validation.getTyping().isConformant(focusNode, shapeLabel);
        List<String> failedNodes = new ArrayList<>();
        if (!valid) {
            for (Pair<RDFTerm, Label> entry :  validation.getTyping().getStatusMap().keySet()) {
                if (validation.getTyping().getStatusMap().get(entry).equals(Status.NONCONFORMANT)) {
                    failedNodes.add(entry.one + " - " + entry.two);
                }
            }
        }

        return new ValidationResult(valid, failedNodes);
    }

    public ShapeTree findMatchingContainsShapeTree(String requestedName, Boolean isContainer, Boolean isNonRdfSource) throws URISyntaxException, ShapeTreeException {
        if (this.contains == null || this.contains.size() == 0) {
            if (this.getExpectedResourceType().equals(ShapeTreeVocabulary.SHAPETREE_RESOURCE)) {
                return this;
            } else {
                return null;
            }
        }

        List<ShapeTree> matchingShapeTrees = new ArrayList<>();

        for (URI childShapeTreeURI : this.contains) {
            ShapeTree childShapeTree = ShapeTreeFactory.getShapeTree(childShapeTreeURI);
            if (childShapeTree == null) {
                continue;
            }
            UriTemplate template = new UriTemplate(childShapeTree.getMatchesUriTemplate());
            if (requestedName.endsWith("/")) {
                requestedName = requestedName.replace("/", "");
            }
            boolean matches = template.match(requestedName, new ArrayList<>());
            if (matches) {
                matchingShapeTrees.add(childShapeTree);
            }
        }

        if (matchingShapeTrees.size() > 1) {
            throw new ShapeTreeException(400, "Multiple ShapeTree matched the incoming slug.  This likely indicates an issue with the ShapeTree definition.  Matched " + matchingShapeTrees.stream().map(ShapeTree::getId).collect(Collectors.joining(", ")));
        }
        else if (matchingShapeTrees.size() == 1) {
            return matchingShapeTrees.get(0);
        }
        else {
            // Within this block of code, it means nothing matched based on the URI template.
            // At this point it must be decided if an unexpected resource is allowed to be created
            // Default behavior is assumed to be ALLOW_NONE

            // If ALLOW_NONE is explicitly set, reject
            if (this.contains.contains(new URI(ShapeTreeVocabulary.ALLOW_NONE))) {
                throw new ShapeTreeException(422, "Failed to match ["+ requestedName +"] against any tree:contains for [" + this.getId() +"].  Further, the tree:AllowNone was specified within tree:contents");
            }

            // If none of the other ALLOW_* predicates are present, reject by default
            if (!this.contains.contains(new URI(ShapeTreeVocabulary.ALLOW_ALL)) &&
                    !this.contains.contains(new URI(ShapeTreeVocabulary.ALLOW_NON_RDF_SOURCES)) &&
                    !this.contains.contains(new URI(ShapeTreeVocabulary.ALLOW_CONTAINERS)) &&
                    !this.contains.contains(new URI(ShapeTreeVocabulary.ALLOW_RESOURCES))
            ) {
                throw new ShapeTreeException(422, "Failed to match ["+ requestedName +"] against any tree:contains for [" + this.getId() +"].  Further, no tree:Allows* are specified to mitigate");
            }

            // If it is a non-RDF source and non-RDF sources are not explicitly allowed for...
            if (isNonRdfSource &&
                    !this.contains.contains(new URI(ShapeTreeVocabulary.ALLOW_ALL)) &&
                    !this.contains.contains(new URI(ShapeTreeVocabulary.ALLOW_NON_RDF_SOURCES))) {
                throw new ShapeTreeException(422, "Failed to match ["+ requestedName +"] against any tree:contains for [" + this.getId() +"].  Further, the requested resource is a NonRDFSource and tree:AllowNonRDFSources was not specified within tree:contents");
            }
            // if it is a Container source and Container sources are not explicitly allowed for...
            if (isContainer &&
                    !this.contains.contains(new URI(ShapeTreeVocabulary.ALLOW_ALL)) &&
                    !this.contains.contains(new URI(ShapeTreeVocabulary.ALLOW_CONTAINERS))) {
                throw new ShapeTreeException(422, "Failed to match ["+ requestedName +"] against any tree:contains for [" + this.getId() +"].  Further, the requested resource is a Container and tree:AllowContainers was not specified within tree:contents");
            }
            //
            if (!isContainer &&
                    !isNonRdfSource &&
                    !this.contains.contains(new URI(ShapeTreeVocabulary.ALLOW_ALL)) &&
                    !this.contains.contains(new URI(ShapeTreeVocabulary.ALLOW_RESOURCES))) {
                throw new ShapeTreeException(422, "Failed to match ["+ requestedName +"] against any tree:contents for [" + this.getId() +"].  Further, the requested resource is a Resource and tree:AllowResources was not specified within tree:contents");
            }
            // If we return null, it will indicate there is nothing to validate against, and that's okay
            // because we've already validated if the type of incoming Resource is allowed in the absence of a
            // URI template match
            return null;
        }
    }

    public Iterator<ReferencedShapeTree> getReferencedShapeTrees() throws URISyntaxException, ShapeTreeException {
        return getReferencedShapeTrees(RecursionMethods.DEPTH_FIRST);
    }

    public Iterator<ReferencedShapeTree> getReferencedShapeTrees(RecursionMethods recursionMethods) throws URISyntaxException, ShapeTreeException {
        return getReferencedShapeTreesList(recursionMethods).iterator();
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
            ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(currentShapeTree.getReferencedShapeTree());
            List<ReferencedShapeTree> currentReferencedShapeTrees = shapeTree.getReferences();
            if (currentReferencedShapeTrees != null) {
                queue.addAll(currentReferencedShapeTrees);
            }
        }
        return referencedShapeTrees;
    }

    private List<ReferencedShapeTree> getReferencedShapeTreesListDepthFirst(List<ReferencedShapeTree> currentReferencedShapeTrees, List<ReferencedShapeTree> referencedShapeTrees) throws URISyntaxException, ShapeTreeException {
        for (ReferencedShapeTree currentShapeTreeReference : currentReferencedShapeTrees) {
            referencedShapeTrees.add(currentShapeTreeReference);
            ShapeTree currentReferencedShapeTree = ShapeTreeFactory.getShapeTree(currentShapeTreeReference.getReferencedShapeTree());
            referencedShapeTrees = getReferencedShapeTreesListDepthFirst(currentReferencedShapeTree.getReferences(), referencedShapeTrees);
        }
        return referencedShapeTrees;
    }
}
