package com.janeirodigital.shapetrees.model;

import com.janeirodigital.shapetrees.RemoteResource;
import com.janeirodigital.shapetrees.ShapeTreeException;
import com.janeirodigital.shapetrees.ShapeTreeFactory;
import com.janeirodigital.shapetrees.ShapeTreeVocabulary;
import com.janeirodigital.shapetrees.enums.RecursionMethod;
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
    private String rdfResourceType;
    private String shapeUri;
    private String label;
    private String uriTemplate;
    private List<URI> contents = new ArrayList<>();
    private List<ReferencedShapeTree> references;

    public URI getURI() throws URISyntaxException {
        return new URI(this.id);
    }

    public Boolean isContainer() {
        return this.getRdfResourceType() != null && this.getRdfResourceType().contains("Container");
    }

    public ValidationResult validateContent(String authorizationHeaderValue, Graph graph, URI focusNodeURI, Boolean isAContainer) throws IOException {
        if (this.isContainer() != isAContainer) {
            throw new ShapeTreeException(400, "The resource type being validated does not match the type expected by the ShapeTree");
        }

        if (this.shapeUri == null) {
            throw new ShapeTreeException(400, "Attempting to validate a ShapeTree (" + id + ") that does not have an associated Shape");
        }
        URI resolvedShapeURI = URI.create(this.id).resolve(this.shapeUri);
        RemoteResource shexShapeSchema = new RemoteResource(resolvedShapeURI, authorizationHeaderValue);
        if (!shexShapeSchema.exists() || shexShapeSchema.getBody() == null) {
            throw new ShapeTreeException(400, "Attempting to validate a ShapeTree (" + id + ") - Shape at (" + this.shapeUri + ") is not found or is empty");
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
        Label shapeLabel = new Label(GlobalFactory.RDFFactory.createIRI(this.shapeUri));
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
        if (this.contents == null || this.contents.size() == 0) {
            if (this.getRdfResourceType().contains("Resource")) {
                return this;
            } else {
                return null;
            }
        }

        List<ShapeTree> matchingShapeTrees = new ArrayList<>();

        for (URI childShapeTreeURI : this.contents) {
            ShapeTree childShapeTree = ShapeTreeFactory.getShapeTree(childShapeTreeURI);
            UriTemplate template = new UriTemplate(childShapeTree.getUriTemplate());
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
            // This means nothing matched according to the URI template.  We must decide what should be allowed.
            // If it is a non-RDF source and non-RDF sources are not explicitly allowed for...
            if (isNonRdfSource &&
                    !this.contents.contains(ShapeTreeVocabulary.ALLOW_ALL) &&
                    !this.contents.contains(ShapeTreeVocabulary.ALLOW_NON_RDF_SOURCES)) {
                throw new ShapeTreeException(422, "Failed to match ["+ requestedName +"] against any tree:contents for [" + this.getId() +"].  Further, the requested resource is a NonRDFSource and tree:AllowNonRDFSources was not specified within tree:contents");
            }
            // if it is a Container source and Container sources are not explicitly allowed for...
            if (isContainer &&
                    !this.contents.contains(ShapeTreeVocabulary.ALLOW_ALL) &&
                    !this.contents.contains(ShapeTreeVocabulary.ALLOW_CONTAINERS)) {
                throw new ShapeTreeException(422, "Failed to match ["+ requestedName +"] against any tree:contents for [" + this.getId() +"].  Further, the requested resource is a Container and tree:AllowContainers was not specified within tree:contents");
            }
            //
            if (!isContainer &&
                    !isNonRdfSource &&
                    !this.contents.contains(ShapeTreeVocabulary.ALLOW_ALL) &&
                    !this.contents.contains(ShapeTreeVocabulary.ALLOW_RESOURCES)) {
                throw new ShapeTreeException(422, "Failed to match ["+ requestedName +"] against any tree:contents for [" + this.getId() +"].  Further, the requested resource is a Resource and tree:AllowResources was not specified within tree:contents");
            }
            // If we return null, it will indicate there is nothing to validate against, and that's okay
            // because we've already validated if the type of incoming Resource is allowed in the absence of a
            // URI template match
            return null;
        }
    }

    public Iterator<ReferencedShapeTree> getReferencedShapeTrees() throws URISyntaxException, ShapeTreeException {
        return getReferencedShapeTrees(RecursionMethod.DEPTH_FIRST);
    }

    public Iterator<ReferencedShapeTree> getReferencedShapeTrees(RecursionMethod recursionMethod) throws URISyntaxException, ShapeTreeException {
        return getReferencedShapeTreesList(recursionMethod).iterator();
    }

    private List<ReferencedShapeTree> getReferencedShapeTreesList(RecursionMethod recursionMethod) throws URISyntaxException, ShapeTreeException {
        if (recursionMethod.equals(RecursionMethod.BREADTH_FIRST)) {
            return getReferencedShapeTreesListBreadthFirst();
        } else {
            List<ReferencedShapeTree> referencedShapeTrees = new ArrayList<>();
            return getReferencedShapeTreesListDepthFirst(this.getReferences(), referencedShapeTrees);
        }
    }

    private List<ReferencedShapeTree> getReferencedShapeTreesListBreadthFirst() throws URISyntaxException, ShapeTreeException {
        List<ReferencedShapeTree> referencedShapeTrees = new ArrayList<>();
        Queue<ReferencedShapeTree> queue = new LinkedList<>();
        queue.addAll(this.getReferences());

        while (!queue.isEmpty()) {
            ReferencedShapeTree currentShapeTree = queue.poll();
            referencedShapeTrees.add(currentShapeTree);
            ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(currentShapeTree.getReferencedShapeTree());
            List<ReferencedShapeTree> currentReferencedShapeTrees = shapeTree.getReferences();
            if (currentReferencedShapeTrees != null) {
                for (ReferencedShapeTree currentReferencedShapeTree : currentReferencedShapeTrees) {
                    queue.add(currentReferencedShapeTree);
                }
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
