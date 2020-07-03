package com.janeirodigital.shapetrees.model;

import com.janeirodigital.shapetrees.RemoteResource;
import com.janeirodigital.shapetrees.ShapeTreeException;
import com.janeirodigital.shapetrees.ShapeTreeFactory;
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
import java.lang.ref.Reference;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.*;
import java.util.stream.Collectors;

@Getter @Setter @NoArgsConstructor @AllArgsConstructor
public class ShapeTreeStep {
    private String id;
    private String rdfResourceType;
    private String shapeUri;
    private String label;
    private String uriTemplate;
    private List<URI> contents = new ArrayList<>();
    private List<ReferencedShapeTreeStep> references;

    public URI getURI() throws URISyntaxException {
        return new URI(this.id);
    }

    public Boolean isContainer() {
        return this.getRdfResourceType() != null && this.getRdfResourceType().contains("Container");
    }

    public ValidationResult validateContent(String authorizationHeaderValue, Graph graph, URI focusNodeURI, Boolean isAContainer) throws IOException {
        if (this.isContainer() != isAContainer) {
            throw new ShapeTreeException(400, "The resource type being validated does not match the type expected by the ShapeTreeStep");
        }

        if (this.shapeUri == null) {
            throw new ShapeTreeException(400, "Attempting to validate a ShapeTreeStep (" + id + ") that does not have an associated Shape");
        }
        URI resolvedShapeURI = URI.create(this.id).resolve(this.shapeUri);
        RemoteResource shexShapeSchema = new RemoteResource(resolvedShapeURI, authorizationHeaderValue);
        if (!shexShapeSchema.exists() || shexShapeSchema.getBody() == null) {
            throw new ShapeTreeException(400, "Attempting to validate a ShapeTreeStep (" + id + ") - Shape at (" + this.shapeUri + ") is not found or is empty");
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

    public ShapeTreeStep findMatchingContainsShapeTreeStep(String requestedName) throws URISyntaxException, ShapeTreeException {
        if (this.contents == null || this.contents.size() == 0) {
            // TODO - this needs to be talked about!!
            // TODO - Effectively, if there are no contents and the current shapetree is a resource, use its own
            // TODO - shape as validation
            if (this.getRdfResourceType().contains("Resource")) {
                return this;
            } else {
                return null;
            }
        }

        List<ShapeTreeStep> matchingSteps = new ArrayList<>();

        for (URI childStepURI : this.contents) {
            ShapeTreeStep childStep = ShapeTreeFactory.getShapeTreeStep(childStepURI);
            UriTemplate template = new UriTemplate(childStep.getUriTemplate());
            if (requestedName.endsWith("/")) {
                requestedName = requestedName.replace("/", "");
            }
            boolean matches = template.match(requestedName, new ArrayList<>());
            if (matches) {
                matchingSteps.add(childStep);
            }
        }

        if (matchingSteps.size() == 0) {
            return null;
        } else if (matchingSteps.size() > 1) {
            throw new ShapeTreeException(400, "Multiple ShapeTree steps matched the incoming slug.  This likely indicates an issue with the ShapeTree definition.  Matched " + matchingSteps.stream().map(ShapeTreeStep::getId).collect(Collectors.joining(", ")));
        } else {
            return matchingSteps.get(0);
        }
    }

    public Iterator<ReferencedShapeTreeStep> getReferencedSteps() throws URISyntaxException, ShapeTreeException {
        return getReferencedSteps(RecursionMethod.DEPTH_FIRST);
    }

    public Iterator<ReferencedShapeTreeStep> getReferencedSteps(RecursionMethod recursionMethod) throws URISyntaxException, ShapeTreeException {
        return getReferencedStepsList(recursionMethod).iterator();
    }

    private List<ReferencedShapeTreeStep> getReferencedStepsList(RecursionMethod recursionMethod) throws URISyntaxException, ShapeTreeException {
        if (recursionMethod.equals(RecursionMethod.BREADTH_FIRST)) {
            return getReferencedStepsListBreadthFirst();
        } else {
            List<ReferencedShapeTreeStep> referencedSteps = new ArrayList<>();
            return getReferencedStepsListDepthFirst(this.getReferences(), referencedSteps);
        }
    }

    private List<ReferencedShapeTreeStep> getReferencedStepsListBreadthFirst() throws URISyntaxException, ShapeTreeException {
        List<ReferencedShapeTreeStep> referencedSteps = new ArrayList<>();
        Queue<ReferencedShapeTreeStep> queue = new LinkedList<>();
        queue.addAll(this.getReferences());

        while (!queue.isEmpty()) {
            ReferencedShapeTreeStep currentStep = queue.poll();
            referencedSteps.add(currentStep);
            ShapeTreeStep step = ShapeTreeFactory.getShapeTreeStep(currentStep.getReferencedStep());
            List<ReferencedShapeTreeStep> currentReferencedSteps = step.getReferences();
            if (currentReferencedSteps != null) {
                for (ReferencedShapeTreeStep currentReferencedStep : currentReferencedSteps) {
                    queue.add(currentReferencedStep);
                }
            }
        }
        return referencedSteps;
    }

    private List<ReferencedShapeTreeStep> getReferencedStepsListDepthFirst(List<ReferencedShapeTreeStep> currentReferencedSteps, List<ReferencedShapeTreeStep> referencedSteps) throws URISyntaxException, ShapeTreeException {
        for (ReferencedShapeTreeStep currentStepReference : currentReferencedSteps) {
            referencedSteps.add(currentStepReference);
            ShapeTreeStep currentReferencedStep = ShapeTreeFactory.getShapeTreeStep(currentStepReference.getReferencedStep());
            referencedSteps = getReferencedStepsListDepthFirst(currentReferencedStep.getReferences(), referencedSteps);
        }
        return referencedSteps;
    }
}
