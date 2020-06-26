package com.janeirodigital.shapetrees.model;

import com.janeirodigital.shapetrees.RemoteResource;
import fr.inria.lille.shexjava.GlobalFactory;
import fr.inria.lille.shexjava.schema.Label;
import fr.inria.lille.shexjava.schema.ShexSchema;
import fr.inria.lille.shexjava.schema.parsing.GenParser;
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
import org.apache.jena.rdf.model.ModelFactory;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

@Getter @Setter @NoArgsConstructor @AllArgsConstructor
public class ShapeTreeStep {
    private String id;
    private String rdfResourceType;
    private String shapeUri;
    private String label;
    private String uriTemplate;
    private List<URI> contents;
    private List<ReferencedShapeTreeStep> references;

    public URI getURI() throws URISyntaxException {
        return new URI(this.id);
    }

    public Boolean isContainer() {
        if (this.getRdfResourceType() != null && this.getRdfResourceType().equals("ldp:Container")) {
            return true;
        }
        return false;
    }

    @SneakyThrows
    public ValidationResult validateContent(String authorizationHeaderValue, Graph graph, URI focusNodeURI) {
        if (this.shapeUri == null) {
            throw new Exception("Attempting to validate a ShapeTreeStep (" + id + ") that does not have an associated Shape");
        }

        RemoteResource shexShapeSchema = new RemoteResource(this.shapeUri, authorizationHeaderValue);
        if (!shexShapeSchema.exists() || shexShapeSchema.getBody() == null) {
            throw new Exception("Attempting to validate a ShapeTreeStep (" + id + ") - Shape at (" + this.shapeUri + ") is not found or is emptys");
        }

        // Tell ShExJava we want to use Jena as our graph library
        JenaRDF jenaRDF = new org.apache.commons.rdf.jena.JenaRDF();
        GlobalFactory.RDFFactory = jenaRDF;
        String shapeBody = shexShapeSchema.getBody();
        InputStream stream = new ByteArrayInputStream(shapeBody.getBytes());
        ShExCParser shexCParser = new ShExCParser();
        ShexSchema schema = new ShexSchema(GlobalFactory.RDFFactory,shexCParser.getRules(stream),shexCParser.getStart());

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
}
