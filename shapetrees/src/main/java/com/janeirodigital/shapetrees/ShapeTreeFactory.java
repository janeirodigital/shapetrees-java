package com.janeirodigital.shapetrees;

import com.janeirodigital.shapetrees.model.ReferencedShapeTreeStep;
import com.janeirodigital.shapetrees.model.ShapeTreeStep;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.apache.jena.graph.Node;
import org.apache.jena.graph.Node_URI;
import org.apache.jena.rdf.model.*;
import org.apache.jena.riot.RDFDataMgr;

import java.net.URI;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Slf4j
public class ShapeTreeFactory {
    private static final String EXPECTED_TYPE = "http://www.w3.org/ns/shapetree#expectedType";
    private static final String REFERENCES = "http://www.w3.org/ns/shapetree#references";
    private static final String CONTENTS = "http://www.w3.org/ns/shapetree#contents";
    private static final String TREE_STEP = "http://www.w3.org/ns/shapetree#treeStep";
    private static final String SHAPE_PATH = "http://www.w3.org/ns/shapetree#shapePath";
    private static final String SHAPE = "http://www.w3.org/ns/shapetree#shape";
    private static final String URI_TEMPLATE = "http://www.w3.org/ns/shapetree#uriTemplate";
    private static final String RDFS_LABEL = "http://www.w3.org/2000/01/rdf-schema#label";

    private static final Map<URI, ShapeTreeStep> localShapeTreeCache = new HashMap<>();

    public static ShapeTreeStep getShapeTreeStep(URI shapeTreeStepURI) throws Exception {
        if (localShapeTreeCache.containsKey(shapeTreeStepURI)) {
            log.info("[{}] previously cached -- returning", shapeTreeStepURI.toString());
            return localShapeTreeCache.get(shapeTreeStepURI);
        }

        dereferenceAndParseShapeTreeResource(shapeTreeStepURI);

        return localShapeTreeCache.get(shapeTreeStepURI);
    }

    private static void dereferenceAndParseShapeTreeResource(URI shapeTreeURI) throws Exception {
        Model model = RDFDataMgr.loadModel(shapeTreeURI.toString());

        Resource resource = model.getResource(shapeTreeURI.toString());
        if (resource != null) {
            recursivelyParseShapeTreeStep(model, resource);
        }
    }

    private static void recursivelyParseShapeTreeStep(Model model, Resource resource) throws Exception{
        String stepURIString = resource.getURI();
        log.info("Entering recursivelyParseShapeTreeStep for [{}]", stepURIString);
        URI stepURI = new URI(stepURIString);

        if (localShapeTreeCache.containsKey(stepURI)) {
            log.info("[{}] previously cached -- returning", stepURIString);
            return;
        }

        ShapeTreeStep step = new ShapeTreeStep();
        // Set the URI as the ID (string representation)
        step.setId(stepURIString);
        // Set the expected resource type
        step.setRdfResourceType(getStringValue(model, resource, EXPECTED_TYPE));
        // Set URI Template
        step.setUriTemplate(getStringValue(model, resource, URI_TEMPLATE));
        // Set Shape URI
        step.setShapeUri(getStringValue(model, resource, SHAPE));
        // Set Label
        step.setLabel(getStringValue(model, resource, RDFS_LABEL));
        // Set Reference collection
        step.setReferences(new ArrayList<>());

        // Add the step to the cache before any of the recursive processing
        localShapeTreeCache.put(stepURI, step);

        Property referencesProperty = model.createProperty(REFERENCES);
        if (resource.hasProperty(referencesProperty)) {
            List<Statement> referenceStatements = resource.listProperties(referencesProperty).toList();
            for (Statement referenceStatement : referenceStatements) {

                Resource referenceResource = referenceStatement.getObject().asResource();
                URI referenceStepUri = new URI(getStringValue(model, referenceResource, TREE_STEP));
                String shapePath = getStringValue(model, referenceResource, SHAPE_PATH);
                if (!localShapeTreeCache.containsKey(referenceStepUri)) {
                    // If the model contains the referenced ShapeTree Step, go ahead and parse and cache it
                    if (model.getResource(referenceStepUri.toString()) != null) {
                        recursivelyParseShapeTreeStep(model, model.getResource(referenceStepUri.toString()));
                    }
                }

                ReferencedShapeTreeStep referencedStep = new ReferencedShapeTreeStep(referenceStepUri, shapePath);
                step.getReferences().add(referencedStep);
            }
        }

        // Containers are expected to have contents
        if (resource.hasProperty(model.createProperty(CONTENTS)) && !step.getRdfResourceType().contains("#Container")) {
            throw new Exception("Contents predicate not expected outside of #Container RDF Types");
        }
        if (step.getRdfResourceType().contains("#Container")) {
            List<URI> uris = getURLListValue(model, resource, CONTENTS);
            step.setContents(uris);
            if (uris != null) {
                for (URI uri : uris) {
                    if (!localShapeTreeCache.containsKey(uri)) {
                        recursivelyParseShapeTreeStep(model, model.getResource(uri.toString()));
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
                System.out.println("Need to figure this out!");
            }

        }
        return null;
    }

    @SneakyThrows
    private static List<URI> getURLListValue(Model model, Resource resource, String predicate) {
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
            return uris;
        }
        return null;
    }
}
