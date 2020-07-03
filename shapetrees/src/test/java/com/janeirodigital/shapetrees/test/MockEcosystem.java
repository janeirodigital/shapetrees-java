package com.janeirodigital.shapetrees.test;

import com.janeirodigital.shapetrees.RemoteResource;
import com.janeirodigital.shapetrees.ShapeTreeEcosystem;
import com.janeirodigital.shapetrees.ShapeTreeException;
import com.janeirodigital.shapetrees.ShapeTreeFactory;
import com.janeirodigital.shapetrees.model.ShapeTreeContext;
import com.janeirodigital.shapetrees.model.ShapeTreePlantResult;
import com.janeirodigital.shapetrees.model.ShapeTreeStep;
import com.janeirodigital.shapetrees.model.ValidationResult;
import lombok.extern.slf4j.Slf4j;
import org.apache.jena.atlas.lib.DateTimeUtils;
import org.apache.jena.datatypes.xsd.XSDDatatype;
import org.apache.jena.graph.Graph;
import org.apache.jena.graph.GraphUtil;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Triple;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.vocabulary.RDF;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;

@Slf4j
public class MockEcosystem implements ShapeTreeEcosystem {

    private static final String PREDICATE_ECOSYSTEM_HAS_REGISTRAR = "http://www.w3.org/ns/solid/ecosystem#hasRegistrar";
    private static final String PREDICATE_ECOSYSTEM_HAS_DATA_REGISTRY_SET = "http://www.w3.org/ns/solid/ecosystem#hasDataRegistrySet";
    private static final String PREDICATE_ECOSYSTEM_HAS_REGISTRY = "http://www.w3.org/ns/solid/ecosystem#hasRegistry";
    private static final String PREDICATE_ECOSYSTEM_REGISTERED_BY = "http://www.w3.org/ns/solid/ecosystem#registeredBy";
    private static final String PREDICATE_ECOSYSTEM_REGISTERED_WITH = "http://www.w3.org/ns/solid/ecosystem#registeredWith";
    private static final String PREDICATE_ECOSYSTEM_REGISTERED_AT = "http://www.w3.org/ns/solid/ecosystem#registeredAt";
    private static final String PREDICATE_ECOSYSTEM_REGISTERED_SHAPE_TREE = "http://www.w3.org/ns/solid/ecosystem#registeredShapeTree";
    private static final String PREDICATE_ECOSYSTEM_HAS_REGISTERED_DATA_INSTANCE = "http://www.w3.org/ns/solid/ecosystem#hasRegisteredDataInstance";
    private static final String PREDICATE_ECOSYSTEM_HAS_REGISTRATION = "http://www.w3.org/ns/solid/ecosystem#hasRegistration";
    private static final String SHAPE_TREE_DATA_REGISTRATION = "http://localhost:9999/static/ecosystem/ecosystem-ShapeTree.ttl#data-registration-tree";
    private static final String ECO_DATA_REGISTRATION = "http://www.w3.org/ns/solid/ecosystem#DataRegistration";

    @Override
    public void initializeEcosystem() {

    }

    @Override
    public ShapeTreePlantResult getExistingShapeTreeFromContainer(URI parentContainer, URI shapeTreeURI) {
        return new ShapeTreePlantResult();
    }

    @Override
    public void indexShapeTree(ShapeTreeContext context, URI parentContainer, URI shapeTreeURI, URI plantedShapeTreeURI) throws IOException, URISyntaxException {
        // plantedShapeTreeURI is the container we want to update with the #registration subject
        RemoteResource remoteResource = new RemoteResource(plantedShapeTreeURI, context.getAuthorizationHeaderValue());
        ShapeTreeStep dataRegistrationShapeTree = ShapeTreeFactory.getShapeTreeStep(new URI(SHAPE_TREE_DATA_REGISTRATION));

        Graph containerGraph;
        String registrationSubject = remoteResource.getURI() + "#registration";
        if (remoteResource.exists()) {
            containerGraph = remoteResource.getGraph(remoteResource.getURI());

            // Remove any previous triples for the data registration
            GraphUtil.remove(containerGraph, NodeFactory.createURI(registrationSubject), NodeFactory.createURI(PREDICATE_ECOSYSTEM_REGISTERED_BY), null);
            GraphUtil.remove(containerGraph, NodeFactory.createURI(registrationSubject), NodeFactory.createURI(PREDICATE_ECOSYSTEM_REGISTERED_WITH), null);
            GraphUtil.remove(containerGraph, NodeFactory.createURI(registrationSubject), NodeFactory.createURI(PREDICATE_ECOSYSTEM_REGISTERED_AT), null);
            GraphUtil.remove(containerGraph, NodeFactory.createURI(registrationSubject), NodeFactory.createURI(PREDICATE_ECOSYSTEM_REGISTERED_SHAPE_TREE), null);
            GraphUtil.remove(containerGraph, NodeFactory.createURI(registrationSubject), NodeFactory.createURI(PREDICATE_ECOSYSTEM_HAS_REGISTERED_DATA_INSTANCE), null);
        } else {
            containerGraph = ModelFactory.createDefaultModel().getGraph();
        }

        List<Triple> triplesToAdd = new ArrayList<>();
        triplesToAdd.add(new Triple(NodeFactory.createURI(registrationSubject), RDF.type.asNode(), NodeFactory.createURI(ECO_DATA_REGISTRATION)));
        triplesToAdd.add(new Triple(NodeFactory.createURI(registrationSubject), NodeFactory.createURI(PREDICATE_ECOSYSTEM_REGISTERED_BY), NodeFactory.createURI(context.getWebID())));
        triplesToAdd.add(new Triple(NodeFactory.createURI(registrationSubject), NodeFactory.createURI(PREDICATE_ECOSYSTEM_REGISTERED_WITH), NodeFactory.createURI(context.getOriginatorIRI())));
        triplesToAdd.add(new Triple(NodeFactory.createURI(registrationSubject), NodeFactory.createURI(PREDICATE_ECOSYSTEM_REGISTERED_AT), NodeFactory.createLiteralByValue(DateTimeUtils.nowAsXSDDateTimeString(), XSDDatatype.XSDdateTime)));
        triplesToAdd.add(new Triple(NodeFactory.createURI(registrationSubject), NodeFactory.createURI(PREDICATE_ECOSYSTEM_REGISTERED_SHAPE_TREE), NodeFactory.createURI(shapeTreeURI.toString())));
        GraphUtil.add(containerGraph, triplesToAdd);

        String focusNode = "#registration";
        URI focusNodeURI = remoteResource.getURI().resolve(focusNode);

        ValidationResult validationResult = dataRegistrationShapeTree.validateContent(context.getAuthorizationHeaderValue(), containerGraph, focusNodeURI, true);
        if (!validationResult.getValid()) {
            throw new ShapeTreeException(400, "Payload did not meet requirements defined by ShapeTree " + dataRegistrationShapeTree.getURI());
        }

        // Write the updates back to the resource
        remoteResource.updateGraph(containerGraph,false, context.getAuthorizationHeaderValue());


        // parentContainer#registry is where we want to add a predicate to point to our new #registration
        addRegistrationToRegistry(context, registrationSubject);
    }

    private void addRegistrationToRegistry(ShapeTreeContext context, String registrationSubject) throws URISyntaxException, IOException {
        String dataRegistryURI = getDataRegistryURI(context.getWebID(), context.getAuthorizationHeaderValue());
        if (dataRegistryURI != null) {
            RemoteResource dataRegistryResource = new RemoteResource(new URI(dataRegistryURI), context.getAuthorizationHeaderValue());
            Graph dataRegistryGraph;
            if (dataRegistryResource.exists()) {
                dataRegistryGraph = dataRegistryResource.getGraph(dataRegistryResource.getURI());

                List<Triple> tripleToAdd = new ArrayList<>();
                tripleToAdd.add(new Triple(NodeFactory.createURI(dataRegistryURI), NodeFactory.createURI(PREDICATE_ECOSYSTEM_HAS_REGISTRATION), NodeFactory.createURI(registrationSubject)));
                GraphUtil.add(dataRegistryGraph, tripleToAdd);
                dataRegistryResource.updateGraph(dataRegistryGraph, false, context.getAuthorizationHeaderValue());
            }
        } else {
            log.warn("Data Registry does not exist!");
        }
    }

    private String getDataRegistryURI(String webID, String authorizationHeaderValue) throws URISyntaxException, IOException {
        RemoteResource resource = new RemoteResource(new URI(webID), authorizationHeaderValue);
        if (resource.exists()) {
            Graph graph = resource.getGraph(new URI(webID));
            List<Triple> registrarTriples = graph.find(null, NodeFactory.createURI(PREDICATE_ECOSYSTEM_HAS_REGISTRAR), null).toList();
            if (registrarTriples != null && registrarTriples.size() > 0) {
                String registrarURI = registrarTriples.get(0).getObject().getURI();
                resource = new RemoteResource(new URI(registrarURI), authorizationHeaderValue);
                if (resource.exists()) {
                    graph = resource.getGraph(new URI(registrarURI));
                    List<Triple> dataRegistrySetTriples = graph.find(null, NodeFactory.createURI(PREDICATE_ECOSYSTEM_HAS_DATA_REGISTRY_SET), null).toList();
                    if (dataRegistrySetTriples != null && dataRegistrySetTriples.size() > 0) {
                        String dataRegistrySetURI = dataRegistrySetTriples.get(0).getObject().getURI();
                        resource = new RemoteResource(new URI(dataRegistrySetURI), authorizationHeaderValue);
                        if (resource.exists()) {
                            graph = resource.getGraph(new URI(dataRegistrySetURI));
                            List<Triple> dataRegistryTriples = graph.find(null, NodeFactory.createURI(PREDICATE_ECOSYSTEM_HAS_REGISTRY), null).toList();
                            if (dataRegistryTriples != null && dataRegistryTriples.size() > 0) {
                                return dataRegistryTriples.get(0).getObject().getURI();
                            }
                        }
                    }
                }
            }
        }
        return null;
    }

    @Override
    public void unIndexShapeTree(URI parentContainer, URI shapeTreeURI, URI plantedShapeTreeURI) {

    }

    @Override
    public void indexShapeTreeDataInstance(URI shapeTreeURI, URI instanceURI) {

    }

    @Override
    public void unIndexShapeTreeDataInstance(URI shapeTreeURI, URI instanceURI) {

    }
}
