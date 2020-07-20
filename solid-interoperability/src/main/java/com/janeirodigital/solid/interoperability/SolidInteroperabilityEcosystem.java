package com.janeirodigital.solid.interoperability;

import com.janeirodigital.shapetrees.RemoteResource;
import com.janeirodigital.shapetrees.ShapeTreeEcosystem;
import com.janeirodigital.shapetrees.model.ShapeTree;
import com.janeirodigital.shapetrees.model.ShapeTreeContext;
import com.janeirodigital.shapetrees.model.ShapeTreePlantResult;
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
import java.util.Map;

@Slf4j
public class SolidInteroperabilityEcosystem implements ShapeTreeEcosystem {

    private static final String SHAPE_TREE_DATA_REGISTRATION = "http://localhost:9999/static/ecosystem/ecosystem-ShapeTree.ttl#data-registration-tree";
    private static final String REL_SHAPE_TREE = "ShapeTree";

    @Override
    public void initializeEcosystem() {

    }

    @Override
    public ShapeTreePlantResult getExistingShapeTreeFromContainer(ShapeTreeContext context, URI parentContainer, List<ShapeTree> shapeTreesToPlant, String requestedName) {
        // TODO implement specific logic to look for existing ShapeTree plants for the given ShapeTree
        return new ShapeTreePlantResult();
    }

    @Override
    public Graph beforePlantShapeTree(ShapeTreeContext context, URI expectedURI, Graph incomingGraph, List<ShapeTree> shapeTreesToPlant, Map<String, List<String>> linkHeaders) throws URISyntaxException {
        // If one of the ShapeTrees being planted is a data registration, add the data registration triples to the incoming graph
        if (linkHeaders.get(REL_SHAPE_TREE) != null && linkHeaders.get(REL_SHAPE_TREE).contains(SHAPE_TREE_DATA_REGISTRATION)) {

            List<Triple> triplesToAdd = new ArrayList<>();
            String registrationSubject = expectedURI + "#registration";
            triplesToAdd.add(new Triple(NodeFactory.createURI(registrationSubject), RDF.type.asNode(), NodeFactory.createURI(SolidEcosystem.DATA_REGISTRATION)));
            triplesToAdd.add(new Triple(NodeFactory.createURI(registrationSubject), NodeFactory.createURI(SolidEcosystem.REGISTERED_BY), NodeFactory.createURI(context.getWebID())));
            triplesToAdd.add(new Triple(NodeFactory.createURI(registrationSubject), NodeFactory.createURI(SolidEcosystem.REGISTERED_WITH), NodeFactory.createURI(context.getOriginatorIRI())));
            triplesToAdd.add(new Triple(NodeFactory.createURI(registrationSubject), NodeFactory.createURI(SolidEcosystem.REGISTERED_AT), NodeFactory.createLiteralByValue(DateTimeUtils.nowAsXSDDateTimeString(), XSDDatatype.XSDdateTime)));
            for (ShapeTree shapeTreeToPlant : shapeTreesToPlant) {
                if (!shapeTreeToPlant.getURI().toString().equals(SHAPE_TREE_DATA_REGISTRATION)) {
                    triplesToAdd.add(new Triple(NodeFactory.createURI(registrationSubject), NodeFactory.createURI(SolidEcosystem.REGISTERED_SHAPE_TREE), NodeFactory.createURI(shapeTreeToPlant.getURI().toString())));
                }
            }

            if (incomingGraph == null) {
                incomingGraph = ModelFactory.createDefaultModel().getGraph();
            }

            GraphUtil.add(incomingGraph, triplesToAdd);
        }

        return incomingGraph;
    }

    @Override
    public void indexShapeTree(ShapeTreeContext context, URI parentContainer, URI shapeTreeURI, URI plantedShapeTreeURI, Map<String, List<String>> linkHeaders) throws IOException, URISyntaxException {

        if (linkHeaders.get(REL_SHAPE_TREE) != null && linkHeaders.get(REL_SHAPE_TREE).contains(SHAPE_TREE_DATA_REGISTRATION)) {
            // Do not add another hasRegistration if it is the data registration tree itself
            if (!shapeTreeURI.toString().equals(SHAPE_TREE_DATA_REGISTRATION)) {
                String registrationSubject = plantedShapeTreeURI + "#registration";
                // parentContainer#registry is where we want to add a predicate to point to our new #registration
                addRegistrationToRegistry(context, registrationSubject);
            }
        }
    }

    @Override
    public void unIndexShapeTree(URI parentContainer, URI shapeTreeURI, URI plantedShapeTreeURI) {

    }

    @Override
    public void indexShapeTreeDataInstance(ShapeTreeContext context, URI parentContainerURI, URI shapeTreeURI, URI instanceURI) throws IOException {
        log.info("Indexing Instance");
        RemoteResource parentContainerResource = new RemoteResource(parentContainerURI, context.getAuthorizationHeaderValue());
        Graph dataRegistrationGraph;
        if (parentContainerResource.exists()) {
            dataRegistrationGraph = parentContainerResource.getGraph(parentContainerResource.getURI());
            // If the parent container is not a registration, stop here
            if (!dataRegistrationGraph.contains(NodeFactory.createURI(parentContainerResource.getURI() + "#registration"), null, null)) return;

            List<Triple> tripleToAdd = new ArrayList<>();
            tripleToAdd.add(new Triple(NodeFactory.createURI(parentContainerResource.getURI() + "#registration"), NodeFactory.createURI(SolidEcosystem.HAS_REGISTERED_DATA_INSTANCE), NodeFactory.createURI(instanceURI.toString())));
            GraphUtil.add(dataRegistrationGraph, tripleToAdd);
            parentContainerResource.updateGraph(dataRegistrationGraph, false, context.getAuthorizationHeaderValue());
        }
    }

    @Override
    public void unIndexShapeTreeDataInstance(URI shapeTreeURI, URI instanceURI) {

    }

    private void addRegistrationToRegistry(ShapeTreeContext context, String registrationSubject) throws URISyntaxException, IOException {
        String dataRegistryURI = getDataRegistryURI(context.getWebID(), context.getAuthorizationHeaderValue());
        if (dataRegistryURI != null) {
            RemoteResource dataRegistryResource = new RemoteResource(new URI(dataRegistryURI), context.getAuthorizationHeaderValue());
            Graph dataRegistryGraph;
            if (dataRegistryResource.exists()) {
                dataRegistryGraph = dataRegistryResource.getGraph(dataRegistryResource.getURI());

                List<Triple> tripleToAdd = new ArrayList<>();
                tripleToAdd.add(new Triple(NodeFactory.createURI(dataRegistryURI), NodeFactory.createURI(SolidEcosystem.HAS_REGISTRATION), NodeFactory.createURI(registrationSubject)));
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
            List<Triple> registrarTriples = graph.find(null, NodeFactory.createURI(SolidEcosystem.HAS_REGISTRAR), null).toList();
            if (registrarTriples != null && registrarTriples.size() > 0) {
                String registrarURI = registrarTriples.get(0).getObject().getURI();
                resource = new RemoteResource(new URI(registrarURI), authorizationHeaderValue);
                if (resource.exists()) {
                    graph = resource.getGraph(new URI(registrarURI));
                    List<Triple> dataRegistrySetTriples = graph.find(null, NodeFactory.createURI(SolidEcosystem.HAS_DATA_REGISTRY_SET), null).toList();
                    if (dataRegistrySetTriples != null && dataRegistrySetTriples.size() > 0) {
                        String dataRegistrySetURI = dataRegistrySetTriples.get(0).getObject().getURI();
                        resource = new RemoteResource(new URI(dataRegistrySetURI), authorizationHeaderValue);
                        if (resource.exists()) {
                            graph = resource.getGraph(new URI(dataRegistrySetURI));
                            List<Triple> dataRegistryTriples = graph.find(null, NodeFactory.createURI(SolidEcosystem.HAS_REGISTRY), null).toList();
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

}
