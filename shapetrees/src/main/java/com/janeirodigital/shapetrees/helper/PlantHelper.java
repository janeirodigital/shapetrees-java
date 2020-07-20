package com.janeirodigital.shapetrees.helper;

import com.janeirodigital.shapetrees.RemoteResource;
import com.janeirodigital.shapetrees.ShapeTreeFactory;
import com.janeirodigital.shapetrees.enums.LinkRelations;
import com.janeirodigital.shapetrees.model.ShapeTreeLocator;
import com.janeirodigital.shapetrees.vocabulary.LdpVocabulary;
import com.janeirodigital.shapetrees.vocabulary.ShapeTreeVocabulary;
import com.janeirodigital.shapetrees.enums.HttpHeaders;
import com.janeirodigital.shapetrees.model.ShapeTree;
import com.janeirodigital.shapetrees.model.ShapeTreePlantResult;
import lombok.extern.slf4j.Slf4j;
import okhttp3.*;
import org.apache.commons.lang3.StringUtils;
import org.apache.jena.graph.Graph;
import org.apache.jena.graph.GraphUtil;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Triple;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;

import java.io.IOException;
import java.io.StringWriter;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Slf4j
public class PlantHelper {

    private static final String REL_TYPE_CONTAINER = "<" + LdpVocabulary.CONTAINER + ">; rel=\"" + LinkRelations.TYPE.getValue() + "\"";

    public static ShapeTreePlantResult plantShapeTree(String authorizationHeaderValue, RemoteResource parentContainer, Graph bodyGraph, ShapeTree rootShapeTree, ShapeTree shapeTree, String requestedName, String shapeTreePath, int depth) throws IOException, URISyntaxException {
        StringWriter sw = new StringWriter();
        if (bodyGraph != null) {
            RDFDataMgr.write(sw, bodyGraph, Lang.TURTLE);
        }
        return plantShapeTree(authorizationHeaderValue, parentContainer, sw.toString(), rootShapeTree, shapeTree, requestedName, shapeTreePath, depth);
    }

    public static ShapeTreePlantResult plantShapeTree(String authorizationHeaderValue, RemoteResource parentContainer, String body, ShapeTreeLocator locator, ShapeTree targetShapeTree, String requestedName, int depth) throws IOException, URISyntaxException {
        ShapeTree rootShapeTree = ShapeTreeFactory.getShapeTree(new URI(locator.getRootShapeTree()));

        // Determine the depth based on container and the relative depth
        String containerPath = locator.getShapeTreeInstancePath();

        return plantShapeTree(authorizationHeaderValue, parentContainer, body, rootShapeTree, targetShapeTree, requestedName, containerPath + requestedName + "/", depth);
    }

    public static ShapeTreePlantResult plantShapeTree(String authorizationHeaderValue, RemoteResource parentContainer, String body, ShapeTree rootShapeTree, ShapeTree shapeTree, String requestedName, String shapeTreePath, int depth) throws IOException, URISyntaxException {
        log.debug("plantShapeTree: parent [{}], root tree [{}], tree [{}], slug [{}], path [{}], depth [{}]", parentContainer.getURI(), rootShapeTree.getId(), shapeTree.getId(), requestedName, shapeTreePath, depth);


        RemoteResource plantedContainer = createOrReuseContainer(parentContainer.getURI(), requestedName, body, authorizationHeaderValue);
        RemoteResource plantedContainerMetadataResource = plantedContainer.getMetadataResource(authorizationHeaderValue);

        // Get the existing graph and reuse it, if possible, if not, create a new graph
        Graph plantedContainerMetadataGraph;
        if (plantedContainerMetadataResource.exists()) {
            plantedContainerMetadataGraph = plantedContainerMetadataResource.getGraph(plantedContainer.getURI());
        } else {
            plantedContainerMetadataGraph = ModelFactory.createDefaultModel().getGraph();
        }

        // Generate a UUID for the ShapeTree
        UUID shapeTreeLocatorUUID = UUID.randomUUID();

        List<Triple> triplesToAdd = new ArrayList<>();
        // Add the triple for the new tree:hasShapeTreeLocator
        String plantedContainerURI = plantedContainer.getURI().toString() + (plantedContainer.getURI().toString().endsWith("/")? "":"/");
        String shapeTreeLocatorURI = plantedContainerURI + "#" + shapeTreeLocatorUUID;
        triplesToAdd.add(new Triple(NodeFactory.createURI(plantedContainerURI), NodeFactory.createURI(ShapeTreeVocabulary.HAS_SHAPE_TREE_LOCATOR), NodeFactory.createURI(shapeTreeLocatorURI)));

        // Add the triples for the locator itself
        triplesToAdd.add(new Triple(NodeFactory.createURI(shapeTreeLocatorURI), NodeFactory.createURI(ShapeTreeVocabulary.HAS_SHAPE_TREE), NodeFactory.createURI(shapeTree.getId())));
        triplesToAdd.add(new Triple(NodeFactory.createURI(shapeTreeLocatorURI), NodeFactory.createURI(ShapeTreeVocabulary.HAS_SHAPE_TREE_INSTANCE_PATH), NodeFactory.createLiteral(shapeTreePath)));
        String relativePath = (depth==0) ? "./" : StringUtils.repeat("../", depth);
        triplesToAdd.add(new Triple(NodeFactory.createURI(shapeTreeLocatorURI), NodeFactory.createURI(ShapeTreeVocabulary.HAS_SHAPE_TREE_INSTANCE_ROOT), NodeFactory.createURI(relativePath)));
        triplesToAdd.add(new Triple(NodeFactory.createURI(shapeTreeLocatorURI), NodeFactory.createURI(ShapeTreeVocabulary.HAS_SHAPE_TREE_ROOT), NodeFactory.createURI(rootShapeTree.getId())));
        GraphUtil.add(plantedContainerMetadataGraph, triplesToAdd);
        // Write the updates back to the resource
        plantedContainerMetadataResource.updateGraph(plantedContainerMetadataGraph,false, authorizationHeaderValue);

        List<URI> nestedContainersCreated = new ArrayList<>();

        depth++;
        // Recursively call plantShapeTree for any static, nested container contents -- resources and dynamically named containers are ignored
        for (URI contentShapeTreeURI : shapeTree.getContains()) {
            ShapeTree contentShapeTree = ShapeTreeFactory.getShapeTree(contentShapeTreeURI);
            if (contentShapeTree != null && contentShapeTree.getLabel() != null) {
                // the return URI is discarded for recursive calls
                // Add a trailing slash so recursion lines up nicely to paths
                if (shapeTreePath.equals(".")) shapeTreePath = "./";
                ShapeTreePlantResult nestedResult = plantShapeTree(authorizationHeaderValue, plantedContainer, (String)null, rootShapeTree, contentShapeTree, contentShapeTree.getLabel(), shapeTreePath + contentShapeTree.getLabel() +"/", depth);
                nestedContainersCreated.add(nestedResult.getRootContainer());
            }
        }

        return new ShapeTreePlantResult(shapeTree.getURI(), plantedContainer.getURI(), plantedContainerMetadataResource.getURI(), nestedContainersCreated);
    }

    private static RemoteResource createOrReuseContainer(URI parentContainerURI, String requestedName, String body, String authorizationHeaderValue) throws IOException, URISyntaxException {
        // First determine if we're looking to plant a ShapeTree in an existing container
        RemoteResource targetContainer = new RemoteResource(parentContainerURI + requestedName, authorizationHeaderValue);
        if (targetContainer.exists()) {
            // If the container already exists, it will not be created again
            return targetContainer;
        } else {
            // Create new container with the Slug/Requested Name
            RemoteResource shapeTreeContainer = createContainer(authorizationHeaderValue, parentContainerURI, requestedName, body);
            // Depending on server implementation, after a POST the response header may pertain to the parent container (the URI)
            // as opposed to the newly created resource.  To ensure we get the proper headers, we reload the contents of the
            // newly created container with a GET.
            shapeTreeContainer = new RemoteResource(shapeTreeContainer.getURI(), authorizationHeaderValue);
            return shapeTreeContainer;
        }
    }

    private static RemoteResource createContainer(String authorizationHeaderValue, URI parentURI, String requestedName, String body) throws IOException, URISyntaxException {
        log.debug("createContainer: parent [{}], slug [{}]", parentURI, requestedName);

        if (body == null) {
            body = "";
        }

        OkHttpClient httpClient = HttpClientHelper.getClient();
        Request createContainerPost = new Request.Builder()
                .addHeader(HttpHeaders.SLUG.getValue(), requestedName)
                .addHeader(HttpHeaders.LINK.getValue(), REL_TYPE_CONTAINER)
                .addHeader(HttpHeaders.CONTENT_TYPE.getValue(), "text/turtle")
                .addHeader(HttpHeaders.AUTHORIZATION.getValue(), authorizationHeaderValue)
                .post(RequestBody.create(body, MediaType.get("text/turtle")))
                .url(parentURI.toURL()).build();

        Response response = httpClient.newCall(createContainerPost).execute();
        return new RemoteResource(response);
    }
}
