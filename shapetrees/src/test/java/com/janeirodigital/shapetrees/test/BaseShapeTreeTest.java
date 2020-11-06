package com.janeirodigital.shapetrees.test;

import com.janeirodigital.shapetrees.RemoteResource;
import com.janeirodigital.shapetrees.ShapeTreeEcosystem;
import com.janeirodigital.shapetrees.client.ShapeTreeClient;
import com.janeirodigital.shapetrees.client.impl.ShapeTreeClientImpl;
import com.janeirodigital.shapetrees.enums.HttpHeaders;
import com.janeirodigital.shapetrees.enums.LinkRelations;
import com.janeirodigital.shapetrees.model.ShapeTreeContext;
import com.janeirodigital.shapetrees.vocabulary.ShapeTreeVocabulary;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import okhttp3.Request;
import okhttp3.mockwebserver.MockWebServer;
import org.apache.jena.datatypes.TypeMapper;
import org.apache.jena.datatypes.xsd.XSDDatatype;
import org.apache.jena.graph.Graph;
import org.apache.jena.graph.Node;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Triple;
import org.jetbrains.annotations.NotNull;
import org.opentest4j.AssertionFailedError;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.*;

@Slf4j
public abstract class BaseShapeTreeTest {

    protected final ShapeTreeClient shapeTreeClient;
    protected final ShapeTreeContext context;
    protected static String TEXT_TURTLE = "text/turtle";

    public BaseShapeTreeTest(ShapeTreeEcosystem ecosystem) {
        this.context = new ShapeTreeContext();

        this.shapeTreeClient = new ShapeTreeClientImpl(ecosystem);
    }
    
    protected static void ensureExists(URI uri) throws IOException {
        RemoteResource resource = new RemoteResource(uri, null);
        if (!resource.exists()) {
            throw new AssertionFailedError("Resource " + uri + " doesn't exist");
        }
    }

    protected static void ensureExistsWithPredicate(URI uri, URI predicate) throws IOException, URISyntaxException {
        RemoteResource resource = new RemoteResource(uri, null);
        if (!resource.exists()) {
            throw new AssertionFailedError("Resource " + uri + " doesn't exist");
        }
        ensureExistingTriple(resource.getGraph(uri), null, NodeFactory.createURI(predicate.toString()), null);
    }

    protected static void ensureExistsHasMetadataWithPredicateValue(URI uri, URI predicate, Object value) throws IOException, URISyntaxException {
        ensureExistsWithPredicateValue(getMetadataResourceURI(uri), uri, predicate, value);
    }

    protected static void ensureExistsWithPredicateValue(URI uri, URI predicate, Object value) throws IOException, URISyntaxException {
        ensureExistsWithPredicateValue(uri, uri, predicate, value);
    }


    protected static void ensureExistsWithPredicateValue(URI uri, URI baseURI, URI predicate, Object value) throws IOException, URISyntaxException {
        RemoteResource resource = new RemoteResource(uri, null);
        if (!resource.exists()) {
            throw new AssertionFailedError("Resource " + uri + " doesn't exist");
        }
        ensureExistingTriple(resource.getGraph(baseURI), null, NodeFactory.createURI(predicate.toString()), value);
    }

    protected static void ensureExistsHasMetadataWithValues(URI uri, URI instanceRootValue) throws IOException, URISyntaxException{
        RemoteResource resource = new RemoteResource(getMetadataResourceURI(uri), null);
        if (!resource.exists()) {
            throw new AssertionFailedError("Resource " + uri + " doesn't exist");
        }
        ensureExistingTriple(resource.getGraph(uri), null, NodeFactory.createURI(ShapeTreeVocabulary.HAS_SHAPE_TREE_INSTANCE_ROOT), instanceRootValue);
    }

    protected static void ensureExistingTriple(Graph graph, Object subject, Object predicate, Object object) {
        if (graph == null) {
            throw new AssertionFailedError("Graph is null");
        }

        Node subjectNode = createNodeFromInput(subject);
        Node predicateNode = createNodeFromInput(predicate);
        Node objectNode = createNodeFromInput(object);

        if (!graph.contains(subjectNode, predicateNode, objectNode)) {
            throw new AssertionFailedError("Specified Triple not found");
        } else {
            List<Triple> triples = graph.find(subjectNode, predicateNode, objectNode).toList();
            log.trace("Found: " + triples.size());
        }
    }

    private static Node createNodeFromInput(Object input) {
        if (input == null) {
            return null;
        } else if (input instanceof Node) {
            return (Node)input;
        } else if (input instanceof URI) {
            return NodeFactory.createURI((input).toString());
        } else if (input instanceof String) {
            String inputString = (String)input;
            if (inputString.contains("^^")) {
                String inputValue = inputString.substring(0, inputString.indexOf("^^"));
                String dataType = inputString.substring(inputString.indexOf("^^")+2);
                if (dataType.startsWith("xsd:")) {
                    dataType = dataType.replace("xsd:","http://www.w3.org/2001/XMLSchema#");
                }
                return NodeFactory.createLiteral(inputValue, TypeMapper.getInstance().getTypeByName(dataType));
            }
            return NodeFactory.createLiteral((String)input);
        } else if (input instanceof Integer) {
            return NodeFactory.createLiteral(input.toString(), XSDDatatype.XSDint);
        }
        else {
            log.warn("Received an input of type " + input.getClass().getSimpleName() + " treating it as a string literal");
            return NodeFactory.createLiteral((String)input);
        }
    }

    @SneakyThrows
    protected static void ensureNotExists(URI uri) {
        RemoteResource resource = new RemoteResource(uri, null);
        if (resource.exists()) {
            throw new AssertionFailedError("Resource " + uri + " unexpectedly exists");
        }
    }

    @NotNull
    @SneakyThrows
    protected static URI getMetadataResourceURI(URI primaryResourceURI) {
        RemoteResource resource = new RemoteResource(primaryResourceURI, null);
        return new URI(resource.getMetadataURI());
    }

    protected URI getURI(MockWebServer server, String path) throws URISyntaxException {
        return new URI(server.url(path).toString());
    }

    protected void applyCommonHeaders(ShapeTreeContext context, Request.Builder builder, String focusNode, URI shapeTreeHint, Boolean isContainer, String proposedResourceName, String contentType) {
        if (context.getAuthorizationHeaderValue() != null) {
            builder.addHeader(HttpHeaders.AUTHORIZATION.getValue(), context.getAuthorizationHeaderValue());
        }

        if (isContainer != null) {
            String resourceTypeUri = isContainer ? "http://www.w3.org/ns/ldp#Container" : "http://www.w3.org/ns/ldp#Resource";
            builder.addHeader(HttpHeaders.LINK.getValue(), "<" + resourceTypeUri + ">; rel=\"type\"");
        }

        if (focusNode != null) {
            builder.addHeader(HttpHeaders.LINK.getValue(), "<" + focusNode + ">; rel=\"" + LinkRelations.FOCUS_NODE.getValue() + "\"");
        }

        if (shapeTreeHint != null) {
            builder.addHeader(HttpHeaders.LINK.getValue(), "<" + shapeTreeHint + ">; rel=\"" + LinkRelations.TARGET_SHAPETREE + "\"");
        }

        if (proposedResourceName != null) {
            builder.addHeader(HttpHeaders.SLUG.getValue(), proposedResourceName);
        }

        if (contentType != null) {
            builder.addHeader(HttpHeaders.CONTENT_TYPE.getValue(), contentType);
        }

        if (context.getWebID() != null) {
            builder.addHeader(HttpHeaders.INTEROP_WEBID.getValue(), context.getWebID());
        }

        if (context.getOriginatorIRI() != null) {
            builder.addHeader(HttpHeaders.INTEROP_ORIGINATOR.getValue(), context.getOriginatorIRI());
        }
    }

}
