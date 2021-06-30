package com.janeirodigital.shapetrees.core.models;

import com.janeirodigital.shapetrees.core.SchemaCache;
import com.janeirodigital.shapetrees.core.ShapeTreeFactory;
import com.janeirodigital.shapetrees.core.contentloaders.DocumentContentsLoader;
import com.janeirodigital.shapetrees.core.enums.RecursionMethods;
import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.vocabularies.ShapeTreeVocabulary;
import fr.inria.lille.shexjava.GlobalFactory;
import fr.inria.lille.shexjava.schema.Label;
import fr.inria.lille.shexjava.schema.ShexSchema;
import fr.inria.lille.shexjava.schema.parsing.ShExCParser;
import fr.inria.lille.shexjava.validation.RecursiveValidation;
import fr.inria.lille.shexjava.validation.ValidationAlgorithm;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.rdf.api.IRI;
import org.apache.commons.rdf.jena.JenaRDF;
import org.apache.jena.graph.Graph;
import org.apache.jena.graph.GraphUtil;
import org.apache.jena.graph.Node;

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
    private String shape;
    private String label;
    private String supports;
    private List<URI> contains = new ArrayList<>();
    private List<String> alsoAllow = new ArrayList<>();
    private List<ReferencedShapeTree> references;

    public ShapeTree(DocumentContentsLoader documentContentsLoader) {
        this.documentContentsLoader = documentContentsLoader;
    }

    public URI getURI() throws URISyntaxException {
        return new URI(this.id);
    }

    public ValidationResult validateResource(String requestedName, ShapeTreeResourceType resourceType, Graph bodyGraph, URI focusNodeURI) throws IOException, URISyntaxException {

        ValidationResult result = new ValidationResult(true, null);

        // Check whether the proposed resource is the same type as what is expected by the shape tree
        if (this.expectedResourceType != resourceType.toString()) {
            throw new ShapeTreeException(400, "The resource type being validated does not match the type expected by the ShapeTree");
        }

        // If a label is specified, check if the proposed name is the same
        if (this.label != null && !this.label.equals(requestedName)) {
            throw new ShapeTreeException(400, "The proposed resource name does not match the resource label expected by the ShapeTree");
        }

        // If the shape tree specifies a shape to validate, perform shape validation
        if (this.shape != null) {
            ValidationResult shapeResult = this.validateGraph(bodyGraph, focusNodeURI);
            result = shapeResult;
        }

        return result;

    }

    public ValidationResult validateGraph(Graph graph, URI focusNodeURI) throws IOException, URISyntaxException {

        if (this.shape == null) {
            throw new ShapeTreeException(400, "Attempting to validate a ShapeTree (" + id + ") that does not have an associated Shape");
        }
        URI resolvedShapeURI = URI.create(this.id).resolve(this.shape);

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
        Label shapeLabel = new Label(GlobalFactory.RDFFactory.createIRI(this.shape));

        if (focusNodeURI != null) {

            IRI focusNode = GlobalFactory.RDFFactory.createIRI(focusNodeURI.toString());
            log.debug("Validating Shape Label = {}, Focus Node = {}", shapeLabel.toPrettyString(), focusNode.getIRIString());
            validation.validate(focusNode, shapeLabel);
            boolean valid = validation.getTyping().isConformant(focusNode, shapeLabel);
            return new ValidationResult(valid, focusNodeURI);

        } else {

            List<Node> evaluateNodes = GraphUtil.listSubjects(graph, Node.ANY, Node.ANY).toList();

            for (Node evaluateNode : evaluateNodes) {

                IRI node = GlobalFactory.RDFFactory.createIRI(evaluateNode.getURI());
                validation.validate(node, shapeLabel);
                boolean valid = validation.getTyping().isConformant(node, shapeLabel);

                if (!valid) {
                    continue;
                } else {
                    return new ValidationResult(valid, URI.create(evaluateNode.getURI()));
                }

            }

            return new ValidationResult(false, null);

        }
    }

    public ShapeTreeMatch findMatchingShapeTreeInContains(String requestedName, ShapeTreeResourceType resourceType, URI targetShapeTreeURI, Graph bodyGraph, String focusNode) throws URISyntaxException, IOException, ShapeTreeException {

        // TODO - Once we get through this, need to determine if there's a way to optimize out recurring calls

        // Not a containing shape tree
        if (this.contains == null || this.contains.isEmpty()) { return null; }

        ShapeTreeMatch match = new ShapeTreeMatch();

        // If a target shape was supplied
        if (targetShapeTreeURI != null) {
            // And if it exists in st:contains
            if (this.contains.contains(targetShapeTreeURI)) {
                // Return a ShapeTreeMatch. There is an expectation that a client smart enough to provide a target
                // shape tree will be smart enough to provide the right focus node.
                ShapeTree targetShapeTree = ShapeTreeFactory.getShapeTree(targetShapeTreeURI);
                return new ShapeTreeMatch(targetShapeTreeURI.toString(),
                                          targetShapeTree.getShape(),
                                          focusNode, false);
            } else {
                // We have a misunderstanding if a target shape tree is supplied but doesn't exist in st:contains
                throw new ShapeTreeException(422, "A target shape tree was provided (" + targetShapeTreeURI + ") but it did not exist within :contains");
            }
        } else {
            // For each shape tree in st:contains
            for (URI containsShapeTreeURI : this.contains) {

                ShapeTree containsShapeTree = ShapeTreeFactory.getShapeTree(containsShapeTreeURI);
                if (containsShapeTree == null) { continue; } // Continue if the shape tree isn't gettable

                // Evaluate the shape tree against the attributes of the proposed resources
                // TODO - Will need to create this method, should be validateResource -> calls validateAttributes -> calls validateGraph
                ValidationResult result = containsShapeTree.validateResource(requestedName, resourceType, bodyGraph, null);

                // Continue if the proposed attributes were not a match
                if (!result.getValid()) { continue; }

                // Update ShapeTreeMatch with the information
                return new ShapeTreeMatch(containsShapeTreeURI.toString(),
                           containsShapeTree.getShape(),
                           result.getMatchingNode().toString(), false);
            }
        }

        // No target shape tree was provided, and no match could be derived on its own
        // At this point it must be decided if an unexpected resource is allowed to be created

        // Default behavior is assumed to be ALLOW_ONLY what was specified in st:contains and nothing else
        if (this.alsoAllow == null || this.alsoAllow.isEmpty() ||
                this.alsoAllow.contains(new URI(ShapeTreeVocabulary.ALLOW_ONLY))) {
            throw new ShapeTreeException(422, exceptionMessage(requestedName, this.getId(), "Further, :AllowOnly does not permit any exceptions"));
        }

        // If none of the other ALLOW_* predicates are present, reject by default
        if (!this.alsoAllow.contains(new URI(ShapeTreeVocabulary.ALLOW_ALL)) &&
                !this.alsoAllow.contains(new URI(ShapeTreeVocabulary.ALLOW_NON_RDF_SOURCES)) &&
                !this.alsoAllow.contains(new URI(ShapeTreeVocabulary.ALLOW_CONTAINERS)) &&
                !this.alsoAllow.contains(new URI(ShapeTreeVocabulary.ALLOW_RESOURCES))
        ) {
            throw new ShapeTreeException(422, exceptionMessage(requestedName, this.getId(), "Further, no :Allows* are specified to mitigate"));
        }

        // If it is a non-RDF source and non-RDF sources are not explicitly allowed for...
        if (resourceType == ShapeTreeResourceType.NON_RDF &&
                !this.alsoAllow.contains(new URI(ShapeTreeVocabulary.ALLOW_ALL)) &&
                !this.alsoAllow.contains(new URI(ShapeTreeVocabulary.ALLOW_NON_RDF_SOURCES))) {
            throw new ShapeTreeException(422, exceptionMessage(requestedName, this.getId(), "Further, the requested resource is a NonRDFSource and :AllowNonRDFSources was not specified within :contents"));
        }
        // if it is a Container and Containers are not explicitly allowed for...
        if (resourceType == ShapeTreeResourceType.CONTAINER &&
                !this.alsoAllow.contains(new URI(ShapeTreeVocabulary.ALLOW_ALL)) &&
                !this.alsoAllow.contains(new URI(ShapeTreeVocabulary.ALLOW_CONTAINERS))) {
            throw new ShapeTreeException(422, exceptionMessage(requestedName, this.getId(),"Further, the requested resource is a Container and :AllowContainers was not specified within :contents"));
        }
        //
        if (resourceType == ShapeTreeResourceType.RESOURCE &&
                !this.alsoAllow.contains(new URI(ShapeTreeVocabulary.ALLOW_ALL)) &&
                !this.alsoAllow.contains(new URI(ShapeTreeVocabulary.ALLOW_RESOURCES))) {
            throw new ShapeTreeException(422, exceptionMessage(requestedName, this.getId(), "Further, the requested resource is a Resource and :AllowResources was not specified within :contents"));
        }

        // Return a ShapeTreeMatch indicating that while no shape tree was matched, the resource
        // is still allowed per st:alsoAllow
        return new ShapeTreeMatch(null, null, null, true);
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
