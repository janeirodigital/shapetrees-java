package com.janeirodigital.shapetrees.test;

import com.janeirodigital.shapetrees.ShapeTreeException;
import com.janeirodigital.shapetrees.ShapeTreeFactory;
import com.janeirodigital.shapetrees.model.ShapeTree;
import com.janeirodigital.shapetrees.test.fixtures.DispatcherEntry;
import com.janeirodigital.shapetrees.test.fixtures.RequestMatchingFixtureDispatcher;
import com.janeirodigital.shapetrees.vocabulary.ShapeTreeVocabulary;
import jdk.jfr.Label;
import lombok.SneakyThrows;
import okhttp3.mockwebserver.MockWebServer;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;
import org.junit.jupiter.api.*;

import java.io.StringReader;
import java.net.URI;
import java.util.Collections;
import java.util.List;
import static org.junit.jupiter.api.Assertions.*;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class ShapeTreeValidationTests extends BaseShapeTreeTest {

    public ShapeTreeValidationTests() {
        super(new MockEcosystem());
    }

    private static RequestMatchingFixtureDispatcher dispatcher = null;

    @BeforeAll
    static void beforeAll() {
        dispatcher = new RequestMatchingFixtureDispatcher(List.of(
                new DispatcherEntry(List.of("shapetrees/medical-record-shapetree-ttl"), "GET", "/static/shapetrees/medical-record/shapetree", null),
                new DispatcherEntry(List.of("schemas/fhir-shex"), "GET", "/static/shex/fhir/r4/shex", null)
        ));
    }

    @Order(1)
    @SneakyThrows
    @Test
    @Label("Attempt validation with tree that does not validate")
    void testTreeWithNoValidation() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Get the medicalRecords shape tree.  Note this shape tree doesn't have a
        // validatedBy property, as such it should return an error when using to validate
        ShapeTree medicalRecordsShapeTree = ShapeTreeFactory.getShapeTree(getURI(server, "/static/shapetrees/medical-record/shapetree#medicalRecords"));
        String graphTtl = "<#a> <#b> <#c> .";
        StringReader sr = new StringReader(graphTtl);
        Model model = ModelFactory.createDefaultModel();
        RDFDataMgr.read(model, sr, "http://example.com/", Lang.TTL);

        assertThrows(ShapeTreeException.class, () -> medicalRecordsShapeTree.validateContent(model.getGraph(), new URI("#a"), true));
    }

    @Order(2)
    @SneakyThrows
    @Test
    @Label("Attempt validation with mismatching isContainer parameter")
    void testTreeWithIncorrectContainerFlag() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Get the medicalRecords shape tree.  Note this shape tree is marked
        // as a container - as such it is expected that the resource provided
        // pertains to a container resource
        ShapeTree medicalRecordsShapeTree = ShapeTreeFactory.getShapeTree(getURI(server, "/static/shapetrees/medical-record/shapetree#medicalRecords"));
        String graphTtl = "<#a> <#b> <#c> .";
        StringReader sr = new StringReader(graphTtl);
        Model model = ModelFactory.createDefaultModel();
        RDFDataMgr.read(model, sr, "http://example.com/", Lang.TTL);

        assertThrows(ShapeTreeException.class, () -> medicalRecordsShapeTree.validateContent(model.getGraph(), new URI("#a"), false));
    }


}
