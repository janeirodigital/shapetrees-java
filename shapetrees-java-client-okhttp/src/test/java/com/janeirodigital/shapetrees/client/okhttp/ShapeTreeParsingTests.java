package com.janeirodigital.shapetrees.client.okhttp;

import com.janeirodigital.shapetrees.client.okhttp.fixtures.DispatcherEntry;
import com.janeirodigital.shapetrees.client.okhttp.fixtures.RequestMatchingFixtureDispatcher;
import com.janeirodigital.shapetrees.core.ShapeTreeFactory;
import com.janeirodigital.shapetrees.core.enums.RecursionMethods;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.ShapeTree;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

@Slf4j
public class ShapeTreeParsingTests extends BaseShapeTreeTest {

    public ShapeTreeParsingTests() {
        super();
    }

    private static RequestMatchingFixtureDispatcher dispatcher = null;

    @BeforeAll
    static void beforeAll() {
        dispatcher = new RequestMatchingFixtureDispatcher(List.of(
                new DispatcherEntry(List.of("shapetrees/medical-record-shapetree-ttl"), "GET", "/static/shapetrees/medical-record/shapetree", null),
                new DispatcherEntry(List.of("shapetrees/medical-record-shapetree-invalid-ttl"), "GET", "/static/shapetrees/medical-record/shapetree-invalid", null),
                new DispatcherEntry(List.of("shapetrees/medical-record-shapetree-invalid-2-ttl"), "GET", "/static/shapetrees/medical-record/shapetree-invalid2", null)
        ));
    }

    @SneakyThrows
    @Test
    @DisplayName("Reuse previously cached shapetree")
    void parseShapeTreeReuse() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ShapeTree medicalRecordsShapeTree1 = ShapeTreeFactory.getShapeTree(getURI(server,"/static/shapetrees/medical-record/shapetree#medicalRecords"));
        Assertions.assertNotNull(medicalRecordsShapeTree1);
        ShapeTree medicalRecordsShapeTree2 = ShapeTreeFactory.getShapeTree(getURI(server,"/static/shapetrees/medical-record/shapetree#medicalRecords"));
        Assertions.assertNotNull(medicalRecordsShapeTree1);
        assertEquals(medicalRecordsShapeTree1.hashCode(), medicalRecordsShapeTree2.hashCode());
    }

    @SneakyThrows
    @Test
    @DisplayName("Attempt to retrieve an Allows* IRI")
    void retrieveAllowsIri() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ShapeTree allowAllShapeTree = ShapeTreeFactory.getShapeTree(new URI("http://www.w3.org/ns/shapetree#AllowAll"));
        Assertions.assertNull(allowAllShapeTree);
    }

    @SneakyThrows
    @Test
    @DisplayName("Invalid shapetree contents within non-container")
    void validateContainsWithinNonContainer() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        Assertions.assertThrows(ShapeTreeException.class, () ->
                ShapeTreeFactory.getShapeTree(getURI(server,"/static/shapetrees/medical-record/shapetree-invalid2#medicalRecords"))
        );
    }


    @SneakyThrows
    @Test
    @DisplayName("Ensure reuse within recursion")
    void ensureCacheWithRecursion() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        // Retrieve the conditions shapetree (which is referred to by the medicalRecords shapetree)
        ShapeTree conditionsShapeTree1 = ShapeTreeFactory.getShapeTree(getURI(server,"/static/shapetrees/medical-record/shapetree#conditions"));
        Assertions.assertNotNull(conditionsShapeTree1);

        // Retrieve the medical records shapetree which will recursively cache the conditions shapetree
        ShapeTree medicalRecordsShapeTree1 = ShapeTreeFactory.getShapeTree(getURI(server,"/static/shapetrees/medical-record/shapetree#medicalRecords"));
        Assertions.assertNotNull(medicalRecordsShapeTree1);

        // Retrieve the conditions shapetree again, ensuring the same instance is used
        ShapeTree conditionsShapeTree2 = ShapeTreeFactory.getShapeTree(getURI(server,"/static/shapetrees/medical-record/shapetree#conditions"));
        Assertions.assertNotNull(conditionsShapeTree2);

        assertEquals(conditionsShapeTree1.hashCode(), conditionsShapeTree2.hashCode());
    }


    @SneakyThrows
    @Test
    @DisplayName("Parse Tree with references")
    void parseShapeTreeReferences() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ShapeTree medicalRecordsShapeTree = ShapeTreeFactory.getShapeTree(getURI(server,"/static/shapetrees/medical-record/shapetree#medicalRecords"));
        Assertions.assertNotNull(medicalRecordsShapeTree);
        assertFalse(medicalRecordsShapeTree.getReferences().isEmpty());
    }

    @SneakyThrows
    @Test
    @DisplayName("Parse Tree with contains")
    void parseShapeTreeContains() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ShapeTree medicalRecordsShapeTree = ShapeTreeFactory.getShapeTree(getURI(server,"/static/shapetrees/medical-record/shapetree#medicalRecords"));
        Assertions.assertNotNull(medicalRecordsShapeTree);
        assertTrue(medicalRecordsShapeTree.getContains().contains(getURI(server,"/static/shapetrees/medical-record/shapetree#medicalRecord")));
    }

    @SneakyThrows
    @Test
    @DisplayName("Parse Tree Failure")
    void parseShapeTreeFailure() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        Assertions.assertThrows(ShapeTreeException.class, () ->
            ShapeTreeFactory.getShapeTree(getURI(server,"/static/shapetrees/medical-record/shapetree-invalid#medicalRecords"))
        );
    }

    @SneakyThrows
    @Test
    @DisplayName("Traverse References")
    void testTraverseReferences() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ShapeTree medicalRecordShapeTree = ShapeTreeFactory.getShapeTree(getURI(server,"/static/shapetrees/medical-record/shapetree#medicalRecord"));
        medicalRecordShapeTree.getReferencedShapeTrees();
        medicalRecordShapeTree.getReferencedShapeTrees(RecursionMethods.BREADTH_FIRST);
        medicalRecordShapeTree.getReferencedShapeTrees(RecursionMethods.DEPTH_FIRST);
    }
}
