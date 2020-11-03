package com.janeirodigital.shapetrees.test;

import com.janeirodigital.shapetrees.ShapeTreeException;
import com.janeirodigital.shapetrees.ShapeTreeFactory;
import com.janeirodigital.shapetrees.model.ShapeTree;
import com.janeirodigital.shapetrees.test.fixtures.DispatcherEntry;
import com.janeirodigital.shapetrees.test.fixtures.RequestMatchingFixtureDispatcher;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

@Slf4j
public class ShapeTreeParsingTests extends BaseShapeTreeTest {

    public ShapeTreeParsingTests() {
        super(new MockEcosystem());
    }

    private static RequestMatchingFixtureDispatcher dispatcher = null;

    @BeforeAll
    static void beforeAll() {
        dispatcher = new RequestMatchingFixtureDispatcher(List.of(
                new DispatcherEntry(List.of("medical-record-shapetree-ttl"), "GET", "/static/shapetrees/medical-record/shapetree", null),
                new DispatcherEntry(List.of("medical-record-shapetree-invalid-ttl"), "GET", "/static/shapetrees/medical-record/shapetree-invalid", null)
        ));
    }

    @SneakyThrows
    @Test
    @DisplayName("Parse Tree with references")
    void parseShapeTreeReferences() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ShapeTree medicalRecordsShapeTree = ShapeTreeFactory.getShapeTree(getURI(server,"/static/shapetrees/medical-record/shapetree#medicalRecords"));
        assertNotNull(medicalRecordsShapeTree);
        assertFalse(medicalRecordsShapeTree.getReferences().isEmpty());
    }

    @SneakyThrows
    @Test
    @DisplayName("Parse Tree with contains")
    void parseShapeTreeContains() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        ShapeTree medicalRecordsShapeTree = ShapeTreeFactory.getShapeTree(getURI(server,"/static/shapetrees/medical-record/shapetree#medicalRecords"));
        assertNotNull(medicalRecordsShapeTree);
        assertTrue(medicalRecordsShapeTree.getContains().contains(getURI(server,"/static/shapetrees/medical-record/shapetree#medicalRecord")));
    }

    @SneakyThrows
    @Test
    @DisplayName("Parse Tree Failure")
    void parseShapeTreeFailure() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);
        assertThrows(ShapeTreeException.class, () ->
            ShapeTreeFactory.getShapeTree(getURI(server,"/static/shapetrees/medical-record/shapetree-invalid#medicalRecords"))
        );
    }
}
