package com.janeirodigital.shapetrees.client.okhttp;

// TODO: Populate tests from primer examples

import com.janeirodigital.shapetrees.client.okhttp.fixtures.DispatcherEntry;
import com.janeirodigital.shapetrees.client.okhttp.fixtures.RequestMatchingFixtureDispatcher;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;
import com.janeirodigital.shapetrees.core.vocabularies.ShapeTreeVocabulary;
import jdk.jfr.Label;
import lombok.SneakyThrows;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.jupiter.api.*;

import java.net.URI;
import java.util.List;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class ProjectTests extends BaseShapeTreeTest {

        public ProjectTests() {
            // Call BaseShapeTreeTest constructor
            super();
        }

        private static RequestMatchingFixtureDispatcher dispatcher = null;

        @BeforeAll
        static void beforeAll() {
            dispatcher = new RequestMatchingFixtureDispatcher(List.of(
                    new DispatcherEntry(List.of("shapetrees/medical-record-shapetree-ttl"), "GET", "/static/shapetrees/medical-record/shapetree", null)
            ));
        }

    @Order(1)
    @SneakyThrows
    @Test
    @Label("Discover Unmanaged Resource")
    void discoverUnmanagedResource() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        // Check to see if a locator is associated with the target resource
        this.shapeTreeClient.discoverShapeTree(ShapeTreeContext, "project-resource");

        // ensure that no shape tree locator is returned with the resource

    }
}
