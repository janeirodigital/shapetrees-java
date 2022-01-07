package com.janeirodigital.shapetrees.okhttp;

import com.janeirodigital.shapetrees.core.ShapeTreeContext;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.tests.fixtures.RequestMatchingFixtureDispatcher;
import okhttp3.OkHttpClient;
import okhttp3.Response;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.jupiter.api.*;

import java.net.URL;

import static com.janeirodigital.shapetrees.okhttp.OkHttpShapeTreeClient.plant;
import static com.janeirodigital.shapetrees.tests.fixtures.DispatcherHelper.mockOnGet;
import static com.janeirodigital.shapetrees.tests.fixtures.DispatcherHelper.mockOnPut;
import static com.janeirodigital.shapetrees.tests.fixtures.MockWebServerHelper.toUrl;
import static org.junit.jupiter.api.Assertions.assertEquals;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class OkHttpShapeTreeClientProjectRecursiveTests {

    private static RequestMatchingFixtureDispatcher dispatcher = null;
    private static MockWebServer server;
    private static ShapeTreeContext context;
    private static OkHttpClient okHttpClient;

    @BeforeAll
    static void beforeAll() throws ShapeTreeException {
        context = new ShapeTreeContext(null);
        okHttpClient = OkHttpValidatingClientFactory.get();
    }

    @BeforeEach
    void beforeEach() {

        dispatcher = new RequestMatchingFixtureDispatcher();

        mockOnGet(dispatcher, "/static/shapetrees/project/shapetree", "shapetrees/project-shapetree-ttl");
        mockOnGet(dispatcher, "/static/shex/project/shex", "schemas/project-shex");
        mockOnGet(dispatcher, "/data/", "project/data-container");
        mockOnGet(dispatcher, "/data/projects/", "project/projects-container");
        mockOnGet(dispatcher, "/data/projects/project-1/", "project/project-1-container");
        mockOnGet(dispatcher, "/data/projects/project-1/milestone-3/", "project/milestone-3-container");
        mockOnGet(dispatcher, "/data/projects/project-1/milestone-3/task-48/", "project/task-48-container");
        mockOnGet(dispatcher, "/data/projects/project-1/milestone-3/task-6/", "project/task-6-container-no-contains");
        mockOnGet(dispatcher, "/data/projects/project-1/milestone-3/issue-2", "project/issue-2");
        mockOnGet(dispatcher, "/data/projects/project-1/milestone-3/issue-3", "project/issue-3");
        mockOnGet(dispatcher, "/data/projects/project-1/milestone-3/task-48/attachment-48", "project/attachment-48");
        mockOnGet(dispatcher, "/data/projects/project-1/milestone-3/task-48/random.png", "project/random-png");

        server = new MockWebServer();
        server.setDispatcher(dispatcher);
    }

    @Test
    @DisplayName("Recursively Plant Data Set")
    void plantDataRecursively() throws ShapeTreeException {
        mockOnPut(dispatcher, "/data/.shapetree", "http/201");
        mockOnPut(dispatcher, "/data/projects/.shapetree", "http/201");

        mockOnGet(dispatcher, "/data/.shapetree", "project/data-container-manager");
        mockOnGet(dispatcher, "/data/projects/.shapetree", "project/projects-container-manager");

        URL targetResource = toUrl(server, "/data/");
        URL targetShapeTree = toUrl(server, "/static/shapetrees/project/shapetree#DataRepositoryTree");
        URL focusNode = toUrl(server, "/data/#repository");

        // Plant the data collection recursively on already existing hierarchy
        Response response = plant(okHttpClient, context, targetResource, targetShapeTree, focusNode);
        assertEquals(201, response.code());
    }

    @Test
    @DisplayName("Recursively Plant Projects Collection")
    void plantProjectsRecursively() throws ShapeTreeException {
        // Add planted data set
        mockOnGet(dispatcher, "/data/.shapetree", "project/data-container-manager");
        mockOnGet(dispatcher, "/data/projects/.shapetree", "project/projects-container-manager");
        mockOnPut(dispatcher, "/data/projects/project-1/milestone-3/task-48/attachment-48.shapetree", "http/201");
        mockOnPut(dispatcher, "/data/projects/project-1/milestone-3/task-48/random.png.shapetree", "http/201");
        mockOnPut(dispatcher, "/data/projects/project-1/milestone-3/task-48/.shapetree", "http/201");
        mockOnPut(dispatcher, "/data/projects/project-1/milestone-3/task-6/.shapetree", "http/201");
        mockOnPut(dispatcher, "/data/projects/project-1/milestone-3/issue-3.shapetree", "http/201");
        mockOnPut(dispatcher, "/data/projects/project-1/milestone-3/issue-2.shapetree", "http/201");
        mockOnPut(dispatcher, "/data/projects/project-1/milestone-3/.shapetree", "http/201");
        mockOnPut(dispatcher, "/data/projects/project-1/.shapetree", "http/201");
        mockOnPut(dispatcher, "/data/projects/.shapetree", "http/201");

        URL targetResource = toUrl(server, "/data/projects/");
        URL targetShapeTree = toUrl(server, "/static/shapetrees/project/shapetree#ProjectCollectionTree");

        // Plant the projects collection recursively on already existing hierarchy
        Response response = plant(okHttpClient, context, targetResource, targetShapeTree, null);
        assertEquals(201, response.code());
    }

}
