// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.tests.clienthttp
import { DocumentResponse } from '@shapetrees/core/src/DocumentResponse';
import { DispatcherEntry } from '../fixtures/DispatcherEntry';
import { RequestMatchingFixtureDispatcher } from '../fixtures/RequestMatchingFixtureDispatcher';
import * as Label from 'jdk/jfr';
import * as MockWebServer from 'okhttp3/mockwebserver';
import * as Arrays from 'java/util';
import { AbstractHttpClientTests } from './AbstractHttpClientTests';

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
export class AbstractHttpClientTypeTests extends AbstractHttpClientTests {

   private static dispatcher: RequestMatchingFixtureDispatcher = null;

  public constructor() {
    // Call AbstractHttpClientTests constructor
    super();
  }

  // @BeforeEach
  initializeDispatcher(): void {
    let dispatcherList: Array = new Array();
    dispatcherList.add(new DispatcherEntry(List.of("type/containers-container"), "GET", "/containers/", null));
    dispatcherList.add(new DispatcherEntry(List.of("type/containers-container-manager"), "GET", "/containers/.shapetree", null));
    dispatcherList.add(new DispatcherEntry(List.of("type/resources-container"), "GET", "/resources/", null));
    dispatcherList.add(new DispatcherEntry(List.of("type/resources-container-manager"), "GET", "/resources/.shapetree", null));
    dispatcherList.add(new DispatcherEntry(List.of("type/non-rdf-resources-container"), "GET", "/non-rdf-resources/", null));
    dispatcherList.add(new DispatcherEntry(List.of("type/non-rdf-resources-container-manager"), "GET", "/non-rdf-resources/.shapetree", null));
    dispatcherList.add(new DispatcherEntry(List.of("shapetrees/type-shapetree-ttl"), "GET", "/static/shapetrees/type/shapetree", null));
    dispatcher = new RequestMatchingFixtureDispatcher(dispatcherList);
  }

  // @SneakyThrows, @Test, @Label("Create container when only containers are allowed")
  createContainer(): void {
    let server: MockWebServer = new MockWebServer();
    server.setDispatcher(dispatcher);
    // Add fixture to handle successful POST response
    dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("type/valid-container-create-response"), "POST", "/containers/valid-container/", null));
    dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/containers/valid-container/.shapetree", null));
    let response: DocumentResponse;
    // Provide target shape tree
    response = shapeTreeClient.postManagedInstance(context, toUrl(server, "/containers/"), null, Arrays.asList(toUrl(server, "/static/shapetrees/type/shapetree#ContainerTree")), "valid-container", true, null, TEXT_TURTLE);
    Assertions.assertEquals(201, response.getStatusCode());
    // Do not provide target shape tree
    response = shapeTreeClient.postManagedInstance(context, toUrl(server, "/containers/"), null, null, "valid-container", true, null, TEXT_TURTLE);
    Assertions.assertEquals(201, response.getStatusCode());
  }

  // @SneakyThrows, @Test, @Label("Fail to create resource when only containers are allowed")
  failToCreateNonContainer(): void {
    let server: MockWebServer = new MockWebServer();
    server.setDispatcher(dispatcher);
    let response: DocumentResponse;
    // Provide target shape tree for a resource when container shape tree is expected
    response = shapeTreeClient.postManagedInstance(context, toUrl(server, "/containers/"), null, Arrays.asList(toUrl(server, "/static/shapetrees/type/shapetree#ResourceTree")), "invalid-resource", false, null, TEXT_TURTLE);
    Assertions.assertEquals(422, response.getStatusCode());
    // Provide target shape tree for a container even though what's being sent is a resource
    response = shapeTreeClient.postManagedInstance(context, toUrl(server, "/containers/"), null, Arrays.asList(toUrl(server, "/static/shapetrees/type/shapetree#ContainerTree")), "invalid-resource", false, null, TEXT_TURTLE);
    Assertions.assertEquals(422, response.getStatusCode());
    // Don't provide a target shape tree at all
    response = shapeTreeClient.postManagedInstance(context, toUrl(server, "/containers/"), null, null, "invalid-resource", false, null, TEXT_TURTLE);
    Assertions.assertEquals(422, response.getStatusCode());
  }

  // @SneakyThrows, @Test, @Label("Create resource when only resources are allowed")
  createResource(): void {
    let server: MockWebServer = new MockWebServer();
    server.setDispatcher(dispatcher);
    // Add fixture to handle successful POST response
    dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("type/valid-resource-create-response"), "POST", "/resources/valid-resource", null));
    dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/resources/valid-resource.shapetree", null));
    let response: DocumentResponse;
    // Provide target shape tree
    response = shapeTreeClient.postManagedInstance(context, toUrl(server, "/resources/"), null, Arrays.asList(toUrl(server, "/static/shapetrees/type/shapetree#ResourceTree")), "valid-resource", false, null, TEXT_TURTLE);
    Assertions.assertEquals(201, response.getStatusCode());
    // Do not provide target shape tree
    response = shapeTreeClient.postManagedInstance(context, toUrl(server, "/resources/"), null, null, "valid-resource", false, null, TEXT_TURTLE);
    Assertions.assertEquals(201, response.getStatusCode());
  }

  // @SneakyThrows, @Test, @Label("Fail to create container when only resources are allowed")
  failToCreateNonResource(): void {
    let server: MockWebServer = new MockWebServer();
    server.setDispatcher(dispatcher);
    let response: DocumentResponse;
    // Provide target shape tree for a container when resource shape tree is expected
    response = shapeTreeClient.postManagedInstance(context, toUrl(server, "/resources/"), null, Arrays.asList(toUrl(server, "/static/shapetrees/type/shapetree#ContainerTree")), "invalid-container", true, null, TEXT_TURTLE);
    Assertions.assertEquals(422, response.getStatusCode());
    // Provide target shape tree for a resource even though what's being sent is a container
    response = shapeTreeClient.postManagedInstance(context, toUrl(server, "/resources/"), null, Arrays.asList(toUrl(server, "/static/shapetrees/type/shapetree#ResourceTree")), "invalid-container", true, null, TEXT_TURTLE);
    Assertions.assertEquals(422, response.getStatusCode());
    // Don't provide a target shape tree at all
    response = shapeTreeClient.postManagedInstance(context, toUrl(server, "/resources/"), null, null, "invalid-container", true, null, TEXT_TURTLE);
    Assertions.assertEquals(422, response.getStatusCode());
  }

  // @SneakyThrows, @Test, @Label("Create non-rdf resource when only non-rdf resources are allowed")
  createNonRDFResource(): void {
    let server: MockWebServer = new MockWebServer();
    server.setDispatcher(dispatcher);
    // Add fixture to handle successful POST response
    dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("type/valid-non-rdf-resource-create-response"), "POST", "/non-rdf-resources/valid-non-rdf-resource", null));
    // TODO: Test: should this fail? should it have already failed?
    dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/non-rdf-resources/valid-non-rdf-resource.shapetree", null));
    let response: DocumentResponse;
    response = shapeTreeClient.postManagedInstance(context, toUrl(server, "/non-rdf-resources/"), null, Arrays.asList(toUrl(server, "/static/shapetrees/type/shapetree#NonRDFResourceTree")), "valid-non-rdf-resource", false, null, "application/octet-stream");
    Assertions.assertEquals(201, response.getStatusCode());
    response = shapeTreeClient.postManagedInstance(context, toUrl(server, "/non-rdf-resources/"), null, null, "valid-non-rdf-resource", false, null, "application/octet-stream");
    Assertions.assertEquals(201, response.getStatusCode());
  }

  // @SneakyThrows, @Test, @Label("Fail to create resource when only non-rdf resources are allowed")
  failToCreateNonRDFResource(): void {
    let server: MockWebServer = new MockWebServer();
    server.setDispatcher(dispatcher);
    let response: DocumentResponse;
    // Provide target shape tree for a resource when non-rdf-resource shape tree is expected
    response = shapeTreeClient.postManagedInstance(context, toUrl(server, "/non-rdf-resources/"), null, Arrays.asList(toUrl(server, "/static/shapetrees/type/shapetree#ResourceTree")), "invalid-non-rdf-resource", false, null, TEXT_TURTLE);
    Assertions.assertEquals(422, response.getStatusCode());
    // Provide target shape tree for a non-rdf-resource even though what's being sent is a resource
    response = shapeTreeClient.postManagedInstance(context, toUrl(server, "/non-rdf-resources/"), null, Arrays.asList(toUrl(server, "/static/shapetrees/type/shapetree#NonRDFResourceTree")), "invalid-non-rdf-resource", false, null, TEXT_TURTLE);
    Assertions.assertEquals(422, response.getStatusCode());
    // Don't provide a target shape tree at all
    response = shapeTreeClient.postManagedInstance(context, toUrl(server, "/non-rdf-resources/"), null, null, "invalid-non-rdf-resource", false, null, TEXT_TURTLE);
    Assertions.assertEquals(422, response.getStatusCode());
  }
}
