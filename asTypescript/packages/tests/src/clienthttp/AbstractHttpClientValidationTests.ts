// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.tests.clienthttp
import { DocumentResponse } from '@shapetrees/core/src/DocumentResponse';
import { DispatcherEntry } from '../fixtures/DispatcherEntry';
import { RequestMatchingFixtureDispatcher } from '../fixtures/RequestMatchingFixtureDispatcher';
import * as MockWebServer from 'okhttp3/mockwebserver';
import * as Assertions from 'org/junit/jupiter/api';
import * as BeforeEach from 'org/junit/jupiter/api';
import * as DisplayName from 'org/junit/jupiter/api';
import * as Test from 'org/junit/jupiter/api';
import * as Arrays from 'java/util';
import { AbstractHttpClientTests } from './AbstractHttpClientTests';

export class AbstractHttpClientValidationTests extends AbstractHttpClientTests {

   private static dispatcher: RequestMatchingFixtureDispatcher = null;

  public constructor() {
    // Call AbstractHttpClient constructor
    super();
  }

  // @BeforeEach
  initializeDispatcher(): void {
    // For this set of tests, we reinitialize the dispatcher set for every test, because almost every test needs a
    // slightly different context. Consequently, we could either modify the state from test to test (which felt a
    // little dirty as we couldn't run tests standalone, or set the context for each test (which we're doing)
    let dispatcherList: Array = new Array();
    dispatcherList.add(new DispatcherEntry(List.of("validation/container-1"), "GET", "/data/container-1/", null));
    dispatcherList.add(new DispatcherEntry(List.of("shapetrees/containment-shapetree-ttl"), "GET", "/static/shapetrees/validation/shapetree", null));
    dispatcherList.add(new DispatcherEntry(List.of("schemas/containment-shex"), "GET", "/static/shex/validation/shex", null));
    dispatcher = new RequestMatchingFixtureDispatcher(dispatcherList);
  }

  // @SneakyThrows, @Test, @DisplayName("Create resource - two containing trees, two shapes, two nodes")
  validateTwoContainsTwoShapesTwoNodes(): void {
    let server: MockWebServer = new MockWebServer();
    server.setDispatcher(dispatcher);
    dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("validation/container-1-twocontains-manager"), "GET", "/data/container-1/.shapetree", null));
    dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("validation/resource-1-create-response"), "POST", "/data/container-1/resource-1", null));
    dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/data/container-1/", null));
    dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/data/container-1/resource-1.shapetree", null));
    let targetResource: URL = toUrl(server, "/data/container-1/");
    let targetShapeTrees: Array<URL> = Arrays.asList(toUrl(server, "/static/shapetrees/validation/shapetree#AttributeTree"), toUrl(server, "/static/shapetrees/validation/shapetree#ElementTree"));
    let focusNodes: Array<URL> = Arrays.asList(toUrl(server, "/data/container-1/resource-1#resource"), toUrl(server, "/data/container-1/resource-1#element"));
    // Plant the data repository on newly created data container
    let response: DocumentResponse = this.shapeTreeClient.postManagedInstance(context, targetResource, focusNodes, targetShapeTrees, "resource-1", false, getResource1BodyString(), "text/turtle");
    Assertions.assertEquals(201, response.getStatusCode());
  }

  // @SneakyThrows, @Test, @DisplayName("Create resource - two containing trees of same tree")
  validateTwoContainsSameContainingTree(): void {
    let server: MockWebServer = new MockWebServer();
    server.setDispatcher(dispatcher);
    dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("validation/container-1-samecontains-manager"), "GET", "/data/container-1/.shapetree", null));
    dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("validation/resource-1-create-response"), "POST", "/data/container-1/resource-1", null));
    dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/data/container-1/", null));
    dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/data/container-1/resource-1.shapetree", null));
    // Validate multiple contains, same shape tree, same node
    let targetResource: URL = toUrl(server, "/data/container-1/");
    let targetShapeTrees: Array<URL> = Arrays.asList(toUrl(server, "/static/shapetrees/validation/shapetree#ChildTree"));
    let focusNodes: Array<URL> = Arrays.asList(toUrl(server, "/data/container-1/resource-1#resource"));
    // Plant the data repository on newly created data container
    let response: DocumentResponse = this.shapeTreeClient.postManagedInstance(context, targetResource, focusNodes, targetShapeTrees, "resource-1", false, getResource1BodyString(), "text/turtle");
    Assertions.assertEquals(201, response.getStatusCode());
  }

  // @SneakyThrows, @Test, @DisplayName("Fail to create - two containing trees and focus node issues")
  failToValidateTwoContainsWithBadFocusNodes(): void {
    let server: MockWebServer = new MockWebServer();
    server.setDispatcher(dispatcher);
    dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("validation/container-1-twocontains-manager"), "GET", "/data/container-1/.shapetree", null));
    dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("validation/resource-1-create-response"), "POST", "/data/container-1/resource-1", null));
    dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/data/container-1/", null));
    dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/data/container-1/resource-1.shapetree", null));
    let targetResource: URL = toUrl(server, "/data/container-1/");
    let targetShapeTrees: Array<URL> = Arrays.asList(toUrl(server, "/static/shapetrees/validation/shapetree#AttributeTree"), toUrl(server, "/static/shapetrees/validation/shapetree#ElementTree"));
    // Only one matching target focus node is provided
    let focusNodes: Array<URL> = Arrays.asList(toUrl(server, "/super/bad#node"));
    let response: DocumentResponse = this.shapeTreeClient.postManagedInstance(context, targetResource, focusNodes, targetShapeTrees, "resource-1", false, getResource1BodyString(), "text/turtle");
    Assertions.assertEquals(422, response.getStatusCode());
    // Multiple non-matching target focus nodes are provided
    focusNodes = Arrays.asList(toUrl(server, "/super/bad#node"), toUrl(server, "/data/container-1/resource-1#badnode"), toUrl(server, "/data/container-1/#badnode"));
    response = this.shapeTreeClient.postManagedInstance(context, targetResource, focusNodes, targetShapeTrees, "resource-1", false, getResource1BodyString(), "text/turtle");
    Assertions.assertEquals(422, response.getStatusCode());
    // Only one matching target focus node is provided when two are needed
    focusNodes = Arrays.asList(toUrl(server, "/data/container-1/resource-1#resource"));
    response = this.shapeTreeClient.postManagedInstance(context, targetResource, focusNodes, targetShapeTrees, "resource-1", false, getResource1BodyString(), "text/turtle");
    Assertions.assertEquals(422, response.getStatusCode());
  }

  /* TODO - Cannot execute this test predicatably as constituted when passing focus nodes from client. Need to test closer to shape tree validation
    @SneakyThrows
    @Test
    @DisplayName("Fail to validate created resource - two containing trees, target node unused")
    void failToValidateTwoContainsTargetNodeUnused() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("validation/container-1-twocontains-onenode-manager"), "GET", "/data/container-1/.shapetree", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("validation/resource-1-create-response"), "POST", "/data/container-1/resource-1", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/data/container-1/", null));
        dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/data/container-1/resource-1.shapetree", null));

        URL targetResource = toUrl(server, "/data/container-1/");
        List<URL> targetShapeTrees = Arrays.asList(toUrl(server, "/static/shapetrees/validation/shapetree#AttributeTree"),
                toUrl(server, "/static/shapetrees/validation/shapetree#ElementTree"));
        // Two target nodes are provided, but one of the nodes is matched twice, and the other isn't matched at all
        List<URL> focusNodes = Arrays.asList(toUrl(server, "/data/container-1/resource-1#resource"));

        DocumentResponse response = this.shapeTreeClient.postManagedInstance(context, targetResource, focusNodes, targetShapeTrees, "resource-1", false, getResource1BodyString(), "text/turtle");
        Assertions.assertEquals(422, response.getStatusCode());

    }
    */
  // @SneakyThrows, @Test, @DisplayName("Fail to create - two containing trees, bad target shape trees")
  failToValidateTwoContainsWithBadTargetShapeTrees(): void {
    let server: MockWebServer = new MockWebServer();
    server.setDispatcher(dispatcher);
    dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("validation/container-1-twocontains-manager"), "GET", "/data/container-1/.shapetree", null));
    dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("validation/resource-1-create-response"), "POST", "/data/container-1/resource-1", null));
    dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/data/container-1/", null));
    dispatcher.getConfiguredFixtures().add(new DispatcherEntry(List.of("http/201"), "POST", "/data/container-1/resource-1.shapetree", null));
    let targetResource: URL = toUrl(server, "/data/container-1/");
    let focusNodes: Array<URL> = Arrays.asList(toUrl(server, "/data/container-1/resource-1#resource"), toUrl(server, "/data/container-1/resource-1#element"));
    // Only one matching target shape tree is provided
    let targetShapeTrees: Array<URL> = Arrays.asList(toUrl(server, "/static/shapetrees/validation/shapetree#AttributeTree"));
    let response: DocumentResponse = this.shapeTreeClient.postManagedInstance(context, targetResource, focusNodes, targetShapeTrees, "resource-1", false, getResource1BodyString(), "text/turtle");
    Assertions.assertEquals(422, response.getStatusCode());
    // Multiple non-matching target focus nodes are provided
    targetShapeTrees = Arrays.asList(toUrl(server, "/static/shapetrees/validation/shapetree#OtherAttributeTree"), toUrl(server, "/static/shapetrees/validation/shapetree#OtherElementTree"));
    response = this.shapeTreeClient.postManagedInstance(context, targetResource, focusNodes, targetShapeTrees, "resource-1", false, getResource1BodyString(), "text/turtle");
    Assertions.assertEquals(422, response.getStatusCode());
    // One tree provided that isn't in either st:contains lists
    targetShapeTrees = Arrays.asList(toUrl(server, "/static/shapetrees/validation/shapetree#AttributeTree"), toUrl(server, "/static/shapetrees/validation/shapetree#StandaloneTree"));
    response = this.shapeTreeClient.postManagedInstance(context, targetResource, focusNodes, targetShapeTrees, "resource-1", false, getResource1BodyString(), "text/turtle");
    Assertions.assertEquals(422, response.getStatusCode());
  }

  private getResource1BodyString(): string {
    return "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \n" + "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \n" + "PREFIX xml: <http://www.w3.org/XML/1998/namespace> \n" + "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> \n" + "PREFIX ex: <http://www.example.com/ns/ex#> \n" + "\n" + "\n" + "<#resource> \n" + "    ex:name \"Some Development Task\" ; \n" + "    ex:created_at \"2021-04-04T20:15:47.000Z\"^^xsd:dateTime . \n" + "\n" + "<#element> \n" + "    ex:name \"Some element\" ; \n" + "    ex:description \"This is a description of an element\" ; \n" + "    ex:created_at \"2021-04-04T20:15:47.000Z\"^^xsd:dateTime . \n";
  }
}
