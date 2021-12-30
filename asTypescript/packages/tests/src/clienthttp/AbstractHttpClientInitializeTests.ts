// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.tests.clienthttp
import { HttpClient } from '@shapetrees/clienthttp/src/HttpClient';
import { RequestMatchingFixtureDispatcher } from '../fixtures/RequestMatchingFixtureDispatcher';
import { AbstractHttpClientTests } from './AbstractHttpClientTests';

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
export class AbstractHttpClientInitializeTests extends AbstractHttpClientTests {

   private static dispatcher: RequestMatchingFixtureDispatcher = null;

  public constructor() {
    // Call AbstractHttpClient constructor
    super();
  }

  // @Test, @SneakyThrows
  testNonValidatingHandler(): void {
    let client: HttpClient = this.factory.get(false);
    Assertions.assertNotNull(client);
  }

  // @Test, @SneakyThrows
  testValidatingHandler(): void {
    let client: HttpClient = this.factory.get(true);
    Assertions.assertNotNull(client);
  }
}
