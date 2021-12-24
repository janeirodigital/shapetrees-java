// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.tests.fixtures
import * as MockWebServer from 'okhttp3/mockwebserver';
import * as MalformedURLException from 'java/net';

export class MockWebServerHelper {

  public static toUrl(server: MockWebServer, path: string): URL /* throws MalformedURLException */ {
    return new URL(server.url(path).toString());
  }
}
