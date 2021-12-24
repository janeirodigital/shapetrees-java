// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.tests.clienthttp
import { HttpClient } from '@shapetrees/clienthttp/src/HttpClient';
import { HttpClientFactory } from '@shapetrees/clienthttp/src/HttpClientFactory';
import { HttpShapeTreeClient } from '@shapetrees/clienthttp/src/HttpShapeTreeClient';
import { ShapeTreeException } from '@shapetrees/core/src/exceptions/ShapeTreeException';
import { ShapeTreeContext } from '@shapetrees/core/src/ShapeTreeContext';
import * as MockWebServer from 'okhttp3/mockwebserver';
import * as MalformedURLException from 'java/net';

export abstract class AbstractHttpClientTests {

   protected factory: HttpClientFactory = null;

   protected shapeTreeClient: HttpShapeTreeClient = new HttpShapeTreeClient();

   protected readonly context: ShapeTreeContext;

   protected fetcher: HttpClient;

   protected static TEXT_TURTLE: string = "text/turtle";

  public constructor() {
    this.context = new ShapeTreeContext(null);
  }

  public toUrl(server: MockWebServer, path: string): URL /* throws MalformedURLException */ {
    // TODO: duplicates com.janeirodigital.shapetrees.tests.fixtures.MockWebServerHelper.getURL;
    return new URL(server.url(path).toString());
  }

  protected skipShapeTreeValidation(b: boolean): void {
    try {
      this.fetcher = this.factory.get(!b);
    } catch (e) {
 if (e instanceof ShapeTreeException) {
      throw new Error(e);
    }
}
  }
}
