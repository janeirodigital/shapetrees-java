// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.client.http
import { ShapeTreeException } from '@shapetrees/exceptions/ShapeTreeException';
import { HttpClientFactory } from './HttpClientFactory';

export abstract class HttpClientFactoryManager {

  @Setter(onMethod_ = { @Synchronized })
   private static factory: HttpClientFactory;

  private constructor() {
    throw new IllegalStateException("Utility class");
  }

  // @Synchronized
  public static getFactory(): HttpClientFactory /* throws ShapeTreeException */ {
    if (factory === null) {
      throw new ShapeTreeException(500, "Must provide a valid HTTP client factory");
    }
    return HttpClientFactoryManager.factory;
  }
}
