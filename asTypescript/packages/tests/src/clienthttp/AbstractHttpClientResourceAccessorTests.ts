// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.tests.clienthttp
import { HttpClient } from '@shapetrees/clienthttp/src/HttpClient';
import { HttpClientFactory } from '@shapetrees/clienthttp/src/HttpClientFactory';
import { HttpResourceAccessor } from '@shapetrees/clienthttp/src/HttpResourceAccessor';
import { ShapeTreeException } from '@shapetrees/core/src/exceptions/ShapeTreeException';
import { AbstractResourceAccessorTests } from '../AbstractResourceAccessorTests';
import * as MethodOrderer from 'org/junit/jupiter/api';
import * as TestMethodOrder from 'org/junit/jupiter/api';

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
export class AbstractHttpClientResourceAccessorTests extends AbstractResourceAccessorTests {

   protected httpResourceAccessor: HttpResourceAccessor = null;

   protected fetcher: HttpClient = null;

   protected factory: HttpClientFactory = null;

  public constructor() {
    super();
    this.resourceAccessor = new HttpResourceAccessor();
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
