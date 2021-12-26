// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.core.methodhandlers
import { ResourceAccessor } from '../ResourceAccessor';
import { ShapeTreeRequestHandler } from '../ShapeTreeRequestHandler';

/**
 * Abstract class providing reusable functionality to different method handlers
 */
export abstract class AbstractValidatingMethodHandler {

   private static readonly DELETE: string = "DELETE";

   protected readonly resourceAccessor: ResourceAccessor;

   protected readonly requestHandler: ShapeTreeRequestHandler;

  protected constructor(resourceAccessor: ResourceAccessor) {
    this.resourceAccessor = resourceAccessor;
    this.requestHandler = new ShapeTreeRequestHandler(resourceAccessor);
  }
}
