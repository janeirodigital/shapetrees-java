// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.core
import { ShapeTreeException } from './exceptions/ShapeTreeException';
import * as Objects from 'java/util';

export class ShapeTreeReference {

   readonly referenceUrl: URL;

   readonly shapePath: string;

   readonly predicate: URL;

  public constructor(referenceUrl: URL, shapePath: string, predicate: URL) /* throws ShapeTreeException */ {
    this.referenceUrl = Objects.requireNonNull(referenceUrl);
    if (shapePath === null && predicate === null) {
      throw new ShapeTreeException(500, "Shape tree reference must have either a shape path or a predicate");
    } else if (shapePath != null && predicate != null) {
      throw new ShapeTreeException(500, "Shape tree reference cannot have a shape path and a predicate");
    } else if (shapePath != null) {
      this.shapePath = shapePath;
      this.predicate = null;
    } else {
      this.predicate = predicate;
      this.shapePath = null;
    }
  }

  public viaShapePath(): boolean {
    return shapePath != null;
  }

  public viaPredicate(): boolean {
    return predicate != null;
  }

  public getReferenceUrl(): URL {
    return this.referenceUrl;
  }

  public getShapePath(): string {
    return this.shapePath;
  }

  public getPredicate(): URL {
    return this.predicate;
  }
}
