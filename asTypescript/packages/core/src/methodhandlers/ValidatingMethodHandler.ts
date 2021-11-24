// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.core.methodhandlers
import { DocumentResponse } from '../DocumentResponse';
import { ShapeTreeException } from '../exceptions/ShapeTreeException';
import { ShapeTreeRequest } from '../ShapeTreeRequest';

export interface ValidatingMethodHandler {

  validateRequest(shapeTreeRequest: ShapeTreeRequest): DocumentResponse | null /* throws ShapeTreeException */;
}
