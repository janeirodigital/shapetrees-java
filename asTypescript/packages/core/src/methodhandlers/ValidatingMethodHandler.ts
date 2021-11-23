// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.core.methodhandlers
import { DocumentResponse } from '../DocumentResponse';
import { ShapeTreeException } from '../exceptions/ShapeTreeException';
import { ShapeTreeRequest } from '../ShapeTreeRequest';
import * as Optional from 'java/util';

export interface ValidatingMethodHandler {

  validateRequest(shapeTreeRequest: ShapeTreeRequest): Optional<DocumentResponse> /* throws ShapeTreeException */;
}
