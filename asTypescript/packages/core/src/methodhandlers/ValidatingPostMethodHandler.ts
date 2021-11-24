// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.core.methodhandlers
import { ShapeTreeRequest } from '../ShapeTreeRequest';
import { ShapeTreeContext } from '../ShapeTreeContext';
import { ManageableInstance } from '../ManageableInstance';
import { DocumentResponse } from '../DocumentResponse';
import { ResourceAccessor } from '../ResourceAccessor';
import { ShapeTreeException } from '../exceptions/ShapeTreeException';
import { RequestHelper } from '../helpers/RequestHelper';
import { HttpHeaders } from '../enums/HttpHeaders';
import * as UUID from 'java/util';
import { AbstractValidatingMethodHandler } from './AbstractValidatingMethodHandler';
import { ValidatingMethodHandler } from './ValidatingMethodHandler';

export class ValidatingPostMethodHandler extends AbstractValidatingMethodHandler implements ValidatingMethodHandler {

  public constructor(resourceAccessor: ResourceAccessor) {
    super(resourceAccessor);
  }

  override public validateRequest(shapeTreeRequest: ShapeTreeRequest): DocumentResponse | null /* throws ShapeTreeException */ {
    let shapeTreeContext: ShapeTreeContext = RequestHelper.buildContextFromRequest(shapeTreeRequest);
    // Look up the target container for the POST. Error if it doesn't exist, or is a manager resource
    let targetContainer: ManageableInstance = this.resourceAccessor.getInstance(shapeTreeContext, shapeTreeRequest.getUrl());
    // Get resource name from the slug or default to UUID
    let proposedName: string = shapeTreeRequest.getHeaders().firstValue(HttpHeaders.SLUG.getValue()).orElse(UUID.randomUUID().toString());
    // If the parent container is managed by a shape tree, the proposed resource being posted must be
    // validated against the parent tree.
    if (targetContainer.isManaged()) {
      shapeTreeRequest.setResourceType(RequestHelper.determineResourceType(shapeTreeRequest, targetContainer));
      return this.requestHandler.createShapeTreeInstance(targetContainer, targetContainer, shapeTreeRequest, proposedName);
    }
    // Reaching this point means validation was not necessary
    // Pass the request along with no validation
    return Optional.empty();
  }
}
