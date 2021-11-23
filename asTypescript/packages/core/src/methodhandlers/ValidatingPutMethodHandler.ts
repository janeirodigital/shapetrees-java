// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.core.methodhandlers
import * as core from 'com/janeirodigital/shapetrees';
import { ShapeTreeException } from '../exceptions/ShapeTreeException';
import { RequestHelper } from '../helpers/RequestHelper';
import { ShapeTreeContext } from '../ShapeTreeContext';
import * as Optional from 'java/util';
import { AbstractValidatingMethodHandler } from './AbstractValidatingMethodHandler';
import { ValidatingMethodHandler } from './ValidatingMethodHandler';

export class ValidatingPutMethodHandler extends AbstractValidatingMethodHandler implements ValidatingMethodHandler {

  public constructor(resourceAccessor: ResourceAccessor) {
    super(resourceAccessor);
  }

  override public validateRequest(shapeTreeRequest: ShapeTreeRequest): Optional<DocumentResponse> /* throws ShapeTreeException */ {
    let shapeTreeContext: ShapeTreeContext = RequestHelper.buildContextFromRequest(shapeTreeRequest);
    let targetInstance: ManageableInstance = this.resourceAccessor.getInstance(shapeTreeContext, shapeTreeRequest.getUrl());
    if (targetInstance.wasRequestForManager()) {
      // Target resource is for shape tree manager, manage shape trees to plant and/or unplant
      return Optional.of(this.requestHandler.manageShapeTree(targetInstance, shapeTreeRequest));
    } else {
      let targetResource: ManageableResource = targetInstance.getManageableResource();
      shapeTreeRequest.setResourceType(RequestHelper.determineResourceType(shapeTreeRequest, targetInstance));
      if (targetResource.isExists()) {
        // The target resource already exists
        if (targetInstance.isManaged()) {
          // If it is managed by a shape tree the update must be validated
          return this.requestHandler.updateShapeTreeInstance(targetInstance, shapeTreeContext, shapeTreeRequest);
        }
      } else {
        // The target resource doesn't exist
        let parentInstance: ManageableInstance = this.resourceAccessor.getInstance(shapeTreeContext, targetResource.getParentContainerUrl());
        if (parentInstance.isManaged()) {
          // If the parent container is managed by a shape tree, the resource to create must be validated
          return this.requestHandler.createShapeTreeInstance(targetInstance, parentInstance, shapeTreeRequest, targetResource.getName());
        }
      }
    }
    // Reaching this point means validation was not necessary
    // Pass the request along with no validation
    return Optional.empty();
  }
}
