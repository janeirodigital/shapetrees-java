// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.core
import { ShapeTreeAssignment } from './ShapeTreeAssignment';
import { ShapeTree } from './ShapeTree';

export class ValidationResult {

   private valid: boolean;

   private validatingShapeTree: ShapeTree;

   private matchingShapeTree: ShapeTree;

   private managingAssignment: ShapeTreeAssignment;

   private matchingFocusNode: URL;

   private message: string;

  public isValid(): boolean {
    return (this.valid != null && this.valid);
  }

  public constructor(valid: boolean, message: string) {
    this.valid = valid;
    this.message = message;
    this.validatingShapeTree = null;
    this.matchingShapeTree = null;
    this.managingAssignment = null;
    this.matchingFocusNode = null;
  }

  public constructor(valid: boolean, validatingShapeTree: ShapeTree) {
    this.valid = valid;
    this.message = null;
    this.validatingShapeTree = validatingShapeTree;
    this.matchingShapeTree = null;
    this.managingAssignment = null;
    this.matchingFocusNode = null;
  }

  public constructor(valid: boolean, validatingShapeTree: ShapeTree, message: string) {
    this.valid = valid;
    this.message = message;
    this.validatingShapeTree = validatingShapeTree;
    this.matchingShapeTree = null;
    this.managingAssignment = null;
    this.matchingFocusNode = null;
  }

  public constructor(valid: boolean, validatingShapeTree: ShapeTree, matchingFocusNode: URL) {
    this.valid = valid;
    this.message = null;
    this.validatingShapeTree = validatingShapeTree;
    this.matchingShapeTree = null;
    this.managingAssignment = null;
    this.matchingFocusNode = matchingFocusNode;
  }

  public constructor(valid: boolean, validatingShapeTree: ShapeTree, matchingShapeTree: ShapeTree, matchingFocusNode: URL) {
    this.valid = valid;
    this.message = null;
    this.validatingShapeTree = validatingShapeTree;
    this.matchingShapeTree = matchingShapeTree;
    this.managingAssignment = null;
    this.matchingFocusNode = matchingFocusNode;
  }

  public constructor(valid: boolean, validatingShapeTree: ShapeTree, matchingShapeTree: ShapeTree, managingAssignment: ShapeTreeAssignment, matchingFocusNode: URL, message: string) {
    this.valid = valid;
    this.validatingShapeTree = validatingShapeTree;
    this.matchingShapeTree = matchingShapeTree;
    this.managingAssignment = managingAssignment;
    this.matchingFocusNode = matchingFocusNode;
    this.message = message;
  }

  public getValid(): boolean {
    return this.valid;
  }

  public getValidatingShapeTree(): ShapeTree {
    return this.validatingShapeTree;
  }

  public getMatchingShapeTree(): ShapeTree {
    return this.matchingShapeTree;
  }

  public getManagingAssignment(): ShapeTreeAssignment {
    return this.managingAssignment;
  }

  public getMatchingFocusNode(): URL {
    return this.matchingFocusNode;
  }

  public getMessage(): string {
    return this.message;
  }
}
