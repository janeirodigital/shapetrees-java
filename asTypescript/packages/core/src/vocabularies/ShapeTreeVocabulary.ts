// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.core.vocabularies
export const class ShapeTreeVocabulary {

  private constructor() {
  }

   public static readonly HAS_ROOT_ASSIGNMENT: string = Namespaces.SHAPETREE + "hasRootAssignment";

   public static readonly MANAGES_RESOURCE: string = Namespaces.SHAPETREE + "manages";

   public static readonly ASSIGNS_SHAPE_TREE: string = Namespaces.SHAPETREE + "assigns";

   public static readonly REFERENCES_SHAPE_TREE: string = Namespaces.SHAPETREE + "referencesShapeTree";

   public static readonly SHAPETREE_MANAGER: string = Namespaces.SHAPETREE + "Manager";

   public static readonly SHAPETREE_ASSIGNMENT: string = Namespaces.SHAPETREE + "Assignment";

   public static readonly EXPECTS_TYPE: string = Namespaces.SHAPETREE + "expectsType";

   public static readonly REFERENCES: string = Namespaces.SHAPETREE + "references";

   public static readonly VIA_SHAPE_PATH: string = Namespaces.SHAPETREE + "viaShapePath";

   public static readonly VIA_PREDICATE: string = Namespaces.SHAPETREE + "viaPredicate";

   public static readonly CONTAINS: string = Namespaces.SHAPETREE + "contains";

   public static readonly HAS_ASSIGNMENT: string = Namespaces.SHAPETREE + "hasAssignment";

   public static readonly SHAPE: string = Namespaces.SHAPETREE + "shape";

   public static readonly FOCUS_NODE: string = Namespaces.SHAPETREE + "focusNode";

   public static readonly CONTAINER: string = Namespaces.SHAPETREE + "Container";

   public static readonly RESOURCE: string = Namespaces.SHAPETREE + "Resource";

   public static readonly NON_RDF_RESOURCE: string = Namespaces.SHAPETREE + "NonRDFResource";
}
