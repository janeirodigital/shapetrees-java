package com.janeirodigital.shapetrees;

import com.janeirodigital.shapetrees.enums.Namespaces;

public class ShapeTreeVocabulary {
    public static final String HAS_SHAPE_TREE_ROOT = Namespaces.SHAPETREE_NAMESPACE.getValue() + "hasRootShapeTree";
    public static final String HAS_SHAPE_TREE_INSTANCE_PATH = Namespaces.SHAPETREE_NAMESPACE.getValue() + "hasShapeTreeInstancePath";
    public static final String HAS_SHAPE_TREE_INSTANCE_ROOT = Namespaces.SHAPETREE_NAMESPACE.getValue() + "hasShapeTreeInstanceRoot";
    public static final String HAS_SHAPE_TREE = Namespaces.SHAPETREE_NAMESPACE.getValue() + "hasShapeTree";
    public static final String HAS_SHAPE_TREE_LOCATOR = Namespaces.SHAPETREE_NAMESPACE.getValue() + "hasShapeTreeLocator";
    public static final String EXPECTS_TYPE = Namespaces.SHAPETREE_NAMESPACE.getValue() + "expectsType";
    public static final String REFERENCES = Namespaces.SHAPETREE_NAMESPACE.getValue() + "references";
    public static final String CONTAINS = Namespaces.SHAPETREE_NAMESPACE.getValue() + "contains";
    public static final String TRAVERSE_VIA_SHAPE_PATH = Namespaces.SHAPETREE_NAMESPACE.getValue() + "traverseViaShapePath";
    public static final String VALIDATED_BY = Namespaces.SHAPETREE_NAMESPACE.getValue() + "validatedBy";
    public static final String MATCHES_URI_TEMPLATE = Namespaces.SHAPETREE_NAMESPACE.getValue() + "matchesUriTemplate";
    public static final String ALLOW_RESOURCES = Namespaces.SHAPETREE_NAMESPACE.getValue() + "AllowResources";
    public static final String ALLOW_CONTAINERS = Namespaces.SHAPETREE_NAMESPACE.getValue() + "AllowContainers";
    public static final String ALLOW_NON_RDF_SOURCES = Namespaces.SHAPETREE_NAMESPACE.getValue() + "AllowNonRDFSources";
    public static final String ALLOW_ALL = Namespaces.SHAPETREE_NAMESPACE.getValue() + "AllowAll";
    public static final String ALLOW_NONE = Namespaces.SHAPETREE_NAMESPACE.getValue() + "AllowNone";

}
