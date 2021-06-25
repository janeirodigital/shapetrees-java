package com.janeirodigital.shapetrees.core.models;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * ShapeTreeLocation
 *
 * Shape Trees, §3:  Each shape tree location identifies a shape tree associated with the managed resource,
 * the focus node for shape validation, and the information needed to navigate the physical hierarchy in
 * which that managed resource resides.
 * https://shapetrees.org/TR/specification/#locator
*/
@Getter @AllArgsConstructor
public class ShapeTreeLocation {

    private String shapeTree;               // Identifies the shape tree to be associated with the managed resource
    private String rootShapeTree;           // Marks the primary, or parent shape tree in a physical hierarchy
    private String rootShapeTreeInstance;   // Marks the primary, or parent shape tree instance in a physical hierarchy
    private String focusNode;               // Identifies the focus node for shape validation in the managed resource
    private String shape;                   // Identifies the shape to which focusNode must conform

}
