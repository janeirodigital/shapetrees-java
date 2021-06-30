package com.janeirodigital.shapetrees.core.models;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

/**
 * ShapeTreeMatch
 *
 * A ShapeTreeMatch is returned by the Find Matching Contained Shape Tree algorithm. It represents a matching
 * shape tree in a parent container's st:contains set, which a candidate resource has matched against.
 *
 * Shape Trees, §5.6: Find Matching Contained Shape Tree
 * https://shapetrees.org/TR/specification/#matching-contained-shapetree
*/
@Getter @Setter @NoArgsConstructor @AllArgsConstructor
public class ShapeTreeMatch {

    private String shapeTree;      // Matching shape tree in a parent container's st:contains set
    private String shape;          // Matching shape in shapeTree
    private String focusNode;      // Focus node in proposed graph that shape was validated against
    private Boolean alsoAllowed;   // Did not match a shape tree, but was allowed per st:alsoAllow

}