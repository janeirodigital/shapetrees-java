package com.janeirodigital.shapetrees.core.comparators;

import com.janeirodigital.shapetrees.core.validation.ShapeTree;
import com.janeirodigital.shapetrees.core.validation.ShapeTreeFactory;
import lombok.SneakyThrows;

import java.io.Serializable;
import java.net.URL;
import java.util.Comparator;

public class
ShapeTreeContainsPriority implements Comparator<URL>, Serializable {
    // Used for sorting shape trees in st:contains by most to least strict
    @SneakyThrows
    @Override
    public int compare(URL stUrl1, URL stUrl2) {

        ShapeTree st1 = ShapeTreeFactory.getShapeTree(stUrl1);
        ShapeTree st2 = ShapeTreeFactory.getShapeTree(stUrl2);

        Integer st1Priority = 0;
        Integer st2Priority = 0;

        if (st1.getShape() != null) {
            st1Priority += 2;
        }
        if (st1.getLabel() != null) {
            st1Priority++;
        }

        // st:expectsType is required so it doesn't affect score priority

        if (st2.getShape() != null) {
            st2Priority += 2;
        }
        if (st2.getLabel() != null) {
            st2Priority++;
        }

        // Reversed to ensure ordering goes from most strict to least
        return Integer.compare(st2Priority, st1Priority);

    }

}
