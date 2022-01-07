package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.enums.LinkRelation;

import java.util.List;

/**
 * Extension of {@link ResourceAttributes} used specifically for storage of
 * Link Relation attribute maps
 */
public class RelationAttributes extends ResourceAttributes {
    public RelationAttributes() { super(); }

    public List<String> getTypes() {
        return allValues(LinkRelation.TYPE.getValue());
    }

    public boolean containsType(String type) {
        return allValues(LinkRelation.TYPE.getValue()).contains(type);
    }

}