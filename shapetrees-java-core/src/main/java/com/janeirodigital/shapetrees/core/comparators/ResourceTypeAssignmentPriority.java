package com.janeirodigital.shapetrees.core.comparators;

import com.janeirodigital.shapetrees.core.ManageableInstance;
import lombok.SneakyThrows;

import java.io.Serializable;
import java.util.Comparator;

public class
ResourceTypeAssignmentPriority implements Comparator<ManageableInstance>, Serializable {

    // Used for sorting by shape tree resource type with the following order
    // 1. Containers
    // 2. Resources
    // 3. Non-RDF Resources

    @SneakyThrows
    public int compare(ManageableInstance a, ManageableInstance b) {
        return a.getManageableResource().getResourceType().compareTo(b.getManageableResource().getResourceType());
    }

}