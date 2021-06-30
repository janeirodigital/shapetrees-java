package com.janeirodigital.shapetrees.core;

import lombok.Getter;
import lombok.Setter;

import java.net.URI;
import java.util.List;
import java.util.Map;

@Getter @Setter
public class ShapeTreeResource {
    private URI uri;
    private String body;
    private boolean exists;
    private boolean container;
    private boolean metadata;
    private Map<String, List<String>> attributes;

    public String getFirstAttributeValue(String attributeName) {
        if (!this.attributes.containsKey(attributeName)) return null;

        return this.attributes.get(attributeName).stream().findFirst().orElse(null);
    }


}
