package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;
import lombok.Getter;
import lombok.Setter;

import java.net.URI;
import java.util.List;
import java.util.Map;

@Getter @Setter
public class ShapeTreeResource {
    private URI uri;
    private URI associatedUri;
    private String name;
    private String body;
    ShapeTreeResourceType type;
    private boolean exists;
    private boolean container;
    private boolean metadata;
    private boolean managed;
    private HttpClientHeaders attributes;

    public String getFirstAttributeValue(String attributeName) {
        if (!this.attributes.containsKey(attributeName)) return null;

        return this.attributes.get(attributeName).stream().findFirst().orElse(null);
    }


}
