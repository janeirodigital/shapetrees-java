package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;
import lombok.Getter;
import lombok.Setter;

import java.net.URI;

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
    private ResourceAttributes attributes;
}
