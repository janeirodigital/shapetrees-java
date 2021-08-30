package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;

import java.net.URI;
import java.util.List;

public interface ShapeTreeRequest {
    String getMethod();
    URI getURI();
    HttpHeaders getHeaders();
    HttpHeaders getLinkHeaders();
    List<String> getHeaderValues(String header);
    String getHeaderValue(String header);
    String getBody();
    String getContentType();
    ShapeTreeResourceType getResourceType();
    void setResourceType(ShapeTreeResourceType resourceType);
}
