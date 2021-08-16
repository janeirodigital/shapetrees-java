package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;

import java.net.URI;
import java.util.List;
import java.util.Map;

public interface ShapeTreeRequest {
    String getMethod();
    URI getURI();
    Map<String, List<String>> getHeaders();
    Map<String, List<String>> getLinkHeaders();
    List<String> getHeaderValues(String header);
    String getHeaderValue(String header);
    String getBody();
    String getContentType();
    ShapeTreeResourceType getResourceType();
    void setResourceType(ShapeTreeResourceType resourceType);
}
