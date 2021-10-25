package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;

import java.net.URL;
import java.util.List;

public interface ShapeTreeRequest {
    String getMethod();
    URL getURL();
    ResourceAttributes getHeaders();
    ResourceAttributes getLinkHeaders();
    List<String> getHeaderValues(String header);
    String getHeaderValue(String header);
    String getBody();
    String getContentType();
    ShapeTreeResourceType getResourceType();
    void setResourceType(ShapeTreeResourceType resourceType);
}
