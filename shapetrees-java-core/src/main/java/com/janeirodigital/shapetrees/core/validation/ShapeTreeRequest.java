package com.janeirodigital.shapetrees.core.validation;

import com.janeirodigital.shapetrees.core.resources.ResourceAttributes;
import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;

import java.net.URL;
import java.util.List;

public interface ShapeTreeRequest {
    String getMethod();
    URL getUrl();
    ResourceAttributes getHeaders();
    ResourceAttributes getLinkHeaders();
    List<String> getHeaderValues(String header);
    String getHeaderValue(String header);
    String getBody();
    String getContentType();
    ShapeTreeResourceType getResourceType();
    void setResourceType(ShapeTreeResourceType resourceType);
}
