package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import org.jetbrains.annotations.NotNull;

import java.net.URI;
import java.util.Optional;

public interface ShapeTreeRequest {
    String getMethod();
    URI getURI();
    ResourceAttributes getHeaders();
    @NotNull
    ResourceAttributes getLinkHeaders();
    @NotNull
    Optional<String> getHeaderValue(String header);
    @NotNull
    Optional<String> getBody();
    @NotNull
    String expectContentType() throws ShapeTreeException;
    @NotNull
    ShapeTreeResourceType getResourceType();
    void setResourceType(@NotNull ShapeTreeResourceType resourceType);
}
