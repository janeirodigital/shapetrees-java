package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;

import java.net.URI;
import java.util.List;
import java.util.Map;

public interface ResourceAccessor {
    ShapeTreeResource getResource(ShapeTreeContext context, URI resourceURI) throws ShapeTreeException;
    ShapeTreeResource createResource(ShapeTreeContext context, URI resourceURI, Map<String, List<String>> headers, String body, String contentType) throws ShapeTreeException;
    ShapeTreeResource updateResource(ShapeTreeContext context, ShapeTreeResource updatedResource) throws ShapeTreeException;
}
