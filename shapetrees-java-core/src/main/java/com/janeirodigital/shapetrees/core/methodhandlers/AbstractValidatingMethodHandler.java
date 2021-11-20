package com.janeirodigital.shapetrees.core.methodhandlers;

import com.janeirodigital.shapetrees.core.ResourceAccessor;
import com.janeirodigital.shapetrees.core.ShapeTreeRequestHandler;
import lombok.extern.slf4j.Slf4j;

/**
 * Abstract class providing reusable functionality to different method handlers
 */
@Slf4j
public abstract class AbstractValidatingMethodHandler {
    private static final String DELETE = "DELETE";
    protected final ResourceAccessor resourceAccessor;
    protected final ShapeTreeRequestHandler requestHandler;

    protected AbstractValidatingMethodHandler(ResourceAccessor resourceAccessor) {
        this.resourceAccessor = resourceAccessor;
        this.requestHandler = new ShapeTreeRequestHandler(resourceAccessor);
    }
}


