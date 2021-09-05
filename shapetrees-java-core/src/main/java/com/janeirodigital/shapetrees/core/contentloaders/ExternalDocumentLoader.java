package com.janeirodigital.shapetrees.core.contentloaders;

import com.janeirodigital.shapetrees.core.ShapeTreeResponse;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;

import java.net.URI;

/**
 * Interface defining how a remote document can be loaded and its contents extracted.
 * Implementations can add capabilities like caching, retrieving resources from alternate
 * locations, etc.
 */
public interface ExternalDocumentLoader {
    /**
     * Describes the retrieval of a remote document
     * @param resourceURI URI of resource to be retrieved
     * @return ShapeTreeResponse representation which contains body and content type
     * @throws ShapeTreeException ShapeTreeException
     */
    ShapeTreeResponse loadExternalDocument(URI resourceURI) throws ShapeTreeException;
}
