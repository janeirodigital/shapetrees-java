package com.janeirodigital.shapetrees.core.contentloaders;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.DocumentResponse;

import java.net.URI;

/**
 * Interface defining how a remote document can be loaded and its contents extracted.
 * Implementations can add capabilities like caching, retrieving resources from alternate
 * locations, etc.
 */
public interface DocumentContentsLoader {
    /**
     * Describes the retrieval of a remote document
     * @param resourceURI URI of resource to be retrieved
     * @return DocumentResponse representation which contains body and content type
     * @throws ShapeTreeException ShapeTreeException
     */
    DocumentResponse loadDocumentContents(URI resourceURI) throws ShapeTreeException;
}
