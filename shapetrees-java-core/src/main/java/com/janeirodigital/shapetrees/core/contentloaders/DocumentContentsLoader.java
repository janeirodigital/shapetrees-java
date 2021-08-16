package com.janeirodigital.shapetrees.core.contentloaders;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.DocumentContents;

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
     * @return DocumentContents representation which contains body and content type
     * @throws ShapeTreeException ShapeTreeException
     */
    DocumentContents loadDocumentContents(URI resourceURI) throws ShapeTreeException;
}
