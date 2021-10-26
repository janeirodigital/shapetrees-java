package com.janeirodigital.shapetrees.core.contentloaders;

import com.janeirodigital.shapetrees.core.DocumentResponse;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;

import java.net.URL;

/**
 * Interface defining how a remote document can be loaded and its contents extracted.
 * Implementations can add capabilities like caching, retrieving resources from alternate
 * locations, etc.
 */
public interface ExternalDocumentLoader {
    /**
     * Describes the retrieval of a remote document
     * @param resourceUrl URL of resource to be retrieved
     * @return DocumentResponse representation which contains body and content type
     * @throws ShapeTreeException ShapeTreeException
     */
    DocumentResponse loadExternalDocument(URL resourceUrl) throws ShapeTreeException;
}
