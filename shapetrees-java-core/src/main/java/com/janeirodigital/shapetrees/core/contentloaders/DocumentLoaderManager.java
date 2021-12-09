package com.janeirodigital.shapetrees.core.contentloaders;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import lombok.Setter;
import lombok.Synchronized;

/**
 * Utility class which allows an external document loader to be set by calling code, and then
 * utilized by other code that doesn't require special knowledge of specific document
 * loader implementations.
 */
public abstract class DocumentLoaderManager {
    @Setter(onMethod_={@Synchronized})
    private static ExternalDocumentLoader loader;

    // Private constructor to offset an implicit public constructor on a utility class
    private DocumentLoaderManager() {}

    /**
     * Return an ExternalDocumentLoader that was previously set and stored statically
     * @return A valid ExternalDocumentLoader that was previously set
     * @throws ShapeTreeException
     */
    @Synchronized
    public static ExternalDocumentLoader getLoader() throws ShapeTreeException {
        if (loader == null) {
            throw new ShapeTreeException(500, "Must provide a valid ExternalDocumentLoader");
        }
        return DocumentLoaderManager.loader;
    }
}
