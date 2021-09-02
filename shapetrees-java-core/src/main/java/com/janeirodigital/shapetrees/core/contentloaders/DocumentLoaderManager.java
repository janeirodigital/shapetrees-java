package com.janeirodigital.shapetrees.core.contentloaders;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import lombok.Setter;
import lombok.Synchronized;

public abstract class DocumentLoaderManager {
    @Setter(onMethod_={@Synchronized})
    private static ExternalDocumentLoader loader;

    @Synchronized
    public static ExternalDocumentLoader getLoader() throws ShapeTreeException {
        if (loader == null) { throw new ShapeTreeException(500, "Must provide a valid ExternalDocumentLoader"); }
        return DocumentLoaderManager.loader;
    }
}
