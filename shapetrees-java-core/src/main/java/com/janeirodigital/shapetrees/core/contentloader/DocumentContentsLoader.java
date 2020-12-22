package com.janeirodigital.shapetrees.core.contentloader;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.DocumentContents;

import java.net.URI;

public interface DocumentContentsLoader {
    DocumentContents loadDocumentContents(URI resourceURI) throws ShapeTreeException;
}
