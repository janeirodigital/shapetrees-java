package com.janeirodigital.shapetrees.okhttp;

import com.janeirodigital.shapetrees.core.contentloaders.DocumentLoaderManager;
import com.janeirodigital.shapetrees.core.contentloaders.ExternalDocumentLoader;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import lombok.Setter;
import lombok.Synchronized;

public class OkHttpClientFactoryManager {
    @Setter(onMethod_={@Synchronized})
    private static OkHttpClientFactory factory = new OkHttpBasicClientFactory();
    private static final ExternalDocumentLoader loader = new OkHttpExternalDocumentLoader();

    private OkHttpClientFactoryManager() { }

    @Synchronized
    public static OkHttpClientFactory getFactory() throws ShapeTreeException {
        if (factory == null) { throw new ShapeTreeException(500, "Must provide a valid HTTP client factory"); }
        DocumentLoaderManager.setLoader(loader);
        return factory;
    }
}
