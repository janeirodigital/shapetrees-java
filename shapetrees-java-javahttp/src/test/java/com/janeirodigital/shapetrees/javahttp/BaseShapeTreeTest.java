package com.janeirodigital.shapetrees.javahttp;

import com.janeirodigital.shapetrees.client.core.ShapeTreeClient;
import com.janeirodigital.shapetrees.client.http.AbstractHttpClientFactory;
import com.janeirodigital.shapetrees.client.http.HttpRemoteResource;
import com.janeirodigital.shapetrees.client.http.HttpShapeTreeClient;
import com.janeirodigital.shapetrees.core.contentloaders.DocumentLoaderManager;
import com.janeirodigital.shapetrees.core.contentloaders.HttpDocumentContentsLoader;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;
import lombok.extern.slf4j.Slf4j;
import okhttp3.mockwebserver.MockWebServer;
import org.opentest4j.AssertionFailedError;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;

@Slf4j
public abstract class BaseShapeTreeTest {

    protected final JavaHttpClientFactory factory;
    protected final HttpShapeTreeClient shapeTreeClient;
    protected final ShapeTreeContext context;
    protected JavaHttpClient fetcher;
    protected static String TEXT_TURTLE = "text/turtle";

    public BaseShapeTreeTest() {
        this.factory = new JavaHttpClientFactory(false);
        this.context = new ShapeTreeContext();
        this.shapeTreeClient = new HttpShapeTreeClient();
        this.skipShapeTreeValidation(false);

        AbstractHttpClientFactory.setFactory(this.factory);
        DocumentLoaderManager.setLoader(new HttpDocumentContentsLoader(null, null));
    }
    
    protected static void ensureExists(URI uri) throws IOException {
        HttpRemoteResource resource = new HttpRemoteResource(uri, null);
        if (!resource.exists()) {
            throw new AssertionFailedError("Resource " + uri + " doesn't exist");
        }
    }

    protected URI getURI(MockWebServer server, String path) throws URISyntaxException {
        return new URI(server.url(path).toString());
    }

    protected void skipShapeTreeValidation(boolean b) {
        try {
            this.fetcher = this.factory.get(!b);
        } catch (ShapeTreeException e) {
            throw new Error(e);
        }
    }
}
