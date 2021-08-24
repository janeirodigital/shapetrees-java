package com.janeirodigital.shapetrees.javahttp;

import com.janeirodigital.shapetrees.client.http.*;
import com.janeirodigital.shapetrees.core.ShapeTreeResponse;
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
        this.shapeTreeClient = new HttpShapeTreeClient();
        this.context = new ShapeTreeContext();
        this.skipShapeTreeValidation(false);

        HttpClientManager.setFactory(this.factory);
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

    ShapeTreeResponse postShapeTreeInstance(ShapeTreeContext context, URI parentContainer, URI focusNode, URI targetShapeTree, String proposedResourceName, Boolean isContainer, String bodyString, String contentType) throws IOException {
        HttpRequest request = this.shapeTreeClient.postShapeTreeInstance(context, parentContainer, focusNode, targetShapeTree, proposedResourceName, isContainer, bodyString, contentType);
        return this.fetcher.fetchShapeTreeResponse(request);
    }

    protected void skipShapeTreeValidation(boolean b) {
        try {
            this.fetcher = this.factory.get(!b);
        } catch (ShapeTreeException e) {
            throw new Error(e);
        }
    }
}
