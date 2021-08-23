package com.janeirodigital.shapetrees.javahttp;

import com.janeirodigital.shapetrees.client.core.ShapeTreeClient;
import com.janeirodigital.shapetrees.client.http.HttpClient;
import com.janeirodigital.shapetrees.client.http.HttpShapeTreeClient;
import com.janeirodigital.shapetrees.client.http.HttpClientManager;
import com.janeirodigital.shapetrees.client.http.HttpRemoteResource;
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

    protected final HttpShapeTreeClient shapeTreeClient;
    protected final ShapeTreeContext context;
    protected HttpClient fetcher;
    protected static String TEXT_TURTLE = "text/turtle";

    public BaseShapeTreeTest() {
        HttpClientManager.setFactory(new JavaHttpClientFactory(false));
        this.context = new ShapeTreeContext();
        this.shapeTreeClient = new HttpShapeTreeClient();
        this.skipShapeTreeValidation(false);
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
        HttpShapeTreeClient.Request request = this.shapeTreeClient.postShapeTreeInstance(context, parentContainer, focusNode, targetShapeTree, proposedResourceName, isContainer, bodyString, contentType);
        return this.fetcher.fetchShapeTreeResponse(request);
    }

    protected void skipShapeTreeValidation(boolean b) {
        try {
            this.fetcher = HttpClientManager.getFactory().get(!b);
        } catch (ShapeTreeException e) {
            throw new Error(e);
        }
    }
}
