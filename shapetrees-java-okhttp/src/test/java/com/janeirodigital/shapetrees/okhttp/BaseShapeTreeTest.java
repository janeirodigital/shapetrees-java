package com.janeirodigital.shapetrees.okhttp;

import com.janeirodigital.shapetrees.client.core.ShapeTreeClient;
import com.janeirodigital.shapetrees.client.http.HttpShapeTreeClient;
import com.janeirodigital.shapetrees.client.http.HttpClientManager;
import com.janeirodigital.shapetrees.client.http.HttpRemoteResource;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;
import lombok.extern.slf4j.Slf4j;
import okhttp3.mockwebserver.MockWebServer;
import org.opentest4j.AssertionFailedError;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;

@Slf4j
public abstract class BaseShapeTreeTest {

    protected final ShapeTreeClient shapeTreeClient;
    protected final ShapeTreeContext context;
    protected static String TEXT_TURTLE = "text/turtle";

    public BaseShapeTreeTest() {
        HttpClientManager.setFactory(new OkHttpClientFactory(false));
        this.context = new ShapeTreeContext();
        this.shapeTreeClient = new HttpShapeTreeClient();
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

}
