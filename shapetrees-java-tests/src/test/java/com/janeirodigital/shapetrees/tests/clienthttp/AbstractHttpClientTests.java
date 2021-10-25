package com.janeirodigital.shapetrees.tests.clienthttp;

import com.janeirodigital.shapetrees.client.http.HttpClient;
import com.janeirodigital.shapetrees.client.http.HttpClientFactory;
import com.janeirodigital.shapetrees.client.http.HttpShapeTreeClient;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;
import lombok.extern.slf4j.Slf4j;
import okhttp3.mockwebserver.MockWebServer;

import java.net.URL;
import java.net.MalformedURLException;

@Slf4j
public abstract class AbstractHttpClientTests {

    protected HttpClientFactory factory = null;
    protected HttpShapeTreeClient shapeTreeClient = new HttpShapeTreeClient();
    protected final ShapeTreeContext context;
    protected HttpClient fetcher;
    protected static String TEXT_TURTLE = "text/turtle";

    public AbstractHttpClientTests() {
        this.context = new ShapeTreeContext(null);
    }

    public URL getURL(MockWebServer server, String path) throws MalformedURLException {
        return new URL(server.url(path).toString());
    }

    protected void skipShapeTreeValidation(boolean b) {
        try {
            this.fetcher = this.factory.get(!b);
        } catch (ShapeTreeException e) {
            throw new Error(e);
        }
    }
}
