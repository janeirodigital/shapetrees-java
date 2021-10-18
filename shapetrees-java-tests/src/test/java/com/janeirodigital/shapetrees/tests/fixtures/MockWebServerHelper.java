package com.janeirodigital.shapetrees.tests.fixtures;

import okhttp3.mockwebserver.MockWebServer;

import java.net.URI;
import java.net.URISyntaxException;

public class MockWebServerHelper {

    public URI getURI(MockWebServer server, String path) throws URISyntaxException {
        return new URI(server.url(path).toString());
    }

}
