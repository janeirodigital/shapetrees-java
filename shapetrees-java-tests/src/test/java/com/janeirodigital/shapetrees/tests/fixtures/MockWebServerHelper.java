package com.janeirodigital.shapetrees.tests.fixtures;

import okhttp3.mockwebserver.MockWebServer;

import java.net.URL;
import java.net.MalformedURLException;

public class MockWebServerHelper {

    public static URL toUrl(MockWebServer server, String path) throws MalformedURLException {
        return new URL(server.url(path).toString());
    }

}
