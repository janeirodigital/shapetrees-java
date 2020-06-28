package com.janeirodigital.shapetrees.client;

import okhttp3.Response;

import java.io.IOException;
import java.net.URISyntaxException;

public interface ValidatingHandler {
    Response process() throws IOException, URISyntaxException;
}
