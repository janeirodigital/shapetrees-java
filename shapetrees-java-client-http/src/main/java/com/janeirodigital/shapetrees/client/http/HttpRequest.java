package com.janeirodigital.shapetrees.client.http;

import com.janeirodigital.shapetrees.core.HttpHeaders;
import lombok.AllArgsConstructor;

import java.net.URI;

@AllArgsConstructor
public class HttpRequest {
    public String method;
    public URI resourceURI;
    public HttpHeaders headers;
    public String body;
    public String contentType;
}
