package com.janeirodigital.shapetrees.client.http;

import com.janeirodigital.shapetrees.core.ResourceAttributes;
import lombok.AllArgsConstructor;

import java.net.URL;

@AllArgsConstructor
public class HttpRequest {
    public String method;
    public URL resourceURL;
    public ResourceAttributes headers;
    public String body;
    public String contentType;
}
