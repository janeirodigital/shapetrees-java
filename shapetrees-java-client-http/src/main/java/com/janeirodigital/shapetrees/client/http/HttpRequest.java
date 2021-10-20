package com.janeirodigital.shapetrees.client.http;

import com.janeirodigital.shapetrees.core.ResourceAttributes;
import lombok.AllArgsConstructor;
import org.jetbrains.annotations.NotNull;

import java.net.URI;
import java.util.Optional;

@AllArgsConstructor
public class HttpRequest {
    public String method;
    public URI resourceURI;
    public ResourceAttributes headers;
    @NotNull public Optional<String> body;
    @NotNull public Optional<String> contentType;
}
