package com.janeirodigital.shapetrees.client.fetch;

import com.janeirodigital.shapetrees.core.ShapeTreeRequest;
import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;
import com.janeirodigital.shapetrees.core.helpers.HttpHeaderHelper;
import lombok.extern.slf4j.Slf4j;
import okhttp3.Request;
import okio.Buffer;

import java.io.IOException;
import java.net.URI;
import java.util.List;
import java.util.Map;
import java.util.Objects;

@Slf4j
public class OkHttpShapeTreeRequest implements ShapeTreeRequest {
    private final Request request;
    private ShapeTreeResourceType resourceType;

    public OkHttpShapeTreeRequest(Request request) {
        this.request = request;
    }

    @Override
    public String getMethod() {
        return this.request.method();
    }

    @Override
    public URI getURI() {
        return this.request.url().uri();
    }

    @Override
    public Map<String, List<String>> getHeaders() {
        return this.request.headers().toMultimap();
    }

    @Override
    public Map<String, List<String>> getLinkHeaders() {
        return HttpHeaderHelper.parseLinkHeadersToMap(this.getHeaderValues(HttpHeaders.LINK.getValue()));
    }

    @Override
    public List<String> getHeaderValues(String header) {
        return this.request.headers(header);
    }

    @Override
    public String getHeaderValue(String header) {
        return this.request.header(header);
    }

    @Override
    public String getContentType() {
        if (this.getHeaders().containsKey(HttpHeaders.CONTENT_TYPE.getValue())) {
            return this.getHeaders().get(HttpHeaders.CONTENT_TYPE.getValue()).stream().findFirst().orElse(null);
        }
        return null;
    }

    @Override
    public ShapeTreeResourceType getResourceType() {
        return this.resourceType;
    }

    @Override
    public void setResourceType(ShapeTreeResourceType resourceType) {
        this.resourceType = resourceType;
    }

    @Override
    public String getBody() {
        try (Buffer buffer = new Buffer()) {
            if (this.request.body() != null) {
                Objects.requireNonNull(this.request.body()).writeTo(buffer);
            }
            return buffer.readUtf8();
        } catch (IOException | NullPointerException ex) {
            log.error("Error writing body to string");
            return null;
        }
    }
}
