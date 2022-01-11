package com.janeirodigital.shapetrees.client.okhttp;

import com.janeirodigital.shapetrees.core.resources.RelationAttributes;
import com.janeirodigital.shapetrees.core.resources.ResourceAttributes;
import com.janeirodigital.shapetrees.core.validation.ShapeTreeRequest;
import com.janeirodigital.shapetrees.core.enums.HttpHeader;
import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.helpers.RequestHelper;
import lombok.extern.slf4j.Slf4j;
import okhttp3.Request;
import okio.Buffer;

import java.io.IOException;
import java.net.URL;
import java.util.List;
import java.util.Objects;

@Slf4j
public class OkHttpShapeTreeRequest implements ShapeTreeRequest {

    private final Request request;
    private ShapeTreeResourceType resourceType;

    public OkHttpShapeTreeRequest(Request request) throws ShapeTreeException {
        this.request = request;
        this.resourceType = RequestHelper.getIncomingResourceType(this);
    }

    @Override
    public String getMethod() {
        return this.request.method();
    }

    @Override
    public URL getUrl() {
        return this.request.url().url();
    }

    @Override
    public ResourceAttributes getHeaders() {
        return new ResourceAttributes(this.request.headers().toMultimap());
    }

    @Override
    public RelationAttributes getLinkHeaders() {
        return ResourceAttributes.parseLinkHeaders(this.getHeaderValues(HttpHeader.LINK.getValue()));
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
        return this.getHeaders().firstValue(HttpHeader.CONTENT_TYPE.getValue()).orElse(null);
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
