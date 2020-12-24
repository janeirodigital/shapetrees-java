package com.janeirodigital.shapetrees.client.okhttp;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.ResourceAccessor;
import com.janeirodigital.shapetrees.core.ShapeTreeResource;
import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;
import lombok.extern.slf4j.Slf4j;
import okhttp3.*;

import java.io.IOException;
import java.net.URI;
import java.util.List;
import java.util.Map;

@Slf4j
public class OkHttpRemoteResourceAccessor implements ResourceAccessor {

    @Override
    public ShapeTreeResource getResource(ShapeTreeContext context, URI resourceURI) throws ShapeTreeException {
        try {
            return mapRemoteResourceToShapeTreeResource(new RemoteResource(resourceURI, context.getAuthorizationHeaderValue()));
        } catch (Exception ex) {
            throw new ShapeTreeException(500, ex.getMessage());
        }
    }

    @Override
    public ShapeTreeResource createResource(ShapeTreeContext context, URI resourceURI, Map<String, List<String>> headers, String body, String contentType) throws ShapeTreeException {
        log.debug("createResource: URI [{}], headers [{}]", resourceURI, writeHeaders(headers));

        try {
            if (body == null) {
                body = "";
            }

            OkHttpClient httpClient = ShapeTreeHttpClientHolder.getForConfig(new ShapeTreeClientConfiguration(false, false));
            Request.Builder createResourcePostBuilder = new Request.Builder();

            createResourcePostBuilder.headers(OkHttpHelper.convertHeaders(headers))
                    .post(RequestBody.create(body, MediaType.get(contentType)))
                    .url(resourceURI.toURL());

            if (context.getAuthorizationHeaderValue() != null) {
                createResourcePostBuilder.addHeader(HttpHeaders.AUTHORIZATION.getValue(), context.getAuthorizationHeaderValue());
            }

            Response response = httpClient.newCall(createResourcePostBuilder.build()).execute();
            return OkHttpHelper.mapOkHttpResponseToShapeTreeResource(response, resourceURI, headers);
        } catch (IOException ex) {
            throw new ShapeTreeException(500, ex.getMessage());
        }
    }

    @Override
    public ShapeTreeResource updateResource(ShapeTreeContext context, ShapeTreeResource updatedResource) throws ShapeTreeException {
        log.debug("updateResource: URI [{}]", updatedResource.getUri());

        try {
            String contentType = updatedResource.getFirstAttributeValue(HttpHeaders.CONTENT_TYPE.getValue());

            OkHttpClient httpClient = ShapeTreeHttpClientHolder.getForConfig(new ShapeTreeClientConfiguration(false, false));
            Request.Builder updateResourcePutBuilder = new Request.Builder();

            updateResourcePutBuilder.headers(OkHttpHelper.convertHeaders(updatedResource.getAttributes()))
                    .put(RequestBody.create(updatedResource.getBody(), MediaType.get(contentType)))
                    .url(updatedResource.getUri().toURL());

            if (context.getAuthorizationHeaderValue() != null) {
                updateResourcePutBuilder.addHeader(HttpHeaders.AUTHORIZATION.getValue(), context.getAuthorizationHeaderValue());
            }

            Response response = httpClient.newCall(updateResourcePutBuilder.build()).execute();
            if (!response.isSuccessful()) {
                log.error("Error updating resource {}, Status {} Message {}", updatedResource.getUri(), response.code(), response.message());
            }

            // Re-pull the resource after the update
            return getResource(context, updatedResource.getUri());
        } catch (IOException ex) {
            throw new ShapeTreeException(500, ex.getMessage());
        }

    }

    private String writeHeaders(Map<String, List<String>> headers) {
        StringBuilder sb = new StringBuilder();
        for (String key : headers.keySet()) {
            for (String value : headers.get(key)) {
                if (sb.length() != 0) {
                    sb.append(",");
                }
                sb.append(key).append("=").append(value);
            }
        }

        return sb.toString();
    }

    private ShapeTreeResource mapRemoteResourceToShapeTreeResource(RemoteResource remoteResource) throws ShapeTreeException {
        ShapeTreeResource shapeTreeResource = new ShapeTreeResource();
        try {
            shapeTreeResource.setUri(remoteResource.getURI());
        } catch (IOException ex) {
            throw new ShapeTreeException(500, "Error resolving URI");
        }

        shapeTreeResource.setExists(remoteResource.exists());
        shapeTreeResource.setContainer(remoteResource.isContainer());
        try {
            shapeTreeResource.setBody(remoteResource.getBody());
        } catch (IOException iex) {
            shapeTreeResource.setBody(null);
        }
        shapeTreeResource.setAttributes(remoteResource.getResponseHeaders());
        return shapeTreeResource;
    }
}
