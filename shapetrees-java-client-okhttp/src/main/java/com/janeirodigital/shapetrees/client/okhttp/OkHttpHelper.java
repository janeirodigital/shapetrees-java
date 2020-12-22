package com.janeirodigital.shapetrees.client.okhttp;

import com.janeirodigital.shapetrees.core.ShapeTreeResource;
import com.janeirodigital.shapetrees.core.ShapeTreeResponse;
import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.core.enums.LinkRelations;
import com.janeirodigital.shapetrees.core.helpers.HttpHeaderHelper;
import com.janeirodigital.shapetrees.core.vocabularies.LdpVocabulary;
import lombok.extern.slf4j.Slf4j;
import okhttp3.Headers;
import okhttp3.Response;

import java.io.IOException;
import java.net.URI;
import java.util.List;
import java.util.Map;
import java.util.Objects;

@Slf4j
public class OkHttpHelper {
    public static Headers convertHeaders(Map<String, List<String>> headers) {
        Headers.Builder okHttpHeaders = new Headers.Builder();
        for (String key : headers.keySet()) {
            List<String> values = headers.get(key);
            for (String value : values) {
                okHttpHeaders.add(key, value);
            }
        }
        return okHttpHeaders.build();
    }

    public static ShapeTreeResource mapOkHttpResponseToShapeTreeResource(Response response, URI requestURI, Map<String, List<String>> requestHeaders) {
        ShapeTreeResource shapeTreeResource = new ShapeTreeResource();

        shapeTreeResource.setExists(response.isSuccessful());
        shapeTreeResource.setContainer(isContainerFromHeaders(requestHeaders));

        try {
            shapeTreeResource.setBody(response.body().string());
        } catch (IOException ex) {
            log.error("Exception retrieving body string");
            shapeTreeResource.setBody(null);
        }
        shapeTreeResource.setAttributes(response.headers().toMultimap());
        shapeTreeResource.setUri(URI.create(Objects.requireNonNull(response.header(HttpHeaders.LOCATION.getValue(), requestURI.toString()))));

        return shapeTreeResource;
    }

    public static ShapeTreeResponse mapOkHttpResponseToShapeTreeResponse(Response response) {
        ShapeTreeResponse shapeTreeResponse = new ShapeTreeResponse();
        try {
            shapeTreeResponse.setBody(response.body().string());
        } catch (IOException ex) {
            log.error("Exception retrieving body string");
            shapeTreeResponse.setBody(null);
        }
        shapeTreeResponse.setHeaders(response.headers().toMultimap());
        shapeTreeResponse.setStatusCode(response.code());
        return shapeTreeResponse;
    }

    private static boolean isContainerFromHeaders(Map<String, List<String>> requestHeaders) {
        Map<String, List<String>> parsedLinkHeaders = HttpHeaderHelper.parseLinkHeadersToMap(requestHeaders.get(HttpHeaders.LINK.getValue()));

        if (parsedLinkHeaders.get(LinkRelations.TYPE.getValue()) != null) {
            return parsedLinkHeaders.get(LinkRelations.TYPE.getValue()).contains(LdpVocabulary.CONTAINER) ||
                    parsedLinkHeaders.get(LinkRelations.TYPE.getValue()).contains(LdpVocabulary.BASIC_CONTAINER);
        }
        return false;
    }
}
