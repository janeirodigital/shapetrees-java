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

    private OkHttpHelper() {
    }

    /**
     * Converts "multi map" representation of headers to the OkHttp Headers class
     * @param headers Multi-map representation of headers
     * @return OkHttp Headers object
     */
    public static Headers convertHeaders(Map<String, List<String>> headers) {
        Headers.Builder okHttpHeaders = new Headers.Builder();
        for (Map.Entry<String, List<String>> entry : headers.entrySet()){
            for (String value : entry.getValue()) {
                okHttpHeaders.add(entry.getKey(), value);
            }
        }
        return okHttpHeaders.build();
    }

    /**
     * Maps an OkHttp Response object to a ShapeTreeResource object
     * @param response OkHttp Response object
     * @param requestURI URI of request associated with response
     * @param requestHeaders Request headers used in request associated with response
     * @return ShapeTreeResource instance with contents and response headers from response
     */
    public static ShapeTreeResource mapOkHttpResponseToShapeTreeResource(Response response, URI requestURI, Map<String, List<String>> requestHeaders) {
        ShapeTreeResource shapeTreeResource = new ShapeTreeResource();

        shapeTreeResource.setExists(response.isSuccessful());
        shapeTreeResource.setContainer(isContainerFromHeaders(requestHeaders));

        try {
            shapeTreeResource.setBody(Objects.requireNonNull(response.body()).string());
        } catch (IOException | NullPointerException ex) {
            log.error("Exception retrieving body string");
            shapeTreeResource.setBody(null);
        }
        shapeTreeResource.setAttributes(response.headers().toMultimap());
        shapeTreeResource.setUri(URI.create(Objects.requireNonNull(response.header(HttpHeaders.LOCATION.getValue(), requestURI.toString()))));

        return shapeTreeResource;
    }

    /**
     * Maps an OkHttp Response object to a ShapeTreeResponse object
     * @param response OkHttp Response object
     * @return ShapeTreeResponse with values from OkHttp response
     */
    public static ShapeTreeResponse mapOkHttpResponseToShapeTreeResponse(Response response) {
        ShapeTreeResponse shapeTreeResponse = new ShapeTreeResponse();
        try {
            shapeTreeResponse.setBody(Objects.requireNonNull(response.body()).string());
        } catch (IOException | NullPointerException ex) {
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
