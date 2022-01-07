package com.janeirodigital.shapetrees.okhttp;

import com.janeirodigital.shapetrees.core.ResourceAttributes;
import com.janeirodigital.shapetrees.core.enums.ContentType;
import com.janeirodigital.shapetrees.core.enums.HttpHeader;
import com.janeirodigital.shapetrees.core.enums.LinkRelation;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import lombok.extern.slf4j.Slf4j;
import okhttp3.*;

import java.io.IOException;
import java.net.URL;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import static com.janeirodigital.shapetrees.core.enums.ContentType.OCTET_STREAM;
import static com.janeirodigital.shapetrees.core.enums.HttpHeader.AUTHORIZATION;
import static com.janeirodigital.shapetrees.core.enums.HttpHeader.LINK;
import static com.janeirodigital.shapetrees.core.enums.HttpMethod.*;

@Slf4j
public class OkHttpHelper {

    /**
     * Perform an HTTP GET on the resource at <code>url</code>.
     * The response MUST be closed outside of this call. The body of the response
     * is returned as a one-shot stream, and <b>MUST BE CLOSED</b> separately
     * by the caller.
     * @see <a href="https://square.github.io/okhttp/4.x/okhttp/okhttp3/-response-body/#the-response-body-must-be-closed">OkHttp - Closing the Response Body</a>
     * @param okHttpClient OkHttpClient to perform the GET with
     * @param url URL of the resource to GET
     * @param credentials Optional credential string to include in Authorization Http Header
     * @param headers Optional OkHttp Headers to include
     * @return OkHttp Response
     * @throws ShapeTreeException
     */
    public static Response getHttpResource(OkHttpClient okHttpClient, URL url, String credentials, Headers headers) throws ShapeTreeException {
        try {
            Request.Builder requestBuilder = new Request.Builder();
            requestBuilder.url(url);
            requestBuilder.method(GET.getValue(), null);
            if (credentials != null) { headers = setHttpHeader(AUTHORIZATION, credentials, headers); }
            if (headers != null) { requestBuilder.headers(headers); }
            return checkResponse(okHttpClient.newCall(requestBuilder.build()).execute());
        } catch (IOException ex) {
            throw new ShapeTreeException(500, "Failed to get remote resource: " + ex.getMessage());
        }
    }

    /**
     * Calls {@link #getHttpResource(OkHttpClient, URL, String, Headers)} without any
     * additional headers supplied.
     * @param okHttpClient OkHttpClient to perform the GET with
     * @param url URL of the resource to GET
     * @param credentials Optional credential string to include in Authorization Http Header
     * @return OkHttp Response
     * @throws ShapeTreeException
     */
    public static Response getHttpResource(OkHttpClient okHttpClient, URL url, String credentials) throws ShapeTreeException {
        return getHttpResource(okHttpClient, url, credentials, null);
    }


    /**
     * Perform an HTTP POST on the resource at <code>url</code>.
     * <i>ResponseBody is closed automatically</i>.
     * @param okHttpClient OkHttpClient to perform the POST with
     * @param url URL of the resource to POST into
     * @param credentials Optional credential string to include in Authorization Http Header
     * @param headers Optional OkHttp Headers to include
     * @param body Body of the POST request
     * @param contentType {@link ContentType} of the POST request
     * @return OkHttp Response
     * @throws ShapeTreeException
     */
    public static Response postHttpResource(OkHttpClient okHttpClient, URL url, String credentials, Headers headers, String body, ContentType contentType) throws ShapeTreeException {

        Request.Builder requestBuilder = new Request.Builder();
        requestBuilder.url(url);
        if (body == null) { body = ""; }
        if (contentType == null) { contentType = OCTET_STREAM; }
        RequestBody requestBody = RequestBody.create(body, MediaType.get(contentType.getValue()));
        requestBuilder.method(POST.getValue(), requestBody);
        if (credentials != null) { headers = setHttpHeader(AUTHORIZATION, credentials, headers); }
        if (headers != null) { requestBuilder.headers(headers); }

        try (Response response = okHttpClient.newCall(requestBuilder.build()).execute()) {
            // wrapping the call in try-with-resources automatically closes the response
            return checkResponse(response);
        } catch (IOException ex) {
            throw new ShapeTreeException(500, "Failed to POST remote resource: " + ex.getMessage());
        }
    }

    /**
     * Call {@link #postHttpResource(OkHttpClient, URL, String, Headers, String, ContentType)} without
     * any additional headers supplied.
     * @param okHttpClient OkHttpClient to perform the POST with
     * @param url URL of the resource to POST into
     * @param credentials Optional credential string to include in Authorization Http Header
     * @param body Body of the POST request
     * @param contentType {@link ContentType} of the POST request
     * @return OkHttp Response     * @return
     * @throws ShapeTreeException
     */
    public static Response postHttpResource(OkHttpClient okHttpClient, URL url, String credentials, String body, ContentType contentType) throws ShapeTreeException {
        return postHttpResource(okHttpClient, url, credentials, null, body, contentType);
    }

    /**
     * Perform an HTTP PUT on the resource at <code>url</code>.
     * <i>ResponseBody is closed automatically</i>.
     * @param okHttpClient OkHttpClient to perform the PUT with
     * @param url URL of the resource to PUT
     * @param credentials Optional credential string to include in Authorization Http Header
     * @param headers Optional OkHttp Headers to include
     * @param body Body of the PUT request
     * @param contentType {@link ContentType} of the PUT request
     * @return OkHttp Response
     * @throws ShapeTreeException
     */
    public static Response putHttpResource(OkHttpClient okHttpClient, URL url, String credentials, Headers headers, String body, ContentType contentType) throws ShapeTreeException {

        Request.Builder requestBuilder = new Request.Builder();
        requestBuilder.url(url);
        if (body == null) { body = ""; }
        if (contentType == null) { contentType = OCTET_STREAM; }
        RequestBody requestBody = RequestBody.create(body, MediaType.get(contentType.getValue()));
        requestBuilder.method(PUT.getValue(), requestBody);
        if (credentials != null) { headers = setHttpHeader(AUTHORIZATION, credentials, headers); }
        if (headers != null) { requestBuilder.headers(headers); }

        try (Response response = okHttpClient.newCall(requestBuilder.build()).execute()) {
            // wrapping the call in try-with-resources automatically closes the response
            return checkResponse(response);
        } catch (IOException ex) {
            throw new ShapeTreeException(500, "Failed to PUT remote resource: " + ex.getMessage());
        }
    }

    /**
     * Call {@link #putHttpResource(OkHttpClient, URL, String, Headers, String, ContentType)} without
     * any additional headers supplied
     * @param okHttpClient OkHttpClient to perform the PUT with
     * @param url URL of the resource to PUT
     * @param credentials Optional credential string to include in Authorization Http Header
     * @param body Body of the PUT request
     * @param contentType {@link ContentType} of the PUT request
     * @return OkHttp Response
     * @throws ShapeTreeException
     */
    public static Response putHttpResource(OkHttpClient okHttpClient, URL url, String credentials, String body, ContentType contentType) throws ShapeTreeException {
        return putHttpResource(okHttpClient, url, credentials, null, body, contentType);
    }

    /**
     * Perform an HTTP PATCH on the resource at <code>url</code>.
     * <i>ResponseBody is closed automatically</i>.
     * @param okHttpClient OkHttpClient to perform the PATCH with
     * @param url URL of the resource to PATCH
     * @param credentials Optional credential string to include in Authorization Http Header
     * @param headers Optional OkHttp Headers to include
     * @param body Body of the PATCH request
     * @param contentType {@link ContentType} of the PATCH request
     * @return OkHttp Response
     * @throws ShapeTreeException
     */
    public static Response patchHttpResource(OkHttpClient okHttpClient, URL url, String credentials, Headers headers, String body, ContentType contentType) throws ShapeTreeException {

        Request.Builder requestBuilder = new Request.Builder();
        requestBuilder.url(url);
        if (body == null) { body = ""; }
        if (contentType == null) { contentType = OCTET_STREAM; }  // TODO - Exception if content-type isn't sparql-update?
        RequestBody requestBody = RequestBody.create(body, MediaType.get(contentType.getValue()));
        requestBuilder.method(PATCH.getValue(), requestBody);
        if (credentials != null) { headers = setHttpHeader(AUTHORIZATION, credentials, headers); }
        if (headers != null) { requestBuilder.headers(headers); }

        try (Response response = okHttpClient.newCall(requestBuilder.build()).execute()) {
            // wrapping the call in try-with-resources automatically closes the response
            return checkResponse(response);
        } catch (IOException ex) {
            throw new ShapeTreeException(500, "Failed to PATCH remote resource: " + ex.getMessage());
        }
    }

    /**
     * Call {@link #patchHttpResource(OkHttpClient, URL, String, Headers, String, ContentType)} without
     * any additional headers supplied
     * @param okHttpClient OkHttpClient to perform the PATCH with
     * @param url URL of the resource to PATCH
     * @param credentials Optional credential string to include in Authorization Http Header
     * @param body Body of the PATCH request
     * @param contentType {@link ContentType} of the PATCH request
     * @return OkHttp Response
     * @throws ShapeTreeException
     */
    public static Response patchHttpResource(OkHttpClient okHttpClient, URL url, String credentials, String body, ContentType contentType) throws ShapeTreeException {
        return patchHttpResource(okHttpClient, url, credentials, null, body, contentType);
    }



    /**
     * Perform an HTTP DELETE on the resource at <code>url</code>.
     * <i>ResponseBody is closed automatically</i>.
     * @param okHttpClient OkHttpClient to perform the DELETE with
     * @param url URL of the resource to DELETE
     * @param credentials Optional credential string to include in Authorization Http Header
     * @param headers Optional OkHttp Headers to include
     * @return OkHttp Response
     * @throws ShapeTreeException
     */
    public static Response deleteHttpResource(OkHttpClient okHttpClient, URL url, String credentials, Headers headers) throws ShapeTreeException {

        Request.Builder requestBuilder = new Request.Builder();
        requestBuilder.url(url);
        RequestBody body = RequestBody.create(null, new byte[0]);
        requestBuilder.method(DELETE.getValue(), body);
        if (credentials != null) { headers = setHttpHeader(AUTHORIZATION, credentials, headers); }
        if (headers != null) { requestBuilder.headers(headers); }

        try (Response response = okHttpClient.newCall(requestBuilder.build()).execute()) {
            return checkResponse(response);
        } catch (IOException ex) {
            throw new ShapeTreeException(500, "Failed to DELETE remote resource: " + ex.getMessage());
        }

    }

    /**
     * Call {@link #deleteHttpResource(OkHttpClient, URL, String, Headers)} without any
     * additional headers supplied
     * @param okHttpClient OkHttpClient to perform the DELETE with
     * @param url URL of the resource to DELETE
     * @param credentials Optional credential string to include in Authorization Http Header
     * @return OkHttp Response
     * @throws ShapeTreeException
     */
    public static Response deleteHttpResource(OkHttpClient okHttpClient, URL url, String credentials) throws ShapeTreeException {
        return deleteHttpResource(okHttpClient, url, credentials, null);
    }

    /**
     * Check the provided <code>response</code> for viability
     * @param response Response to check
     * @return Checked response
     */
    public static Response checkResponse(Response response) throws ShapeTreeException {
        Objects.requireNonNull(response, "Do not expect to receive a null response to an HTTP client request");
        Request request = response.request();
        if (!response.isSuccessful()) { log.warn("{} request to {} unsuccessful: {} {}", request.method(), request.url(), response.code(), response.message()); }
        return response;
    }

    /**
     * Set the HTTP header identified by <code>name</code> with the provided <code>value</code>. If
     * <code>headers</code> are provided, they will be included in the Headers returned. If
     * the Header to set already exists in that set, it will be updated.
     * @param name {@link HttpHeader} to set
     * @param value Value to use for the header
     * @param headers Optional OkHttp Headers to include
     * @return Updated OkHttp Headers
     */
    public static Headers setHttpHeader(HttpHeader name, String value, Headers headers) {
        Headers.Builder builder = new Headers.Builder();
        if (headers != null) { builder.addAll(headers); }
        builder.set(name.getValue(), value);
        return builder.build();
    }

    /**
     * Set the HTTP header identified by <code>name</code> with the provided <code>value</code>.
     * @param name {@link HttpHeader} to set
     * @param value Value to use for the header
     * @return Updated OkHttp Headers
     */
    public static Headers setHttpHeader(HttpHeader name, String value) {
        return setHttpHeader(name, value, null);
    }

    /**
     * Add the HTTP header identified by <code>name</code> with the provided <code>value</code>.
     * If <code>headers</code> are provided, they will be included in the Headers returned.
     * @param name {@link HttpHeader} to add
     * @param value Value to use for the added header
     * @param headers Optional OkHttp Headers to include
     * @return Populated OkHttp Headers
     */
    public static Headers addHttpHeader(HttpHeader name, String value, Headers headers) {
        Headers.Builder builder = new Headers.Builder();
        if (headers != null) { builder.addAll(headers); }
        builder.add(name.getValue(), value);
        return builder.build();
    }

    /**
     * Add the HTTP header identified by <code>name</code> with the provided <code>value</code>.
     * @param name {@link HttpHeader} to add
     * @param value Value to use for the added header
     * @return Populated OkHttp Headers
     */
    public static Headers addHttpHeader(HttpHeader name, String value) {
        return addHttpHeader(name, value, null);
    }

    /**
     * Converts "multi map" representation of attributes to the OkHttp Headers class
     * public for OkHttpValidatingShapeTreeInterceptor.createResponse
     * @param attributes Multi-map representation of attributes
     * @return OkHttp Headers object
     */
    public static Headers attributesToHeaders(ResourceAttributes attributes) {
        Headers.Builder okHttpHeaders = new Headers.Builder();
        if (attributes != null) {
            for (Map.Entry<String, List<String>> entry : attributes.toMultimap().entrySet()) {
                for (String value : entry.getValue()) {
                    okHttpHeaders.add(entry.getKey(), value);
                }
            }
        }
        return okHttpHeaders.build();
    }

    /**
     * Add an HTTP Link Relation header of <code>type</code> with the provided <code>target</code>.
     * If <code>headers</code> are provided, they will be included in the Headers returned.
     * @see <a href="https://www.rfc-editor.org/rfc/rfc8288.html">RFC 8288 - Web Linking</a>
     * @param type Link relation type
     * @param target Link relation target
     * @param headers Optional OkHttp Headers to include
     * @return Populated OkHttp Headers
     */
    public static Headers addLinkRelationHeader(LinkRelation type, String target, Headers headers) {
        return addHttpHeader(LINK, getLinkRelationString(type, target), headers);
    }

    /**
     * Add an HTTP Link Relation header of <code>type</code> with the provided <code>target</code>.
     * @see <a href="https://www.rfc-editor.org/rfc/rfc8288.html">RFC 8288 - Web Linking</a>
     * @param type Link relation type
     * @param target Link relation target
     * @return Populated OkHttp Headers
     */
    public static Headers addLinkRelationHeader(LinkRelation type, String target) {
        return addLinkRelationHeader(type, target, null);
    }

    /**
     * Get a formatted HTTP Link Relation string in compliance with RFC 8288
     * @param type Link relation type
     * @param target Link relation target
     * @return Formatted Link Relation string
     */
    public static String getLinkRelationString(LinkRelation type, String target) {
        return "<"+target+">;"+" rel=\""+type.getValue()+"\"";
    }

}
