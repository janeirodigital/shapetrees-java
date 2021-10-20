package com.janeirodigital.shapetrees.okhttp;

import com.janeirodigital.shapetrees.client.http.HttpClient;
import com.janeirodigital.shapetrees.client.http.HttpRequest;
import com.janeirodigital.shapetrees.core.DocumentResponse;
import com.janeirodigital.shapetrees.core.ResourceAttributes;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import okhttp3.Headers;

import javax.net.ssl.*;
import java.io.IOException;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.util.*;

import lombok.extern.slf4j.Slf4j;

/**
 * okhttp implementation of HttpClient
 */
@Slf4j
public class OkHttpClient implements HttpClient {
    private static final okhttp3.OkHttpClient baseClient = new okhttp3.OkHttpClient();

    private final okhttp3.OkHttpClient httpClient;

    /**
     * Execute an HTTP request to create a DocumentResponse object
     * Implements `HttpClient` interface
     * @param request an HTTP request with appropriate headers for ShapeTree interactions
     * @return new DocumentResponse with response headers and contents
     * @throws ShapeTreeException
     */
    @Override
    public DocumentResponse fetchShapeTreeResponse(HttpRequest request) throws ShapeTreeException {
        okhttp3.Response response = fetch(request);

        String body = null;
        try {
            body = Objects.requireNonNull(response.body()).string();
        } catch (IOException | NullPointerException ex) {
            log.error("Exception retrieving body string");
        }
        return new DocumentResponse(new ResourceAttributes(response.headers().toMultimap()), body, response.code());
    }

    /**
     * Converts "multi map" representation of headers to the OkHttp Headers class
     * public for OkHttpValidatingShapeTreeInterceptor.createResponse
     * @param headers Multi-map representation of headers
     * @return OkHttp Headers object
     */
    public static Headers toNativeHeaders(ResourceAttributes headers) {
        Headers.Builder okHttpHeaders = new Headers.Builder();
        for (Map.Entry<String, List<String>> entry : headers.toMultimap().entrySet()){
            for (String value : entry.getValue()) {
                okHttpHeaders.add(entry.getKey(), value);
            }
        }
        return okHttpHeaders.build();
    }

    /**
     * Construct an OkHttpClient with switches to enable or disable SSL and ShapeTree validation
     * @param useSslValidation
     * @param useShapeTreeValidation
     * @throws NoSuchAlgorithmException potentially thrown while disabling SSL validation
     * @throws KeyManagementException potentially thrown while disabling SSL validation
     */
    protected OkHttpClient(boolean useSslValidation, boolean useShapeTreeValidation) throws NoSuchAlgorithmException, KeyManagementException {
        okhttp3.OkHttpClient.Builder clientBuilder = baseClient.newBuilder();
        if (Boolean.TRUE.equals(useShapeTreeValidation)) {
            clientBuilder.interceptors().add(new OkHttpValidatingShapeTreeInterceptor());
        }
        if (Boolean.FALSE.equals(useSslValidation)) {
            // Install the all-trusting trust manager
            final SSLContext sslContext = SSLContext.getInstance("TLSv1.2");
            TrustManager[] trustAllCerts = getTrustAllCertsManager();
            sslContext.init(null, trustAllCerts, new java.security.SecureRandom());
            // Create an ssl socket factory with our all-trusting manager
            final SSLSocketFactory sslSocketFactory = sslContext.getSocketFactory();

            clientBuilder.sslSocketFactory(sslSocketFactory, (X509TrustManager)trustAllCerts[0])
                    .hostnameVerifier((hostname, session) -> true);
        }
        this.httpClient = clientBuilder.build();
    }

    // permissive SSL trust manager
    private static TrustManager[] getTrustAllCertsManager() {
        // Create a trust manager that does not validate certificate chains
        return new TrustManager[] {
                new X509TrustManager() {
                    @Override
                    public void checkClientTrusted(java.security.cert.X509Certificate[] chain, String authType) {
                        // All clients are trusted when SSL validation is skipped
                    }

                    @Override
                    public void checkServerTrusted(java.security.cert.X509Certificate[] chain, String authType) {
                        // All servers are trusted when SSL validation is skipped
                    }

                    @Override
                    public java.security.cert.X509Certificate[] getAcceptedIssuers() {
                        return new java.security.cert.X509Certificate[]{};
                    }
                }
        };
    }

    /**
     * Internal function to execute HTTP request and return okhttp response
     * @param request
     * @return
     * @throws ShapeTreeException
     */
    private okhttp3.Response fetch(HttpRequest request) throws ShapeTreeException {
        try {
            okhttp3.Request.Builder requestBuilder = new okhttp3.Request.Builder();
            requestBuilder.url(request.resourceURI.toURL());

            if (request.headers != null) {
                requestBuilder.headers(toNativeHeaders(request.headers));
            }

            switch (request.method) {

                case HttpClient.GET:
                    requestBuilder.get();
                    break;

                case HttpClient.DELETE:
//                    requestBuilder.method(request.method, null); // in principle, this invocation would work for GET and DELETE, but since DELETE *can* have a body, we need to:
//                    requestBuilder.method(request.method, okhttp3.RequestBody.create(null, new byte[0])); // *but* this breaks `request.getBody() == null` so give up and use specific function
                    requestBuilder.delete();
                    break;

                case HttpClient.PUT:
                case HttpClient.POST:
                case HttpClient.PATCH:
                    requestBuilder.method(request.method, okhttp3.RequestBody.create(request.body.orElseThrow(
                            () -> new ShapeTreeException(500, "Need body for " + request.method + " <" + request.resourceURI + ">")
                    ), okhttp3.MediaType.get(request.contentType.orElseThrow(
                            () -> new ShapeTreeException(500, "Need content type for " + request.method + " <" + request.resourceURI + ">")
                    ))));
                    requestBuilder.addHeader("Content-Type", request.contentType.orElseThrow(
                            () -> new ShapeTreeException(500, "Need content type for " + request.method + " <" + request.resourceURI + ">")
                    ));
                    break;

                default:
                    throw new ShapeTreeException(500, "Unsupported HTTP method for resource creation");

            }

            return OkHttpClient.check(this.httpClient.newCall(requestBuilder.build()).execute());
        } catch (IOException ex) {
            throw new ShapeTreeException(500, ex.getMessage());
        }
    }

    protected static okhttp3.Response check(okhttp3.Response resp) {
        if (resp.code() > 599) {
            throw new Error("invalid HTTP response: " + resp + (resp.body() == null ? "" : "\n" + resp.body()));
        }
        return resp;
    }
}
