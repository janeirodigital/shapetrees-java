package com.janeirodigital.shapetrees.okhttp;

import com.janeirodigital.shapetrees.client.http.HttpClient;
import com.janeirodigital.shapetrees.client.http.HttpRequest;
import com.janeirodigital.shapetrees.core.DocumentResponse;
import com.janeirodigital.shapetrees.core.ResourceAttributes;
import com.janeirodigital.shapetrees.client.http.HttpRemoteResource;
import com.janeirodigital.shapetrees.core.ShapeTreeResource;
import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import okhttp3.Headers;
import okhttp3.ResponseBody;

import javax.net.ssl.*;
import java.io.IOException;
import java.net.URI;
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

    private okhttp3.OkHttpClient httpClient;

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
     * Execute an HTTP request and store the results in the passed HttpRemoteResource
     * @param request to execute
     * @param remoteResource to be updated
     * @throws IOException if HTTP request fails
     */
    @Override
    public void fetchIntoRemoteResource(HttpRequest request, HttpRemoteResource remoteResource) throws IOException {
        okhttp3.Response response = fetch(request);

        remoteResource.setExists(response.code() < 400);

        // Parse the headers for ease of use later
        ResourceAttributes parsedHeaders = new ResourceAttributes(response.headers().toMultimap());
        remoteResource.setResponseHeaders(parsedHeaders);

        // We especially care about Link headers which require extra parsing of the rel values
        final List<String> linkHeaders = parsedHeaders.allValues(HttpHeaders.LINK.getValue());
        if (linkHeaders.size() != 0) {
            remoteResource.setParsedLinkHeaders(ResourceAttributes.parseLinkHeaders(linkHeaders));
        } else {
            remoteResource.setParsedLinkHeaders(new ResourceAttributes());
        }

        // Save raw body
        try (ResponseBody respBody = response.body()) {
            remoteResource.setRawBody(respBody.string());
        }
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
        httpClient = clientBuilder.build();
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
        if (request.body == null)
            request.body = "";

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

                case HttpClient.PUT:
                    requestBuilder.put(okhttp3.RequestBody.create(request.body, okhttp3.MediaType.get(request.contentType)));
                    break;

                case HttpClient.POST:
                    requestBuilder.post(okhttp3.RequestBody.create(request.body, okhttp3.MediaType.get(request.contentType)));
                    break;

                case HttpClient.PATCH:
                    requestBuilder.patch(okhttp3.RequestBody.create(request.body, okhttp3.MediaType.get(request.contentType)));
                    break;

                case HttpClient.DELETE:
                    requestBuilder.delete();
                    break;

                default:
                    throw new ShapeTreeException(500, "Unsupported HTTP method for resource creation");

            }

            return httpClient.newCall(requestBuilder.build()).execute();
        } catch (IOException ex) {
            throw new ShapeTreeException(500, ex.getMessage());
        }
    }
}
