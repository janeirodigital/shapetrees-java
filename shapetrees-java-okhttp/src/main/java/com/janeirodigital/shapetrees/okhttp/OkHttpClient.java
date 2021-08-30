package com.janeirodigital.shapetrees.okhttp;

import com.janeirodigital.shapetrees.client.http.HttpClient;
import com.janeirodigital.shapetrees.client.http.HttpRequest;
import com.janeirodigital.shapetrees.core.HttpHeaders;
import com.janeirodigital.shapetrees.client.http.HttpRemoteResource;
import com.janeirodigital.shapetrees.core.ShapeTreeResource;
import com.janeirodigital.shapetrees.core.ShapeTreeResponse;
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
 * OkHttp documentation (https://square.github.io/okhttp/4.x/okhttp/okhttp3/-ok-http-client/#okhttpclients-should-be-shared)
 * recommends that instance of the client be shared/reused.  OkHttpClient's getForConfig provides an
 * instance of the OkHttpClient which can be re-used for multiple configurations (validation on/off, https verification on/off).
 */
@Slf4j
public class OkHttpClient extends HttpClient {
    private static final okhttp3.OkHttpClient baseClient = new okhttp3.OkHttpClient();

    private okhttp3.OkHttpClient httpClient;

    /**
     * Maps an OkHttp Response object to a ShapeTreeResource object
     * @param response OkHttp Response object
     * @param resourceURI URI of request associated with response
     * @param headers Request headers used in request associated with response
     * @return ShapeTreeResource instance with contents and response headers from response
     */
    public ShapeTreeResource fetchShapeTreeResource(HttpRequest request) throws ShapeTreeException {
        okhttp3.Response response = fetch(request);

        ShapeTreeResource shapeTreeResource = new ShapeTreeResource();

        shapeTreeResource.setExists(response.isSuccessful());
        shapeTreeResource.setContainer(isContainerFromHeaders(request.headers));
        shapeTreeResource.setType(getResourceTypeFromHeaders(request.headers));

        try {
            shapeTreeResource.setBody(Objects.requireNonNull(response.body()).string());
        } catch (IOException | NullPointerException ex) {
            log.error("Exception retrieving body string");
            shapeTreeResource.setBody(null);
        }
        shapeTreeResource.setAttributes(new HttpHeaders(response.headers().toMultimap()));
        shapeTreeResource.setUri(URI.create(Objects.requireNonNull(response.header(com.janeirodigital.shapetrees.core.enums.HttpHeaders.LOCATION.getValue(), request.resourceURI.toString()))));

        return shapeTreeResource;
    }

    /**
     * Maps an OkHttp Response object to a ShapeTreeResponse object
     * @return ShapeTreeResponse with values from OkHttp response
     */
    public ShapeTreeResponse fetchShapeTreeResponse(HttpRequest request) throws ShapeTreeException {
        okhttp3.Response response = fetch(request);

        ShapeTreeResponse shapeTreeResponse = new ShapeTreeResponse();
        try {
            shapeTreeResponse.setBody(Objects.requireNonNull(response.body()).string());
        } catch (IOException | NullPointerException ex) {
            log.error("Exception retrieving body string");
            shapeTreeResponse.setBody(null);
        }
        shapeTreeResponse.setHeaders(new HttpHeaders(response.headers().toMultimap()));
        shapeTreeResponse.setStatusCode(response.code());
        return shapeTreeResponse;
    }

    public void fetchIntoRemoteResource(HttpRequest request, HttpRemoteResource remoteResource) throws IOException {
        okhttp3.Response response = fetch(request);

        remoteResource.setExists(response.code() < 400);

        // Parse the headers for ease of use later
        HttpHeaders parsedHeaders = new HttpHeaders(response.headers().toMultimap());
        remoteResource.setResponseHeaders(parsedHeaders);

        // We especially care about Link headers which require extra parsing of the rel values
        if (parsedHeaders.get(com.janeirodigital.shapetrees.core.enums.HttpHeaders.LINK.getValue()) != null) {
            remoteResource.setParsedLinkHeaders(HttpHeaders.parseLinkHeaders(response.headers(com.janeirodigital.shapetrees.core.enums.HttpHeaders.LINK.getValue())));
        } else {
            remoteResource.setParsedLinkHeaders(new HttpHeaders());
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
    public static Headers toNativeHeaders(HttpHeaders headers) {
        Headers.Builder okHttpHeaders = new Headers.Builder();
        for (Map.Entry<String, List<String>> entry : headers.entrySet()){
            for (String value : entry.getValue()) {
                okHttpHeaders.add(entry.getKey(), value);
            }
        }
        return okHttpHeaders.build();
    }

    // constructor and its helpers
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
                    .hostnameVerifier(getTrustAllHostnameVerifier());
        }
        httpClient = clientBuilder.build();
    }

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

    private static HostnameVerifier getTrustAllHostnameVerifier() {
        return (hostname, session) -> true;
    }

    // http functions
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

                case GET:
                    requestBuilder.get();
                    break;

                case PUT:
                    requestBuilder.put(okhttp3.RequestBody.create(request.body, okhttp3.MediaType.get(request.contentType)));
                    break;

                case POST:
                    requestBuilder.post(okhttp3.RequestBody.create(request.body, okhttp3.MediaType.get(request.contentType)));
                    break;

                case PATCH:
                    requestBuilder.patch(okhttp3.RequestBody.create(request.body, okhttp3.MediaType.get(request.contentType)));
                    break;

                case DELETE:
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
