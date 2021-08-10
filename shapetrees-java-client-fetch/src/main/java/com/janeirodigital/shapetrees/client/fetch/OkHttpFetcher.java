package com.janeirodigital.shapetrees.client.fetch;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URI;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import com.janeirodigital.shapetrees.core.ShapeTreeResource;
import com.janeirodigital.shapetrees.core.ShapeTreeResponse;
import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.helpers.HttpHeaderHelper;
import okhttp3.*;

import javax.net.ssl.*;

public class OkHttpFetcher {
    private static final OkHttpClient baseClient = new OkHttpClient();
    private OkHttpClient httpClient;
    private static final String GET = "GET";
    private static final String PUT = "PUT";
    private static final String POST = "POST";
    private static final String PATCH = "PATCH";
    private static final String DELETE = "DELETE";

    private OkHttpFetcher(ShapeTreeClientConfiguration configuration) throws NoSuchAlgorithmException, KeyManagementException {
        OkHttpClient.Builder clientBuilder = baseClient.newBuilder();
        if (Boolean.TRUE.equals(configuration.getUseValidation())) {
            clientBuilder.interceptors().add(new ValidatingShapeTreeInterceptor());
        }
        if (Boolean.TRUE.equals(configuration.getSkipSslValidation())) {
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

    private static final ConcurrentHashMap<ShapeTreeClientConfiguration, OkHttpFetcher> clientMap999 = new ConcurrentHashMap<>();
    public static OkHttpFetcher getFetcher999(ShapeTreeClientConfiguration configuration) throws ShapeTreeException {
        if (clientMap999.containsKey(configuration)) {
            return clientMap999.get(configuration);
        }
        try {
            OkHttpFetcher client = new OkHttpFetcher(configuration);
            clientMap999.put(configuration, client);
            return client;
        } catch (Exception ex) {
            throw new ShapeTreeException(500, ex.getMessage());
        }
    }

    private okhttp3.Response fetch(String method, URI resourceURI, Map<String, List<String>> headers, String authorizationHeaderValue, String body, String contentType) throws ShapeTreeException {
        if (body == null)
            body = "";

        try {
            okhttp3.Request.Builder ohHttpReqBuilder = new okhttp3.Request.Builder();
            ohHttpReqBuilder.url(resourceURI.toURL());

            if (headers != null) {
                ohHttpReqBuilder.headers(FetchHelper.convertHeaders(headers));
            }

            if (authorizationHeaderValue != null) {
                ohHttpReqBuilder.addHeader(HttpHeaders.AUTHORIZATION.getValue(), authorizationHeaderValue);
            }

            switch (method) {

                case GET:
                    ohHttpReqBuilder.get();
                    break;

                case PUT:
                    ohHttpReqBuilder.put(okhttp3.RequestBody.create(body, okhttp3.MediaType.get(contentType)));
                    break;

                case POST:
                    ohHttpReqBuilder.post(okhttp3.RequestBody.create(body, okhttp3.MediaType.get(contentType)));
                    break;

                case PATCH:
                    ohHttpReqBuilder.patch(okhttp3.RequestBody.create(body, okhttp3.MediaType.get(contentType)));
                    break;

                case DELETE:
                    ohHttpReqBuilder.delete();
                    break;

                default:
                    throw new ShapeTreeException(500, "Unsupported HTTP method for resource creation");

            }

            return httpClient.newCall(ohHttpReqBuilder.build()).execute();
        } catch (IOException ex) {
            throw new ShapeTreeException(500, ex.getMessage());
        }
    }

    public ShapeTreeResource fetchShapeTreeResource(String method, URI resourceURI, Map<String, List<String>> headers, String authorizationHeaderValue, String body, String contentType) throws ShapeTreeException {
        okhttp3.Response response = fetch(method, resourceURI, headers, authorizationHeaderValue, body, contentType);
        return FetchHelper.mapFetchResponseToShapeTreeResource(response, resourceURI, headers);
    }

    public ShapeTreeResponse fetchShapeTreeResponse(String method, URI resourceURI, Map<String, List<String>> headers, String authorizationHeaderValue, String body, String contentType) throws ShapeTreeException {
        okhttp3.Response response = fetch(method, resourceURI, headers, authorizationHeaderValue, body, contentType);
        return FetchHelper.mapFetchResponseToShapeTreeResponse(response);
    }

    public void fetchIntoRemoteResource(String method, URI resourceURI, Map<String, List<String>> headers, String authorizationHeaderValue, String body, String contentType, RemoteResource remoteResource) throws IOException {
        okhttp3.Response response = fetch(method, resourceURI, headers, authorizationHeaderValue, body, contentType);

        remoteResource.setExists(response.code() < 400);

        // Parse the headers for ease of use later
        Map<String, List<String>> parsedHeaders = response.headers().toMultimap();
        remoteResource.setResponseHeaders(parsedHeaders);

        // We especially care about Link headers which require extra parsing of the rel values
        if (parsedHeaders.get(HttpHeaders.LINK.getValue()) != null) {
            remoteResource.setParsedLinkHeaders(HttpHeaderHelper.parseLinkHeadersToMap(response.headers(HttpHeaders.LINK.getValue())));
        } else {
            remoteResource.setParsedLinkHeaders(new HashMap<>());
        }

        // Save raw body
        try (ResponseBody respBody = response.body()) {
            remoteResource.setRawBody(respBody.string());
        }
    }
}
