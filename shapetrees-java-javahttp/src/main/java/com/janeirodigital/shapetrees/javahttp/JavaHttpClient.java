package com.janeirodigital.shapetrees.javahttp;

import com.janeirodigital.shapetrees.client.http.HttpClient;
import com.janeirodigital.shapetrees.client.http.HttpRequest;
import com.janeirodigital.shapetrees.core.DocumentResponse;
import com.janeirodigital.shapetrees.core.ResourceAttributes;
import com.janeirodigital.shapetrees.client.http.HttpRemoteResource;
import com.janeirodigital.shapetrees.core.ShapeTreeResource;
import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import lombok.extern.slf4j.Slf4j;

import javax.net.ssl.*;
import java.io.IOException;
import java.net.URI;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

/**
 * java.net.http implementation of HttpClient
 */
@Slf4j
public class JavaHttpClient implements HttpClient {
    private static final boolean USE_INTERCEPTOR = false;
    private java.net.http.HttpClient httpClient;
    private JavaHttpValidatingShapeTreeInterceptor validatingWrapper;

    /**
     * Execute an HTTP request to create a DocumentResponse object
     * Implements `HttpClient` interface
     * @param request an HTTP request with appropriate headers for ShapeTree interactions
     * @return new DocumentResponse with response headers and contents
     * @throws ShapeTreeException
     */
    @Override
    public DocumentResponse fetchShapeTreeResponse(HttpRequest request) throws ShapeTreeException {
        java.net.http.HttpResponse response = fetch(request);

        String body = null;
        try {
            body = Objects.requireNonNull(response.body()).toString();
        } catch (NullPointerException ex) {
            log.error("Exception retrieving body string");
        }
        return new DocumentResponse(new ResourceAttributes(response.headers().map()), body, response.statusCode());
    }

    /**
     * Construct an JavaHttpClient with switches to enable or disable SSL and ShapeTree validation
     * @param useSslValidation
     * @param useShapeTreeValidation
     * @throws NoSuchAlgorithmException potentially thrown while disabling SSL validation
     * @throws KeyManagementException potentially thrown while disabling SSL validation
     */
    protected JavaHttpClient(boolean useSslValidation, boolean useShapeTreeValidation) throws NoSuchAlgorithmException, KeyManagementException {
        java.net.http.HttpClient.Builder clientBuilder = java.net.http.HttpClient.newBuilder();
        validatingWrapper = null;
        if (Boolean.TRUE.equals(useShapeTreeValidation)) {
            validatingWrapper = new JavaHttpValidatingShapeTreeInterceptor();
        }
        if (Boolean.FALSE.equals(useSslValidation)) {
            TrustManager[] trustAllCerts = new TrustManager[] {
                    new X509TrustManager() {
                        public java.security.cert.X509Certificate[] getAcceptedIssuers() {
                            return null;
                        }
                        @Override
                        public void checkClientTrusted(X509Certificate[] arg0, String arg1)
                                throws CertificateException {}

                        @Override
                        public void checkServerTrusted(X509Certificate[] arg0, String arg1)
                                throws CertificateException {}

                    }
            };

            SSLContext sc=null;
            try {
                sc = SSLContext.getInstance("SSL");
            } catch (NoSuchAlgorithmException e) {
                e.printStackTrace();
            }
            try {
                sc.init(null, trustAllCerts, new java.security.SecureRandom());
            } catch (KeyManagementException e) {
                e.printStackTrace();
            }
            HttpsURLConnection.setDefaultSSLSocketFactory(sc.getSocketFactory());

            // Create all-trusting host name verifier
            HostnameVerifier validHosts = new HostnameVerifier() {
                @Override
                public boolean verify(String arg0, SSLSession arg1) {
                    return true;
                }
            };
            // All hosts will be valid
            HttpsURLConnection.setDefaultHostnameVerifier(validHosts);
        }
        httpClient = clientBuilder.build();
    }

    /**
     * Internal function to execute HTTP request and return java.net.http response
     * @param request
     * @return
     * @throws ShapeTreeException
     */
    private java.net.http.HttpResponse fetch(HttpRequest request) throws ShapeTreeException {
        if (request.body == null)
            request.body = "";

        try {
            java.net.http.HttpRequest.Builder requestBuilder = java.net.http.HttpRequest.newBuilder();
            requestBuilder.uri(request.resourceURI);

            if (request.headers != null) {
                String[] headerList = request.headers.toList("connection", "content-length", "date", "expect", "from", "host", "upgrade", "via", "warning");
                if (headerList.length > 0) {
                    requestBuilder.headers(headerList);
                }
                /* TODO: decide between above and below. Above requires precient knowledge of illegal client
                   headers (which tend to be there 'cause we re-use the server headers as client headers.
                for (Map.Entry<String, List<String>> entry : request.headers.toMultimap().entrySet()){
                    for (String value : entry.getValue()) {
                        try {
                            requestBuilder.header(entry.getKey(), value);
                        } catch (IllegalArgumentException ex) {
                            // current illegal client headers: connection, content-length, date, expect, from, host, upgrade, via, warning
                        }
                    }
                }
                 */
            }

            switch (request.method) {
                case HttpClient.GET:
                case HttpClient.DELETE:
                    requestBuilder.method(request.method, java.net.http.HttpRequest.BodyPublishers.noBody());
                    break;

                case HttpClient.PUT:
                case HttpClient.POST:
                case HttpClient.PATCH:
                    requestBuilder.method(request.method, java.net.http.HttpRequest.BodyPublishers.ofString(request.body));
                    requestBuilder.header("Content-Type", request.contentType);
                    break;

                default:
                    throw new ShapeTreeException(500, "Unsupported HTTP method for resource creation");
            }

            java.net.http.HttpRequest nativeRequest = requestBuilder.build();
            if (validatingWrapper == null) {
                return httpClient.send(nativeRequest, java.net.http.HttpResponse.BodyHandlers.ofString());
            } else {
                return validatingWrapper.validatingWrap(nativeRequest, httpClient, request.body, request.contentType);
            }
        } catch (IOException | InterruptedException ex) {
            throw new ShapeTreeException(500, ex.getMessage());
        }
    }
}
