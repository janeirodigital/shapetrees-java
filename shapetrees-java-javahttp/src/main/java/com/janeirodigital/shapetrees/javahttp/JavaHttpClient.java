package com.janeirodigital.shapetrees.javahttp;

import com.janeirodigital.shapetrees.client.http.HttpClient;
import com.janeirodigital.shapetrees.client.http.HttpRequest;
import com.janeirodigital.shapetrees.client.http.HttpShapeTreeClient;
import com.janeirodigital.shapetrees.core.HttpClientHeaders;
import com.janeirodigital.shapetrees.client.http.HttpRemoteResource;
import com.janeirodigital.shapetrees.core.HttpClientHeaders;
import com.janeirodigital.shapetrees.core.ShapeTreeResource;
import com.janeirodigital.shapetrees.core.ShapeTreeResponse;
import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.core.enums.LinkRelations;
import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.vocabularies.LdpVocabulary;
import lombok.extern.slf4j.Slf4j;

import javax.net.ssl.*;
import java.io.IOException;
import java.net.URI;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * java.net.http implementation of HttpClient
 */
@Slf4j
public class JavaHttpClient extends HttpClient {
    private static final boolean USE_INTERCEPTOR = false;
    private java.net.http.HttpClient httpClient;
    private JavaHttpValidatingShapeTreeInterceptor validatingWrapper;

    /**
     * Maps a java.net.http.HttpResponse object to a ShapeTreeResource object
     * @param response java.net.http.HttpResponse object
     * @param resourceURI URI of request associated with response
     * @param headers Request headers used in request associated with response
     * @return ShapeTreeResource instance with contents and response headers from response
     */
    public ShapeTreeResource fetchShapeTreeResource(HttpRequest request) throws ShapeTreeException {
        java.net.http.HttpResponse response = fetch(request.method, request.resourceURI, request.headers, request.body, request.contentType);

        ShapeTreeResource shapeTreeResource = new ShapeTreeResource();

        shapeTreeResource.setExists(response.statusCode()/100 == 2);
        shapeTreeResource.setContainer(isContainerFromHeaders(request.headers));
        shapeTreeResource.setType(getResourceTypeFromHeaders(request.headers));

        try {
            shapeTreeResource.setBody(Objects.requireNonNull(response.body()).toString());
        } catch (NullPointerException ex) {
            log.error("Exception retrieving body string");
            shapeTreeResource.setBody(null);
        }
        shapeTreeResource.setAttributes(new HttpClientHeaders(response.headers().map()));
        shapeTreeResource.setUri(URI.create(Objects.requireNonNull(response.headers().firstValue(HttpHeaders.LOCATION.getValue()).orElse(request.resourceURI.toString()))));

        return shapeTreeResource;
    }

    /**
     * Maps an HttpResponse object to a ShapeTreeResponse object
     * @return ShapeTreeResponse with values from HttpResponse
     */
    public ShapeTreeResponse fetchShapeTreeResponse(HttpRequest request) throws ShapeTreeException {
        java.net.http.HttpResponse response = fetch(request.method, request.resourceURI, request.headers, request.body, request.contentType);

        ShapeTreeResponse shapeTreeResponse = new ShapeTreeResponse();
        try {
            shapeTreeResponse.setBody(Objects.requireNonNull(response.body()).toString());
        } catch (NullPointerException ex) {
            log.error("Exception retrieving body string");
            shapeTreeResponse.setBody(null);
        }
        shapeTreeResponse.setHeaders(new HttpClientHeaders(response.headers().map()));
        shapeTreeResponse.setStatusCode(response.statusCode());
        return shapeTreeResponse;
    }

    public void fetchIntoRemoteResource(HttpRequest request, HttpRemoteResource remoteResource) throws IOException {
        java.net.http.HttpResponse response = fetch(request.method, request.resourceURI, request.headers, request.body, request.contentType);

        remoteResource.setExists(response.statusCode() < 400);

        // Parse the headers for ease of use later
        HttpClientHeaders parsedHeaders = new HttpClientHeaders(response.headers().map());
        remoteResource.setResponseHeaders(parsedHeaders);

        // We especially care about Link headers which require extra parsing of the rel values
        if (parsedHeaders.get(HttpHeaders.LINK.getValue()) != null) {
            remoteResource.setParsedLinkHeaders(HttpClientHeaders.parseLinkHeaders(response.headers().allValues(HttpHeaders.LINK.getValue())));
        } else {
            remoteResource.setParsedLinkHeaders(new HttpClientHeaders());
        }

        // Save raw body
        String respBody = Objects.requireNonNull(response.body()).toString(); // @@ is requireNull useful here?
        remoteResource.setRawBody(respBody);
    }

    // constructor and its helpers
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
    private java.net.http.HttpResponse fetch(String method, URI resourceURI, HttpClientHeaders headers, String body, String contentType) throws ShapeTreeException {
        if (body == null)
            body = "";

        try {
            java.net.http.HttpRequest.Builder requestBuilder = java.net.http.HttpRequest.newBuilder();
            requestBuilder.uri(resourceURI);

            if (headers != null) {
                for (Map.Entry<String, List<String>> entry : headers.entrySet()){
                    for (String value : entry.getValue()) {
                        try {
                            requestBuilder.header(entry.getKey(), value);
                        } catch (IllegalArgumentException ex) {
                            // current illegal client headers: connection, content-length, date, expect, from, host, upgrade, via, warning
                        }
                    }
                }
            }

            switch (method) {

                case GET:
                    requestBuilder.GET();
                    break;

                case PUT:
                    requestBuilder.PUT(java.net.http.HttpRequest.BodyPublishers.ofString(body));
                    requestBuilder.header("Content-Type", contentType);
                    break;

                case POST:
                    requestBuilder.POST(java.net.http.HttpRequest.BodyPublishers.ofString(body));
                    requestBuilder.header("Content-Type", contentType);
                    break;

                case PATCH:
                    requestBuilder.method("PATCH", java.net.http.HttpRequest.BodyPublishers.ofString(body));
                    requestBuilder.header("Content-Type", contentType);
                    break;

                case DELETE:
                    requestBuilder.DELETE();
                    break;

                default:
                    throw new ShapeTreeException(500, "Unsupported HTTP method for resource creation");

            }

            java.net.http.HttpRequest request = requestBuilder.build();
            if (validatingWrapper == null) {
                return httpClient.send(request, java.net.http.HttpResponse.BodyHandlers.ofString());
            } else {
                return validatingWrapper.validatingWrap(request, httpClient, body, contentType);
            }
        } catch (IOException | InterruptedException ex) {
            throw new ShapeTreeException(500, ex.getMessage());
        }
    }
}
