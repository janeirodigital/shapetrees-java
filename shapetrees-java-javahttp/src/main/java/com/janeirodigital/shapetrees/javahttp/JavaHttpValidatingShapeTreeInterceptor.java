package com.janeirodigital.shapetrees.javahttp;

import com.janeirodigital.shapetrees.client.http.HttpRemoteResourceAccessor;
import com.janeirodigital.shapetrees.core.*;
import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.methodhandlers.*;
import lombok.AllArgsConstructor;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.jetbrains.annotations.NotNull;

import javax.net.ssl.SSLSession;
import java.io.IOException;
import java.net.URI;
import java.net.http.HttpResponse;
import java.util.*;

/**
 * Wrapper used for client-side validation
 */
@Slf4j
public class JavaHttpValidatingShapeTreeInterceptor {

    private static final String POST = "POST";
    private static final String PUT = "PUT";
    private static final String PATCH = "PATCH";
    private static final String DELETE = "DELETE";

    // public java.net.http.HttpResponse intercept(@NotNull Chain chain) throws IOException {}

    @NotNull
    public java.net.http.HttpResponse validatingWrap(java.net.http.HttpRequest clientRequest, java.net.http.HttpClient httpClient, String body, String contentType) throws IOException, InterruptedException {

        ShapeTreeRequest shapeTreeRequest = new JavaHttpShapeTreeRequest(clientRequest, body, contentType);
        ResourceAccessor resourceAccessor = new HttpRemoteResourceAccessor();

        // Get the handler
        ValidatingMethodHandler handler = getHandler(shapeTreeRequest.getMethod(), resourceAccessor);
        if (handler != null) {
            try {
                ShapeTreeValidationResponse shapeTreeResponse = handler.validateRequest(shapeTreeRequest);
                if (shapeTreeResponse.isValidRequest() && !shapeTreeResponse.isRequestFulfilled()) {
                    return httpClient.send(clientRequest, java.net.http.HttpResponse.BodyHandlers.ofString());
                } else {
                    return createResponse(shapeTreeRequest, clientRequest, shapeTreeResponse);
                }
            } catch (ShapeTreeException ex){
                log.error("Error processing shape tree request: ", ex);
                return createErrorResponse(ex, shapeTreeRequest, clientRequest);
            } catch (Exception ex) {
                log.error("Error processing shape tree request: ", ex);
                return createErrorResponse(new ShapeTreeException(500, ex.getMessage()), shapeTreeRequest, clientRequest);
            }
        } else {
            log.warn("No handler for method [{}] - passing through request", shapeTreeRequest.getMethod());
            return httpClient.send(clientRequest, java.net.http.HttpResponse.BodyHandlers.ofString());
        }
    }

    private ValidatingMethodHandler getHandler(String requestMethod, ResourceAccessor resourceAccessor) {
        switch (requestMethod) {
            case POST:
                return new ValidatingPostMethodHandler(resourceAccessor);
            case PUT:
                return new ValidatingPutMethodHandler(resourceAccessor);
            case PATCH:
                return new ValidatingPatchMethodHandler(resourceAccessor);
            case DELETE:
                return new ValidatingDeleteMethodHandler(resourceAccessor);
            default:
                return null;
        }
    }

    // TODO: Update to a simple JSON-LD body
    private java.net.http.HttpResponse createErrorResponse(ShapeTreeException exception, ShapeTreeRequest request, java.net.http.HttpRequest nativeRequest) {
        // java.net.http.HttpHeaders headers = new java.net.http.HttpHeaders();
        // headers.set("Content-type", "text/plain");
        return new MyHttpResponse(exception.getStatusCode(), nativeRequest, null, exception.getMessage());
    }

    @SneakyThrows
    private java.net.http.HttpResponse createResponse(ShapeTreeRequest request, java.net.http.HttpRequest nativeRequest, ShapeTreeResponse response) {
        // [ericP] I don't how to create a mutable HttpHeaders object so I make HttoRequest do it for me.
        java.net.http.HttpRequest.Builder tempRequestBuilder = java.net.http.HttpRequest.newBuilder().uri(new URI("http://a.example/"));
        for (Map.Entry<String, List<String>> entry : response.getHeaders().entrySet()){
            for (String value : entry.getValue()) {
                tempRequestBuilder.header(entry.getKey(), value);
            }
        }
        return new MyHttpResponse(response.getStatusCode(), nativeRequest, tempRequestBuilder.build().headers(), response.getBody());
    }

    private class JavaHttpShapeTreeRequest implements ShapeTreeRequest {
        private final java.net.http.HttpRequest request;
        private ShapeTreeResourceType resourceType;
        private String body;
        private String contentType;
        private HttpClientHeaders headers;

        public JavaHttpShapeTreeRequest(java.net.http.HttpRequest request, String body, String contentType) {
            this.request = request;
            this.body = body;
            this.contentType = contentType;
            TreeMap<String, List<String>> tm = new TreeMap<>(String.CASE_INSENSITIVE_ORDER);
            Map<String, List<String>> headerMap = this.request.headers().map();
            for (Map.Entry<String, List<String>> entry : headerMap.entrySet()) {
                tm.put(entry.getKey(), entry.getValue());
            }
            this.headers = new HttpClientHeaders(tm);
        }

        @Override
        public String getMethod() {
            return this.request.method();
        }

        @Override
        public URI getURI() {
            return this.request.uri();
        }

        @Override
        public HttpClientHeaders getHeaders() {
            return this.headers;
        }

        @Override
        public HttpClientHeaders getLinkHeaders() {
            return HttpClientHeaders.parseLinkHeaders(this.getHeaderValues(HttpHeaders.LINK.getValue()));
        }

        @Override
        public List<String> getHeaderValues(String header) {
            return this.request.headers().allValues(header);
        }

        @Override
        public String getHeaderValue(String header) {
            return String.valueOf(this.request.headers().firstValue(header));
        }

        @Override
        public String getContentType() {
            if (this.getHeaders().containsKey(HttpHeaders.CONTENT_TYPE.getValue())) {
                return this.getHeaders().get(HttpHeaders.CONTENT_TYPE.getValue()).stream().findFirst().orElse(null);
            }
            return null;
        }

        @Override
        public ShapeTreeResourceType getResourceType() {
            return this.resourceType;
        }

        @Override
        public void setResourceType(ShapeTreeResourceType resourceType) {
            this.resourceType = resourceType;
        }

        @Override
        public String getBody() {
            return this.body;
        }
    }

    @AllArgsConstructor
    private class MyHttpResponse implements java.net.http.HttpResponse {
        // private URI _uri;
        private int _statusCode;
        private java.net.http.HttpRequest _request;
        private java.net.http.HttpHeaders _headers;
        private String _body;

        @Override
        public int statusCode() {
            return _statusCode;
        }

        @Override
        public java.net.http.HttpRequest request() {
            return _request;
        }

        @Override
        public Optional<HttpResponse<String>> previousResponse() {
            return Optional.empty();
        }

        @Override
        public java.net.http.HttpHeaders headers() {
            return _headers;
        }

        @Override
        public String body() {
            return _body;
        }

        @Override
        public Optional<SSLSession> sslSession() {
            return Optional.empty();
        }

        @Override
        public URI uri() {
            return null;
        }

        @Override
        public java.net.http.HttpClient.Version version() {
            return null;
        }
    };
}
