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

    @NotNull
    public java.net.http.HttpResponse validatingWrap(java.net.http.HttpRequest clientRequest, java.net.http.HttpClient httpClient, @NotNull Optional<String> body, String contentType) throws IOException, InterruptedException {

        ShapeTreeRequest shapeTreeRequest = new JavaHttpShapeTreeRequest(clientRequest, body, contentType);
        ResourceAccessor resourceAccessor = new HttpRemoteResourceAccessor();

        // Get the handler
        ValidatingMethodHandler handler = getHandler(shapeTreeRequest.getMethod(), resourceAccessor);
        if (handler != null) {
            try {
                Optional<DocumentResponse> shapeTreeResponse = handler.validateRequest(shapeTreeRequest);
                if (!shapeTreeResponse.isPresent()) {
                    return JavaHttpClient.check(httpClient.send(clientRequest, java.net.http.HttpResponse.BodyHandlers.ofString()));
                } else {
                    return createResponse(shapeTreeRequest, clientRequest, shapeTreeResponse.get());
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
            return JavaHttpClient.check(httpClient.send(clientRequest, java.net.http.HttpResponse.BodyHandlers.ofString()));
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

    // TODO: Spec/API: send error response as a structured JSON-LD body
    private java.net.http.HttpResponse createErrorResponse(ShapeTreeException exception, ShapeTreeRequest request, java.net.http.HttpRequest nativeRequest) {
        // java.net.http.ResourceAttributes headers = new java.net.http.ResourceAttributes();
        // headers.set("Content-type", "text/plain");
        return new MyHttpResponse(exception.getStatusCode(), nativeRequest, java.net.http.HttpHeaders.of(Collections.emptyMap(), (a, v) -> true), exception.getMessage());
    }

    @SneakyThrows
    private java.net.http.HttpResponse createResponse(ShapeTreeRequest request, java.net.http.HttpRequest nativeRequest, DocumentResponse response) {
        java.net.http.HttpHeaders headers = java.net.http.HttpHeaders.of(response.getResourceAttributes().toMultimap(), (a, v) -> true);
        return new MyHttpResponse(response.getStatusCode(), nativeRequest, headers, response.getBody());
    }

    private class JavaHttpShapeTreeRequest implements ShapeTreeRequest {
        private final java.net.http.HttpRequest request;
        private ShapeTreeResourceType resourceType;
        @NotNull private final Optional<String> body;
        private final String contentType;
        private final ResourceAttributes headers;

        public JavaHttpShapeTreeRequest(java.net.http.HttpRequest request, @NotNull Optional<String> body, String contentType) {
            this.request = request;
            this.body = body;
            this.contentType = contentType;
            TreeMap<String, List<String>> tm = new TreeMap<>(String.CASE_INSENSITIVE_ORDER);
            Map<String, List<String>> headerMap = this.request.headers().map();
            for (Map.Entry<String, List<String>> entry : headerMap.entrySet()) {
                tm.put(entry.getKey(), entry.getValue());
            }
            this.headers = new ResourceAttributes(tm);
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
        public ResourceAttributes getHeaders() {
            return this.headers;
        }

        @NotNull
        @Override
        public ResourceAttributes getLinkHeaders() {
            return ResourceAttributes.parseLinkHeaders(this.request.headers().allValues(HttpHeaders.LINK.getValue()));
        }

        @Override
        public Optional<String> getHeaderValue(String header) {
            return this.request.headers().firstValue(header);
        }

        @Override
        public @NotNull String expectContentType() throws ShapeTreeException {
            return this.getHeaders().firstValue(HttpHeaders.CONTENT_TYPE.getValue()).orElseThrow(
                    () -> new ShapeTreeException(400, "Content-Type is required")
            );
        }

        @Override
        public ShapeTreeResourceType getResourceType() {
            return this.resourceType;
        }

        @Override
        public void setResourceType(ShapeTreeResourceType resourceType) {
            this.resourceType = resourceType;
        }

        @NotNull
        @Override
        public Optional<String> getBody() {
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
            return this._statusCode;
        }

        @Override
        public java.net.http.HttpRequest request() {
            return this._request;
        }

        @Override
        public Optional<HttpResponse<String>> previousResponse() {
            return Optional.empty();
        }

        @Override
        public java.net.http.HttpHeaders headers() {
            return this._headers;
        }

        @Override
        public String body() {
            return this._body;
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
    }
}
