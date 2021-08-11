package com.janeirodigital.shapetrees.client.fetch;

import com.janeirodigital.shapetrees.core.ResourceAccessor;
import com.janeirodigital.shapetrees.core.ShapeTreeRequest;
import com.janeirodigital.shapetrees.core.ShapeTreeResponse;
import com.janeirodigital.shapetrees.core.ShapeTreeValidationResponse;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.methodhandlers.*;
import lombok.extern.slf4j.Slf4j;
import okhttp3.*;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;

/**
 * Interceptor used for client-side validation
 */
@Slf4j
public class OkHttpValidatingShapeTreeInterceptor implements Interceptor {

    private static final String POST = "POST";
    private static final String PUT = "PUT";
    private static final String PATCH = "PATCH";
    private static final String DELETE = "DELETE";

    /**
     * Key method on Interceptor class which is implemented on an intercepted HTTP call.
     * Responsible for initializing a shape tree validation handler based on the HTTP method that
     * was intercepted.
     *
     * ShapeTreeResponse is used to determine whether an artificial response from the validation library should
     * be returned or if the original request should be passed through to the 'real' server.
     *
     * @param chain OkHttp request chain
     * @return Response to return back to intercepting chain
     * @throws IOException IOException thrown from chain.proceed
     */
    @NotNull
    @Override
    public Response intercept(@NotNull Chain chain) throws IOException {

        ShapeTreeRequest shapeTreeRequest = new OkHttpShapeTreeRequest(chain.request());
        ResourceAccessor resourceAccessor = new FetchRemoteResourceAccessor();

        // Get the handler
        ValidatingMethodHandler handler = getHandler(shapeTreeRequest.getMethod(), resourceAccessor);
        if (handler != null) {
            try {
                ShapeTreeValidationResponse shapeTreeResponse = handler.validateRequest(shapeTreeRequest);
                if (shapeTreeResponse.isValidRequest() && !shapeTreeResponse.isRequestFulfilled()) {
                    return chain.proceed(chain.request());
                } else {
                    return createResponse(shapeTreeRequest, chain.request(), shapeTreeResponse);
                }
            } catch (ShapeTreeException ex){
                log.error("Error processing shape tree request: ", ex);
                return createErrorResponse(ex, shapeTreeRequest, chain.request());
            } catch (Exception ex) {
                log.error("Error processing shape tree request: ", ex);
                return createErrorResponse(new ShapeTreeException(500, ex.getMessage()), shapeTreeRequest, chain.request());
            }
        } else {
            log.warn("No handler for method [{}] - passing through request", shapeTreeRequest.getMethod());
            return chain.proceed(chain.request());
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
    private Response createErrorResponse(ShapeTreeException exception, ShapeTreeRequest request, Request nativeRequest) {
        return new Response.Builder()
                .code(exception.getStatusCode())
                .body(ResponseBody.create(exception.getMessage(), MediaType.get("text/plain")))
                .request(nativeRequest)
                .protocol(Protocol.HTTP_2)
                .message(exception.getMessage())
                .build();
    }

    private Response createResponse(ShapeTreeRequest request, Request nativeRequest, ShapeTreeResponse response) {
        Response.Builder builder = new Response.Builder();
        builder.code(response.getStatusCode());
        Headers headers = OkHttpFetcher.convertHeaders(response.getResponseHeaders());
        builder.headers(headers);
        String contentType = headers.get("Content-Type");
        if (contentType == null) {
            contentType = "text/turtle";
        }

        builder.body(ResponseBody.create(response.getBody(), MediaType.get(contentType)))
                .protocol(Protocol.HTTP_2)
                .message("Success")
                .request(nativeRequest);

        return builder.build();
    }

    private class FetchShapeTreeRequest implements ShapeTreeRequest {
        private final Request request;
        private ShapeTreeResourceType resourceType;

        public FetchShapeTreeRequest(Request request) {
            this.request = request;
        }

        @Override
        public String getMethod() {
            return this.request.method();
        }

        @Override
        public URI getURI() {
            return this.request.url().uri();
        }

        @Override
        public Map<String, List<String>> getHeaders() {
            return this.request.headers().toMultimap();
        }

        @Override
        public Map<String, List<String>> getLinkHeaders() {
            return HttpHeaderHelper.parseLinkHeadersToMap(this.getHeaderValues(HttpHeaders.LINK.getValue()));
        }

        @Override
        public List<String> getHeaderValues(String header) {
            return this.request.headers(header);
        }

        @Override
        public String getHeaderValue(String header) {
            return this.request.header(header);
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
            try (Buffer buffer = new Buffer()) {
                if (this.request.body() != null) {
                    Objects.requireNonNull(this.request.body()).writeTo(buffer);
                }
                return buffer.readUtf8();
            } catch (IOException | NullPointerException ex) {
                log.error("Error writing body to string");
                return null;
            }
        }
    }
}
