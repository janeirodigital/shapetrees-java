package com.janeirodigital.shapetrees.okhttp;

import com.janeirodigital.shapetrees.core.*;
import com.janeirodigital.shapetrees.core.enums.HttpHeader;
import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.methodhandlers.*;
import lombok.extern.slf4j.Slf4j;
import okhttp3.*;
import okio.Buffer;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.net.URL;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import static com.janeirodigital.shapetrees.okhttp.OkHttpHelper.attributesToHeaders;

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
     * DocumentResponse is used to determine whether an artificial response from the validation library should
     * be returned or if the original request should be passed through to the 'real' server.
     *
     * @param chain OkHttp request chain
     * @return Response to return back to intercepting chain
     * @throws IOException IOException thrown from chain.proceed
     */
    @NotNull
    @Override
    public Response intercept(@NotNull Chain chain) throws IOException {

        log.info("Intercepting {} request to {} for client-side validation: ", chain.request().method(), chain.request().url());

        ShapeTreeRequest shapeTreeRequest = new OkHttpShapeTreeRequest(chain.request());
        ResourceAccessor resourceAccessor = new OkHttpResourceAccessor();

        // Get the handler
        ValidatingMethodHandler handler = getHandler(shapeTreeRequest.getMethod(), resourceAccessor);
        if (handler != null) {
            try {
                Optional<DocumentResponse> shapeTreeResponse = handler.validateRequest(shapeTreeRequest);
                if (!shapeTreeResponse.isPresent()) {
                    log.info("Client-side validation successful. Passing {} request to {} through to server", shapeTreeRequest.getMethod(), shapeTreeRequest.getUrl());
                    return check(chain.proceed(chain.request()));
                } else {
                    return createResponse(chain.request(), shapeTreeResponse.get());
                }
            } catch (ShapeTreeException ex){
                return createErrorResponse(ex, chain.request());
            } catch (Exception ex) {
                return createErrorResponse(new ShapeTreeException(500, ex.getMessage()), chain.request());
            }
        } else {
            log.warn("No handler for method [{}] - passing through request to {}", shapeTreeRequest.getMethod(), shapeTreeRequest.getUrl());
            return check(chain.proceed(chain.request()));
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

    private Response createErrorResponse(ShapeTreeException exception, Request nativeRequest) {
        log.error("Error processing shape tree request: ", exception);
        return new Response.Builder()
                .code(exception.getStatusCode())
                .body(ResponseBody.create(exception.getMessage(), MediaType.get("text/plain")))
                .request(nativeRequest)
                .protocol(Protocol.HTTP_2)
                .message(exception.getMessage())
                .build();
    }

    private Response createResponse(Request nativeRequest, DocumentResponse response) {
        Response.Builder builder = new Response.Builder();
        builder.code(response.getStatusCode());
        ResourceAttributes responseHeaders = response.getResourceAttributes();
        builder.headers(attributesToHeaders(responseHeaders));
        String contentType = responseHeaders.firstValue(HttpHeader.CONTENT_TYPE.getValue()).orElse("text/turtle");

        log.info("Client-side validation successful. {} request was fulfilled to {}", nativeRequest.method(), nativeRequest.url());

        builder.body(ResponseBody.create(response.getBody(), MediaType.get(contentType)))
                .protocol(Protocol.HTTP_2)
                .message("Success")
                .request(nativeRequest);

        return builder.build();
    }

    private Response check(Response response) throws IOException {
        if (response.code() > 599) {
            throw new IOException("Invalid HTTP response: " + response + (response.body() == null ? "" : "\n" + response.body()));
        }
        return response;
    }

    private class OkHttpShapeTreeRequest implements ShapeTreeRequest {
        private final Request request;
        private ShapeTreeResourceType resourceType;

        public OkHttpShapeTreeRequest(Request request) {
            this.request = request;
        }

        @Override
        public String getMethod() {
            return this.request.method();
        }

        @Override
        public URL getUrl() {
            return this.request.url().url();
        }

        @Override
        public ResourceAttributes getHeaders() {
            return new ResourceAttributes(this.request.headers().toMultimap());
        }

        @Override
        public RelationAttributes getLinkHeaders() {
            return ResourceAttributes.parseLinkHeaders(this.getHeaderValues(HttpHeader.LINK.getValue()));
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
            return this.getHeaders().firstValue(HttpHeader.CONTENT_TYPE.getValue()).orElse(null);
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
