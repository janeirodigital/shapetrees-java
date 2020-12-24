package com.janeirodigital.shapetrees.client.okhttp;

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
public class ValidatingShapeTreeInterceptor implements Interceptor {

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

        ShapeTreeRequest<Request> shapeTreeRequest = new OkHttpShapeTreeRequest(chain.request());
        ResourceAccessor resourceAccessor = new OkHttpRemoteResourceAccessor();

        // Get the handler
        ValidatingMethodHandler handler = getHandler(shapeTreeRequest.getMethod(), resourceAccessor);
        if (handler != null) {
            try {
                ShapeTreeValidationResponse shapeTreeResponse = handler.validateRequest(shapeTreeRequest);
                if (shapeTreeResponse.isValidRequest() && !shapeTreeResponse.isRequestFulfilled()) {
                    return chain.proceed(chain.request());
                } else {
                    return createResponse(shapeTreeRequest, shapeTreeResponse);
                }
            } catch (Exception ex) {
                log.error("Error processing shape tree request: ", ex);
                if (ex instanceof ShapeTreeException) {
                    return createErrorResponse((ShapeTreeException)ex, shapeTreeRequest);
                } else {
                    return createErrorResponse(new ShapeTreeException(500, ex.getMessage()), shapeTreeRequest);
                }
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
        }
        return null;
    }

    // TODO: Update to a simple JSON-LD body
    private Response createErrorResponse(ShapeTreeException exception, ShapeTreeRequest<Request> request) {
        return new Response.Builder()
                .code(exception.getStatusCode())
                .body(ResponseBody.create(exception.getMessage(), MediaType.get("text/plain")))
                .request(request.getNativeRequest())
                .protocol(Protocol.HTTP_2)
                .message(exception.getMessage())
                .build();
    }

    private Response createResponse(ShapeTreeRequest<Request> request, ShapeTreeResponse response) {
        Response.Builder builder = new Response.Builder();
        builder.code(response.getStatusCode());
        Headers headers = OkHttpHelper.convertHeaders(response.getResponseHeaders());
        builder.headers(headers);
        String contentType = headers.get("Content-Type");
        if (contentType == null) {
            contentType = "text/turtle";
        }

        builder.body(ResponseBody.create(response.getBody(), MediaType.get(contentType)))
                .protocol(Protocol.HTTP_2)
                .message("Success")
                .request(request.getNativeRequest());

        return builder.build();
    }

}