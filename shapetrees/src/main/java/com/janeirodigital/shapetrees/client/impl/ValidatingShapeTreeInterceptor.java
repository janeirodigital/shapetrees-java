package com.janeirodigital.shapetrees.client.impl;

import com.janeirodigital.shapetrees.*;
import com.janeirodigital.shapetrees.client.ValidatingMethodHandler;
import lombok.extern.slf4j.Slf4j;
import okhttp3.*;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;

@Slf4j
public class ValidatingShapeTreeInterceptor implements Interceptor {

    private static final String POST = "POST";
    private static final String PUT = "PUT";
    private static final String PATCH = "PATCH";
    private static final String DELETE = "DELETE";
    private final ShapeTreeEcosystem ecosystem;

    public ValidatingShapeTreeInterceptor(ShapeTreeEcosystem ecosystem) {
        this.ecosystem = ecosystem;
    }

    @NotNull
    @Override
    public Response intercept(Chain chain) throws IOException {

        log.debug("Intercepting request for shape tree validation");

        Request request = chain.request();

        // Get the handler
        ValidatingMethodHandler handler = getHandler(request.method(), chain, this.ecosystem);
        if (handler != null) {
            try {
                return handler.process();
            } catch (Exception ex) {
                log.error("Error processing shape tree request: ", ex);
                if (ex instanceof ShapeTreeException) {
                    return createErrorResponse((ShapeTreeException)ex, request);
                } else {
                    return createErrorResponse(new ShapeTreeException(500, ex.getMessage()), request);
                }
            }
        } else {
            log.warn("No handler for method [{}] - passing through request", request.method());
            return chain.proceed(chain.request());
        }
    }

    /***
     * Get the appropriate handler to process the intercepted call based on the incoming HTTP method
     * @param method HTTP method of the intercepted request
     * @param chain The request chain being processed
     * @param ecosystem The ShapeTree ecosystem that should be used for processing
     * @return The appropriate handler class.  If no handler is found, return null.
     * @throws IOException I/O exception from construction of ValidationHandler
     */
    private ValidatingMethodHandler getHandler(String method, Chain chain, ShapeTreeEcosystem ecosystem) throws IOException {
        switch (method) {
            case POST:
                return new ValidatingPostMethodHandler(chain, ecosystem);
            case PUT:
                return new ValidatingPutMethodHandler(chain, ecosystem);
            case PATCH:
                return new ValidatingPatchMethodHandler(chain, ecosystem);
            case DELETE:
                return new ValidatingDeleteMethodHandler(chain, ecosystem);
        }
        return null;
    }

    // TODO: Update to a simple JSON-LD body
    private Response createErrorResponse(ShapeTreeException exception, Request request) {
        return new Response.Builder()
                .code(exception.getStatusCode())
                .body(ResponseBody.create(exception.getMessage(), MediaType.get("text/plain")))
                .request(request)
                .protocol(Protocol.HTTP_2)
                .message(exception.getMessage())
                .build();
    }
}
