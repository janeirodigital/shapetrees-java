package com.janeirodigital.shapetrees.client.okhttp;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.helpers.RequestHelper;
import com.janeirodigital.shapetrees.core.validation.ShapeTreeContext;
import com.janeirodigital.shapetrees.core.validation.ShapeTreeRequest;
import com.janeirodigital.shapetrees.core.resources.ManageableInstance;
import com.janeirodigital.shapetrees.core.resources.ManagerResource;
import com.janeirodigital.shapetrees.core.resources.ResourceAccessor;
import com.janeirodigital.shapetrees.core.validation.ValidationResult;
import lombok.extern.slf4j.Slf4j;
import okhttp3.Interceptor;
import okhttp3.Request;
import okhttp3.Response;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.util.Optional;

import static com.janeirodigital.shapetrees.core.resources.ManageableInstance.getInstance;
import static com.janeirodigital.shapetrees.core.resources.ManageableInstance.reloadInstance;
import static com.janeirodigital.shapetrees.core.validation.ShapeTreeRequestHandler.manageShapeTreeAssignment;
import static com.janeirodigital.shapetrees.client.okhttp.OkHttpHelper.*;

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
        ResourceAccessor resourceAccessor = new OkHttpResourceAccessor();

        ShapeTreeRequest shapeTreeRequest;
        ShapeTreeContext shapeTreeContext;
        ManageableInstance requestInstance;
        try {
            shapeTreeRequest = new OkHttpShapeTreeRequest(chain.request());
            shapeTreeContext = RequestHelper.buildContextFromRequest(shapeTreeRequest);
            requestInstance = getInstance(resourceAccessor, shapeTreeContext, shapeTreeRequest.getUrl());
        } catch (ShapeTreeException ex) {
            throw new IOException("Failed to initiate shape tree processing for " + chain.request().url() + ": " + ex.getMessage());
        }

        ValidationResult validationResult;
        if (requestInstance.wasRequestForManager()) {
            // Target resource is for shape tree manager - manage shape trees to plant and/or unplant
            try {
                validationResult = manageShapeTreeAssignment(resourceAccessor, requestInstance, shapeTreeRequest);
                if (!validationResult.isValid()) { return createInvalidResponse(chain.request(), validationResult); }
                return createManagerResponse(chain.request(), requestInstance, validationResult);
            } catch (ShapeTreeException ex) {
                return createErrorResponse(chain.request(), ex);
            }
        }

        // Get the handler
        Optional<OkHttpValidatingMethodHandler> handler = getHandler(shapeTreeRequest.getMethod(), resourceAccessor);
        if (handler.isPresent()) {
            try {
                Optional<Response> optionalResponse = handler.get().validateRequest(chain.request(), shapeTreeRequest, shapeTreeContext, requestInstance);
                if (!optionalResponse.isPresent()) {
                    log.info("Client-side validation successful. Passing {} request to {} through to server", shapeTreeRequest.getMethod(), shapeTreeRequest.getUrl());
                    return check(chain.proceed(chain.request()));
                } else {
                    return optionalResponse.get();
                }
            } catch (ShapeTreeException ex){
                return createErrorResponse(chain.request(), ex);
            } catch (Exception ex) {
                return createErrorResponse(chain.request(), new ShapeTreeException(500, ex.getMessage()));
            }
        } else {
            log.warn("No handler for method [{}] - passing through request to {}", shapeTreeRequest.getMethod(), shapeTreeRequest.getUrl());
            return check(chain.proceed(chain.request()));
        }
    }

    private Optional<OkHttpValidatingMethodHandler> getHandler(String requestMethod, ResourceAccessor resourceAccessor) {
        switch (requestMethod) {
            case POST:
                return Optional.of(new OkHttpValidatingPostMethodHandler(resourceAccessor));
            case PUT:
                return Optional.of(new OkHttpValidatingPutMethodHandler(resourceAccessor));
            case PATCH:
                return Optional.of(new OkHttpValidatingPatchMethodHandler(resourceAccessor));
            default:
                return Optional.empty();
        }
    }

    private Response createManagerResponse(Request nativeRequest, ManageableInstance requestInstance, ValidationResult validationResult) throws ShapeTreeException {
        if (!validationResult.isValid()) { return createInvalidResponse(nativeRequest, validationResult); }
        ManageableInstance updatedInstance = reloadInstance(requestInstance);
        ManagerResource original = requestInstance.getManagerResource();
        ManagerResource updated = updatedInstance.getManagerResource();
        int responseCode;
        if (!original.isExists() && updated.isExists()) { responseCode = 201; } else { responseCode = 204; }
        return createResponse(nativeRequest, responseCode);
    }

}
