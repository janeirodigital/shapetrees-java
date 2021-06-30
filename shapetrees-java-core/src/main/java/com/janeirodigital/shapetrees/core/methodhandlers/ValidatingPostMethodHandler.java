package com.janeirodigital.shapetrees.core.methodhandlers;

import com.janeirodigital.shapetrees.core.ResourceAccessor;
import com.janeirodigital.shapetrees.core.ShapeTreeRequest;
import com.janeirodigital.shapetrees.core.ShapeTreeResource;
import com.janeirodigital.shapetrees.core.ShapeTreeValidationResponse;
import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;
import lombok.extern.slf4j.Slf4j;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;
import java.util.UUID;

@Slf4j
public class ValidatingPostMethodHandler extends AbstractValidatingMethodHandler implements ValidatingMethodHandler {

    public ValidatingPostMethodHandler(ResourceAccessor resourceAccessor) {
        super(resourceAccessor);
    }

    @Override
    public ShapeTreeValidationResponse validateRequest(ShapeTreeRequest<?> shapeTreeRequest) {
        try {
            ShapeTreeContext shapeTreeContext = buildContextFromRequest(shapeTreeRequest);

            // Look up the target container for the POST. Error if it doesn't exist, or is a metadata resource
            ShapeTreeResource existingResource = getRequestResource(shapeTreeContext, shapeTreeRequest);
            ensureRequestResourceExists(existingResource,"Target container for POST not found");
            ensureRequestResourceIsNotMetadata(existingResource,"POST not allowed to shape tree metadata resource");

            // Check the resource type, and error if it isn't a container
            shapeTreeRequest.setResourceType(determineResourceType(shapeTreeRequest, existingResource));
            ensureRequestResourceIsContainer(existingResource,"POST not allowed to a non-container resource");

            // Get resource name from the slug or default to UUID
            String requestedName = getIncomingHeaderValueWithDefault(shapeTreeRequest, HttpHeaders.SLUG.getValue(), UUID.randomUUID().toString());

            URI normalizedBaseURI = normalizeBaseURI(existingResource.getUri(), requestedName, shapeTreeRequest.getResourceType());

            return ShapeTreeValidationResponse.passThroughResponse();

        } catch (ShapeTreeException ste) {
            return new ShapeTreeValidationResponse(ste);
        } catch (URISyntaxException e) {
            return new ShapeTreeValidationResponse(new ShapeTreeException(400, "Value of 'ShapeTree' link header is not a value URI"));
        } catch (Exception ex) {
            return new ShapeTreeValidationResponse(new ShapeTreeException(500, ex.getMessage()));
        }

    }

    private void ensureRequestResourceIsNotMetadata(ShapeTreeResource shapeTreeResource, String message) throws ShapeTreeException {
        if (shapeTreeResource.isMetadata()) {
            throw new ShapeTreeException(400, message);
        }
    }

    private void ensureRequestResourceExists(ShapeTreeResource shapeTreeResource, String message) throws ShapeTreeException {
        if (!shapeTreeResource.isExists()) {
            throw new ShapeTreeException(404, message);
        }
    }

    private void ensureRequestResourceIsContainer(ShapeTreeResource shapeTreeResource, String message) throws ShapeTreeException {
        if (!shapeTreeResource.isContainer()) {
            throw new ShapeTreeException(400, message);
        }
    }

    private String getIncomingHeaderValueWithDefault(ShapeTreeRequest<?> shapeTreeRequest, String headerName, String defaultValue) {
        if (shapeTreeRequest.getHeaders().containsKey(headerName)) {
            return shapeTreeRequest.getHeaders().get(headerName).stream().findFirst().orElse(defaultValue);
        } else {
            return defaultValue;
        }
    }

    protected List<String> getIncomingLinkHeaderByRelationValue(ShapeTreeRequest<?> shapeTreeRequest, String relation) {
        if (shapeTreeRequest.getLinkHeaders().containsKey(relation)) {
            return shapeTreeRequest.getLinkHeaders().get(relation);
        }
        return null;
    }
}
