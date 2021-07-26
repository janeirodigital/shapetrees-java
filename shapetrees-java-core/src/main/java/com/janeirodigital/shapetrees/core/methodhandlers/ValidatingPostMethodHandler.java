package com.janeirodigital.shapetrees.core.methodhandlers;

import com.janeirodigital.shapetrees.core.ResourceAccessor;
import com.janeirodigital.shapetrees.core.ShapeTreeRequest;
import com.janeirodigital.shapetrees.core.ShapeTreeResource;
import com.janeirodigital.shapetrees.core.ShapeTreeValidationResponse;
import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;
import lombok.extern.slf4j.Slf4j;

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
            ShapeTreeResource targetContainer = getRequestResource(shapeTreeContext, shapeTreeRequest);

            // Get resource name from the slug or default to UUID
            String proposedName = getIncomingHeaderValueWithDefault(shapeTreeRequest, HttpHeaders.SLUG.getValue(), UUID.randomUUID().toString());

            // If the parent container is managed by a shape tree, the proposed resource being posted must be
            // validated against the parent tree.
            if (targetContainer.isManaged()) {
                return createShapeTreeInstance(shapeTreeContext, shapeTreeRequest, proposedName);
            }

            // Reaching this point means validation was not necessary
            // Pass the request along with no validation
            return ShapeTreeValidationResponse.passThroughResponse();

        } catch (ShapeTreeException ste) {
            return new ShapeTreeValidationResponse(ste);
//        } catch (URISyntaxException e) {
//            return new ShapeTreeValidationResponse(new ShapeTreeException(400, "Value of 'ShapeTree' link header is not a value URI"));
        } catch (Exception ex) {
            return new ShapeTreeValidationResponse(new ShapeTreeException(500, ex.getMessage()));
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
