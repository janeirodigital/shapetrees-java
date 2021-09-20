package com.janeirodigital.shapetrees.core.methodhandlers;

import com.janeirodigital.shapetrees.core.ResourceAccessor;
import com.janeirodigital.shapetrees.core.ShapeTreeRequest;
import com.janeirodigital.shapetrees.core.ShapeTreeResource;
import com.janeirodigital.shapetrees.core.DocumentResponse;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.Optional;

@Slf4j
public class ValidatingPatchMethodHandler extends AbstractValidatingMethodHandler implements ValidatingMethodHandler {

    public ValidatingPatchMethodHandler(ResourceAccessor resourceAccessor) {
        super(resourceAccessor);
    }

    @Override
    public Optional<DocumentResponse> validateRequest(ShapeTreeRequest shapeTreeRequest) throws ShapeTreeException, URISyntaxException {
            if (shapeTreeRequest.getContentType() == null || !shapeTreeRequest.getContentType().equalsIgnoreCase("application/sparql-update")) {
                log.error("Received a patch without a content type of application/sparql-update");
                throw new ShapeTreeException(415, "PATCH verb expects a content type of application/sparql-update");
            }

            ShapeTreeContext shapeTreeContext = buildContextFromRequest(shapeTreeRequest);

            ShapeTreeResource targetResource = getRequestResource(shapeTreeContext, shapeTreeRequest);

            if (targetResource.isMetadata()) {
                // Target resource is for shape tree metadata, manage shape trees to plant and/or unplant
                return Optional.of(manageShapeTree(shapeTreeContext, shapeTreeRequest, targetResource));
            } else {
                if (targetResource.isExists()) {
                    // The target resource already exists
                    if (targetResource.isManaged()) {
                        // If it is managed by a shape tree the update must be validated
                        return updateShapeTreeInstance(shapeTreeContext, shapeTreeRequest);
                    }
                } else {
                    // The target resource doesn't exist
                    ShapeTreeResource parentResource = this.resourceAccessor.getResource(shapeTreeContext, getParentContainerURI(targetResource));
                    if (parentResource.isManaged()) {
                        // If the parent container is managed by a shape tree, the resource to create must be validated
                        return createShapeTreeInstance(shapeTreeContext, shapeTreeRequest, getRequestResourceName(targetResource));
                    }
                }
            }

            // Reaching this point means validation was not necessary
            // Pass the request along with no validation
            return Optional.empty();
    }
}
