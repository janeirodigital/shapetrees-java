package com.janeirodigital.shapetrees.core.methodhandlers;

import com.janeirodigital.shapetrees.core.*;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;
import lombok.extern.slf4j.Slf4j;

import java.net.MalformedURLException;
import java.util.Optional;

@Slf4j
public class ValidatingPatchMethodHandler extends AbstractValidatingMethodHandler implements ValidatingMethodHandler {

    public ValidatingPatchMethodHandler(ResourceAccessor resourceAccessor) {
        super(resourceAccessor);
    }

    @Override
    public Optional<DocumentResponse> validateRequest(ShapeTreeRequest shapeTreeRequest) throws ShapeTreeException, MalformedURLException {
            if (shapeTreeRequest.getContentType() == null || !shapeTreeRequest.getContentType().equalsIgnoreCase("application/sparql-update")) {
                log.error("Received a patch without a content type of application/sparql-update");
                throw new ShapeTreeException(415, "PATCH verb expects a content type of application/sparql-update");
            }

            ShapeTreeContext shapeTreeContext = buildContextFromRequest(shapeTreeRequest);

            ShapeTreeResource rc = new ShapeTreeResource(shapeTreeRequest.getUrl(), this.resourceAccessor, shapeTreeContext);

            if (rc.wasCreatedFromMetadata()) {
                // Target resource is for shape tree metadata, manage shape trees to plant and/or unplant
                return Optional.of(manageShapeTree(rc, shapeTreeRequest));
            } else {
                ShapeTreeResource.Primary targetResource = rc.getUserOwnedResourceFork();
                shapeTreeRequest.setResourceType(determineResourceType(shapeTreeRequest, rc));
                if (targetResource.isExists()) {
                    // The target resource already exists
                    if (!targetResource.getMetadataResourceUrl().isEmpty()) {
                        // If it is managed by a shape tree the update must be validated
                        return updateShapeTreeInstance(rc, shapeTreeContext, shapeTreeRequest);
                    }
                } else {
                    // The target resource doesn't exist
                    ShapeTreeResource parentResource = new ShapeTreeResource(getParentContainerUrl(targetResource), this.resourceAccessor, shapeTreeContext);
                    if (!targetResource.getMetadataResourceUrl().isEmpty()) {
                        // If the parent container is managed by a shape tree, the resource to create must be validated
                        return createShapeTreeInstance(rc, parentResource, shapeTreeRequest, getRequestResourceName(targetResource));
                    }
                }
            }

            // Reaching this point means validation was not necessary
            // Pass the request along with no validation
            return Optional.empty();
    }
}
