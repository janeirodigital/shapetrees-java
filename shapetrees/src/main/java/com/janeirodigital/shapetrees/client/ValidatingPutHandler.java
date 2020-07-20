package com.janeirodigital.shapetrees.client;

import com.janeirodigital.shapetrees.*;
import com.janeirodigital.shapetrees.helper.PlantHelper;
import com.janeirodigital.shapetrees.model.ShapeTreeLocator;
import com.janeirodigital.shapetrees.model.ShapeTreePlantResult;
import com.janeirodigital.shapetrees.model.ValidationContext;
import okhttp3.Interceptor;
import okhttp3.Response;
import org.apache.commons.lang3.StringUtils;
import org.apache.jena.graph.Graph;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;

public class ValidatingPutHandler extends AbstractValidatingHandler implements ValidatingHandler {

    public ValidatingPutHandler(Interceptor.Chain chain, ShapeTreeEcosystem ecosystem) throws IOException {
        super(chain, ecosystem);
    }

    @Override
    public Response process() throws IOException, URISyntaxException {

        Boolean resourceAlreadyExists = this.requestRemoteResource.exists();
        URI parentURI = getParentContainerURI();
        boolean isContainer = this.requestRemoteResource.isContainer();
        URI normalizedBaseURI = normalizeBaseURI(this.requestRemoteResource.getURI(), null, isContainer);
        Graph incomingRequestBodyGraph = getIncomingBodyGraph(normalizedBaseURI);
        String requestedName = getRequestResourceName();
        RemoteResource parentContainer = new RemoteResource(parentURI, this.authorizationHeaderValue);
        ValidationContext validationContext = validateAgainstParentContainer(incomingRequestBodyGraph, normalizedBaseURI, parentContainer, requestedName, isContainer);
        // Two reasons for passing through the request (and not performing validation):
        // 1. Validation returns no locators, meaning the parent container is not managed
        // 2. We're creating a resource and it has already passed validation
        if (validationContext == null || validationContext.getParentContainerLocators() == null || !isContainer) {
            Response response = this.chain.proceed(this.chain.request());
            // If there is a ShapeTree managing the new resource, register it
            if (validationContext != null && validationContext.getValidatingShapeTree() != null) {
                if (!resourceAlreadyExists) {
                    this.ecosystem.indexShapeTreeDataInstance(this.getShapeTreeContext(), parentURI, validationContext.getValidatingShapeTree().getURI(), this.requestRemoteResource.getURI());
                }
            }
            return response;
        }

        List<ShapeTreePlantResult> results = new ArrayList<>();
        for (ShapeTreeLocator locator : validationContext.getParentContainerLocators()) {

            // Determine the depth based on container and the relative depth
            String pathFromRoot = requestRemoteResource.getURI().toString().replace(locator.getShapeTreeRoot(), "");
            // In this case the URI is going to end in a slash for the contain that is being requested to create
            // because of this extra slash, we just count the slashes instead of adding one as seen in the ValidatingPostHandler
            int relativeDepth = StringUtils.countMatches(pathFromRoot, "/");

            if (requestedName.endsWith("/")) {
                requestedName = requestedName.replace("/","");
            }
            ShapeTreePlantResult result = PlantHelper.plantShapeTree(this.authorizationHeaderValue, this.requestRemoteResource, this.incomingRequestBody, locator, validationContext.getValidatingShapeTree(), requestedName, relativeDepth);
            results.add(result);
        }

        return createPlantResponse(results, this.request, this.incomingRequestLinkHeaders);
    }
}
