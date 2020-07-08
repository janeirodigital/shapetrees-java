package com.janeirodigital.shapetrees.client;

import com.janeirodigital.shapetrees.*;
import com.janeirodigital.shapetrees.model.ShapeTree;
import com.janeirodigital.shapetrees.model.ShapeTreeLocator;
import com.janeirodigital.shapetrees.model.ShapeTreePlantResult;
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

        URI parentURI = getParentContainerURI();
        boolean isContainer = this.requestRemoteResource.isContainer();
        URI normalizedBaseURI = normalizeBaseURI(this.requestRemoteResource.getURI(), null, isContainer);
        Graph incomingRequestBodyGraph = getIncomingBodyGraph(normalizedBaseURI);
        String requestedName = getRequestResourceName();
        RemoteResource parentContainer = new RemoteResource(parentURI, this.authorizationHeaderValue);
        List<ShapeTreeLocator> shapeTreeLocatorMetadatas = validateAgainstParentContainer(incomingRequestBodyGraph, normalizedBaseURI, parentContainer, requestedName, isContainer);
        if (shapeTreeLocatorMetadatas == null) {
            // If validation returns no locators, that means the parent container is not managed and the request should be passed through
            return this.chain.proceed(this.chain.request());
        }
        if (!isContainer) {
            // We're creating/updating a resource and it has already passed validation, pass through
            return this.chain.proceed(this.chain.request());
        }

        List<ShapeTreePlantResult> results = new ArrayList<>();
        for (ShapeTreeLocator locator : shapeTreeLocatorMetadatas) {

            ShapeTree rootShapeTree = ShapeTreeFactory.getShapeTree(new URI(locator.getRootShapeTree()));
            ShapeTree shapeTree = ShapeTreeFactory.getShapeTree(new URI(locator.getShapeTree()));

            // Determine the depth based on container and the relative depth
            String containerPath = locator.getShapeTreeInstancePath();
            String instanceRoot = locator.getShapeTreeRoot();
            String pathFromRoot = requestRemoteResource.getURI().toString().replace(instanceRoot, "");
            // In this case the URI is going to end in a slash for the contain that is being requested to create
            // because of this extra slash, we just count the slashes instead of adding one as seen in the ValidatingPostHandler
            int relativeDepth = StringUtils.countMatches(pathFromRoot, "/");

            if (requestedName.endsWith("/")) {
                requestedName = requestedName.replace("/","");
            }
            ShapeTreePlantResult result = plantShapeTree(this.authorizationHeaderValue, parentContainer, this.incomingRequestBody, rootShapeTree, shapeTree, requestedName, containerPath + requestedName + "/", relativeDepth);
            results.add(result);
        }

        return createPlantResponse(results, this.request, this.incomingRequestLinkHeaders);
    }
}
