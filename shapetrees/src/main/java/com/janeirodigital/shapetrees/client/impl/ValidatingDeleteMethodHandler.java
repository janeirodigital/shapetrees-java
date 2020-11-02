package com.janeirodigital.shapetrees.client.impl;

import com.janeirodigital.shapetrees.RemoteResource;
import com.janeirodigital.shapetrees.ShapeTreeEcosystem;
import com.janeirodigital.shapetrees.client.AbstractValidatingMethodHandler;
import com.janeirodigital.shapetrees.client.ValidatingMethodHandler;
import com.janeirodigital.shapetrees.enums.HttpHeaders;
import com.janeirodigital.shapetrees.model.ShapeTree;
import com.janeirodigital.shapetrees.model.ShapeTreeLocator;
import okhttp3.Interceptor;
import okhttp3.Response;
import org.apache.jena.graph.Graph;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;

public class ValidatingDeleteMethodHandler extends AbstractValidatingMethodHandler implements ValidatingMethodHandler {

    public ValidatingDeleteMethodHandler(Interceptor.Chain chain, ShapeTreeEcosystem ecosystem) throws IOException {
        super(chain, ecosystem);
    }

    @Override
    public Response process() throws IOException, URISyntaxException {
        // Get the parent container URI
        URI parentURI = getParentContainerURI();
        // Get shape tree hint, if present
        URI targetShapeTreeHint = getIncomingTargetShapeTreeHint();
        // Get requested name (resource being DELETEd)
        String resourceName = getRequestResourceName();
        // Is resource being deleted a container
        boolean isContainer = this.requestRemoteResource.isContainer();
        // Is resource being deleted a non-RDF source?
        boolean isNonRdfSource = determineIsNonRdfSource(this.requestRemoteResource.getFirstHeaderByName(HttpHeaders.CONTENT_TYPE.getValue()));
        // Dereference parent container
        RemoteResource parentContainer = new RemoteResource(parentURI, this.shapeTreeContext.getAuthorizationHeaderValue());
        // Dereference parent container metadata resource
        RemoteResource parentContainerMetadataResource = parentContainer.getMetadataResource(this.shapeTreeContext.getAuthorizationHeaderValue());
        if (!parentContainerMetadataResource.exists()) {
            // If the parent container is doesn't have a metadata resource it is not managed
            return chain.proceed(chain.request());
        }

        // Retrieve graph of parent container metadata resource
        Graph parentContainerMetadataGraph = parentContainerMetadataResource.getGraph(parentURI);
        // Get ShapeTree locators managing parent container
        List<ShapeTreeLocator> locators = ShapeTreeLocator.getShapeTreeLocatorsFromGraph(parentContainerMetadataGraph);
        if (locators == null || locators.size() == 0) {
            // If there are no shapetrees present in the metadata, then it is not managed
            return chain.proceed(chain.request());
        }

        ShapeTree containingShapeTree = getShapeTreeWithContentsFromShapeTreeLocators(locators);
        ShapeTree targetShapeTree = containingShapeTree.findMatchingContainsShapeTree(resourceName, targetShapeTreeHint, isContainer, isNonRdfSource);

        Response deleteResponse = chain.proceed(chain.request());
        if (deleteResponse.isSuccessful()) {
            // Allow the ecosystem to do any bookkeeping as a result of resource being deleted
            ecosystem.unIndexShapeTreeDataInstance(this.shapeTreeContext, targetShapeTree.getURI(), this.requestRemoteResource.getURI());
        }
        return deleteResponse;
    }
}
