package com.janeirodigital.shapetrees.core.resources;

import com.janeirodigital.shapetrees.core.enums.HttpHeader;
import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.helpers.GraphHelper;
import lombok.Getter;
import org.apache.jena.graph.Graph;

import java.net.URI;
import java.net.URL;

import static com.janeirodigital.shapetrees.core.helpers.GraphHelper.urlToUri;

/**
 * InstanceResource is a base class which may represent either a ManageableResource
 * (a resource that can be managed by a shape tree), or a ManagerResource (a resource
 * which assigns shape trees to a ManageableResource). This class is only meant to be
 * extended by ManageableResource and ManagerResource, or used to indicate either
 * of the two.
 */
@Getter
public class InstanceResource {

    private final URL url;
    private final ShapeTreeResourceType resourceType;
    private final ResourceAttributes attributes;
    private final String body;
    private final String name;
    private final boolean exists;

    /**
     * Construct an InstanceResource by providing essential attributes. This constructor is meant
     * to be called by sub-class constructors.
     * @param url URL of the instance resource
     * @param resourceType Identified shape tree resource type
     * @param attributes Associated resource attributes
     * @param body Body of the resource
     * @param name Name of the resource
     * @param exists Whether the resource exists
     */
    InstanceResource(URL url, ShapeTreeResourceType resourceType, ResourceAttributes attributes, String body, String name, boolean exists) {
        this.url = url;
        this.resourceType = resourceType;
        this.attributes = attributes;
        this.body = body;
        this.name = name;
        this.exists = exists;
    }

    /**
     * Get an RDF graph of the body of the InstanceResource. If <code>baseUrl</code> is not
     * provided, the URL of the InstanceResource will be used. An exception is thrown if
     * the body cannot be processed (e.g. if it isn't a valid RDF resource).
     * @param baseUrl Base URL to use for the graph
     * @return RDF graph of the InstanceResource body
     * @throws ShapeTreeException
     */
    public Graph getGraph(URL baseUrl) throws ShapeTreeException {
        if (!this.isExists()) { return null; }
        if (baseUrl == null) { baseUrl = this.url; }
        final URI baseUri = urlToUri(baseUrl);
        return GraphHelper.readStringIntoGraph(baseUri, this.getBody(), this.getAttributes().firstValue(HttpHeader.CONTENT_TYPE.getValue()).orElse(null));
    }
}
