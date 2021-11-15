package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.contentloaders.DocumentLoaderManager;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.helpers.GraphHelper;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.apache.jena.rdf.model.Model;

import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import static com.janeirodigital.shapetrees.core.helpers.GraphHelper.removeUrlFragment;
import static com.janeirodigital.shapetrees.core.helpers.GraphHelper.urlToUri;

/**
 * Represents a resource that contains one or more shape tree definitions. Provides
 * a factory to lookup, initialize, and cache them.
 */
@AllArgsConstructor @Slf4j @Getter
public class ShapeTreeResource {

    final URL url;
    final String body;
    final String contentType;
    final Model model;

    @Getter
    private static final Map<URL, ShapeTreeResource> localResourceCache = new HashMap<>();

    /**
     * Looks up and caches the shape tree resource at <code>resourceUrl</code>. Will used cached
     * verion if it exists. Throws exceptions if the resource doesn't exist, or isn't a valid
     * RDF document.
     * @param resourceUrl URL of shape tree resource
     * @return Shape tree resource at provided <code>resourceUrl</code>
     * @throws ShapeTreeException
     */
    public static ShapeTreeResource getShapeTreeResource(URL resourceUrl) throws ShapeTreeException {

        resourceUrl = removeUrlFragment(resourceUrl);

        if (localResourceCache.containsKey(resourceUrl)) {
            log.debug("[{}] previously cached -- returning", resourceUrl);
            return localResourceCache.get(resourceUrl);
        }

        DocumentResponse externalDocument = DocumentLoaderManager.getLoader().loadExternalDocument(resourceUrl);
        if (!externalDocument.isExists()) {
            throw new ShapeTreeException(500, "Cannot load shape shape tree resource at " + resourceUrl);
        }

        Model model = GraphHelper.readStringIntoModel(urlToUri(resourceUrl), externalDocument.getBody(), externalDocument.getContentType().orElse("text/turtle"));

        ShapeTreeResource resource = new ShapeTreeResource(resourceUrl, externalDocument.getBody(), externalDocument.getContentType().orElse("text/turtle"), model);

        localResourceCache.put(resourceUrl, resource);

        return resource;

    }

    /**
     * Clears the local shape tree resource cache
     */
    public static void
    clearCache() {
        localResourceCache.clear();
    }

}
