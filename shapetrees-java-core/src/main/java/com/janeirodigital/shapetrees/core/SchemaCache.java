package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import fr.inria.lille.shexjava.schema.ShexSchema;
import lombok.extern.slf4j.Slf4j;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;

/**
 * Optional, static cache for pre-compiled ShEx schemas
 */
@Slf4j
public class SchemaCache {
    private static Map<URI, ShexSchema> schemaCache = null;

    public static void initializeCache() {
        schemaCache = new HashMap<>();
    }

    public static void initializeCache(Map<URI, ShexSchema> existingCache) {
        schemaCache = existingCache;
    }

    public static boolean isInitialized() {
        boolean initialized = schemaCache != null;
        log.debug("Cache initialized set to {}", initialized);
        return initialized;
    }

    public static boolean containsSchema(URI schemaURI) throws ShapeTreeException {
        log.debug("Determining if cache contains schema {}", schemaURI);
        if (schemaCache == null) {
            throw new ShapeTreeException(500, "Cache is not initialized");
        }
        return schemaCache.containsKey(schemaURI);
    }

    public static ShexSchema getSchema(URI schemaURI) throws ShapeTreeException {
        log.debug("Getting schema {}", schemaURI);
        if (schemaCache == null) {
            throw new ShapeTreeException(500, "Cache is not initialized");
        }
        return schemaCache.get(schemaURI);
    }

    public static void putSchema(URI schemaURI, ShexSchema schema) throws ShapeTreeException {
        log.debug("Caching schema {}", schemaURI.toString());
        if (schemaCache == null) {
            throw new ShapeTreeException(500, "Cache is not initialized");
        }
        schemaCache.put(schemaURI, schema);
    }

    public static void clearCache() throws ShapeTreeException {
        if (schemaCache == null) {
            throw new ShapeTreeException(500, "Cache is not initialized");
        }
        schemaCache.clear();
    }
}
