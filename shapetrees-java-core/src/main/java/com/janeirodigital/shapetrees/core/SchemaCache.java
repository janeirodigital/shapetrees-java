package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import fr.inria.lille.shexjava.schema.ShexSchema;
import lombok.extern.slf4j.Slf4j;

import java.net.URL;
import java.util.HashMap;
import java.util.Map;

/**
 * Optional, static cache for pre-compiled ShEx schemas
 */
@Slf4j
public class SchemaCache {
    private SchemaCache() {
    }

    public static final String CACHE_IS_NOT_INITIALIZED = "Cache is not initialized";
    private static Map<URL, ShexSchema> cache = null;

    public static void initializeCache() {
        cache = new HashMap<>();
    }

    public static void initializeCache(Map<URL, ShexSchema> existingCache) {
        cache = existingCache;
    }

    public static boolean isInitialized() {
        boolean initialized = cache != null;
        log.debug("Cache initialized set to {}", initialized);
        return initialized;
    }

    public static boolean containsSchema(URL schemaUrl) throws ShapeTreeException {
        log.debug("Determining if cache contains schema {}", schemaUrl);
        if (cache == null) {
            throw new ShapeTreeException(500, CACHE_IS_NOT_INITIALIZED);
        }
        return cache.containsKey(schemaUrl);
    }

    public static ShexSchema getSchema(URL schemaUrl) throws ShapeTreeException {
        log.debug("Getting schema {}", schemaUrl);
        if (cache == null) {
            throw new ShapeTreeException(500, CACHE_IS_NOT_INITIALIZED);
        }
        return cache.get(schemaUrl);
    }

    public static void putSchema(URL schemaUrl, ShexSchema schema) throws ShapeTreeException {
        log.debug("Caching schema {}", schemaUrl.toString());
        if (cache == null) {
            throw new ShapeTreeException(500, CACHE_IS_NOT_INITIALIZED);
        }
        cache.put(schemaUrl, schema);
    }

    public static void clearCache() throws ShapeTreeException {
        if (cache == null) {
            throw new ShapeTreeException(500, CACHE_IS_NOT_INITIALIZED);
        }
        cache.clear();
    }
}
