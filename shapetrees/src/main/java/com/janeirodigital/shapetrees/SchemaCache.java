package com.janeirodigital.shapetrees;

import fr.inria.lille.shexjava.schema.ShexSchema;
import lombok.extern.slf4j.Slf4j;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;

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
        log.debug("Cache initialized {}", initialized);
        return initialized;
    }

    public static boolean containsSchema(URI schemaURI) {
        log.debug("Determining if cache contains schema {}", schemaURI);
        return schemaCache.containsKey(schemaURI);
    }

    public static ShexSchema getSchema(URI schemaURI) {
        log.debug("Getting schema {}", schemaURI);
        return schemaCache.get(schemaURI);
    }

    public static void putSchema(URI schemaURI, ShexSchema schema) {
        log.debug("Caching schema {}", schemaURI.toString());
        schemaCache.put(schemaURI, schema);
    }

    public static void clearCache() {
        schemaCache.clear();
    }
}
