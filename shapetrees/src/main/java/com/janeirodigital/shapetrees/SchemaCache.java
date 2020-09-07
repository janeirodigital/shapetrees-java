package com.janeirodigital.shapetrees;

import fr.inria.lille.shexjava.schema.ShexSchema;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;

public class SchemaCache {
    private static Map<URI, ShexSchema> schemaCache = null;

    public static void initializeCache() {
        schemaCache = new HashMap<>();
    }

    public static void initializeCache(Map<URI, ShexSchema> existingCache) {
        schemaCache = existingCache;
    }

    public static boolean isInitialized() {
        return schemaCache != null;
    }

    public static boolean containsSchema(URI schemaURI) { return schemaCache.containsKey(schemaURI); }

    public static ShexSchema getSchema(URI schemaURI) {
        return schemaCache.get(schemaURI);
    }

    public static void putSchema(URI schemaURI, ShexSchema schema) {
        schemaCache.put(schemaURI, schema);
    }

    public static void clearCache() {
        schemaCache.clear();
    }
}
