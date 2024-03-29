package com.janeirodigital.shapetrees.core.enums;

import java.util.Collections;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Enumerated list of applicable HTTP Content-Type values
 */
public enum ContentType {
    TEXT_TURTLE("text/turtle"),
    RDF_XML("application/rdf+xml"),
    N_TRIPLES("application/n-triples"),
    LD_JSON("application/ld+json"),
    TEXT_HTML("text/html"),
    OCTET_STREAM("application/octet-stream"),
    TEXT_PLAIN("text/plain"),
    SPARQL_UPDATE("application/sparql-update");

    public String getValue() {
        return this.value;
    }

    private final String value;
    
    private static final Map<String,ContentType> ENUM_MAP;
    
    ContentType(String value) {
        this.value = value;
    }

    static {
        Map<String,ContentType> map = new ConcurrentHashMap<>();
        for (ContentType instance : ContentType.values()) {
            map.put(instance.getValue().toLowerCase(),instance);
        }
        ENUM_MAP = Collections.unmodifiableMap(map);
    }

    public static ContentType get(String name) {
        return ENUM_MAP.get(name.toLowerCase());
    }
    
}
