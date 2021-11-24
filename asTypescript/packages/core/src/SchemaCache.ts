// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.core
import { ShapeTreeException } from './exceptions/ShapeTreeException';
import * as ShexSchema from 'fr/inria/lille/shexjava/schema';

/**
 * Optional, static cache for pre-compiled ShEx schemas
 */
export class SchemaCache {

  private constructor() {
  }

   public static readonly CACHE_IS_NOT_INITIALIZED: string = "Cache is not initialized";

   private static cache: Map<URL, ShexSchema> = null;

  public static initializeCache(): void {
    cache = new Map<>();
  }

  public static initializeCache(existingCache: Map<URL, ShexSchema>): void {
    cache = existingCache;
  }

  public static isInitialized(): boolean {
    let initialized: boolean = cache != null;
    log.debug("Cache initialized set to {}", initialized);
    return initialized;
  }

  public static containsSchema(schemaUrl: URL): boolean /* throws ShapeTreeException */ {
    log.debug("Determining if cache contains schema {}", schemaUrl);
    if (cache === null) {
      throw new ShapeTreeException(500, CACHE_IS_NOT_INITIALIZED);
    }
    return cache.containsKey(schemaUrl);
  }

  public static getSchema(schemaUrl: URL): ShexSchema /* throws ShapeTreeException */ {
    log.debug("Getting schema {}", schemaUrl);
    if (cache === null) {
      throw new ShapeTreeException(500, CACHE_IS_NOT_INITIALIZED);
    }
    return cache.get(schemaUrl);
  }

  public static putSchema(schemaUrl: URL, schema: ShexSchema): void /* throws ShapeTreeException */ {
    log.debug("Caching schema {}", schemaUrl.toString());
    if (cache === null) {
      throw new ShapeTreeException(500, CACHE_IS_NOT_INITIALIZED);
    }
    cache.put(schemaUrl, schema);
  }

  public static clearCache(): void /* throws ShapeTreeException */ {
    if (cache === null) {
      throw new ShapeTreeException(500, CACHE_IS_NOT_INITIALIZED);
    }
    cache.clear();
  }

  public static unInitializeCache(): void /* throws ShapeTreeException */ {
    if (cache != null) {
      cache.clear();
    }
    cache = null;
  }
}
