// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.core.contentloaders
import { ShapeTreeException } from '../exceptions/ShapeTreeException';
import { ExternalDocumentLoader } from './ExternalDocumentLoader';

/**
 * Utility class which allows an external document loader to be set by calling code, and then
 * utilized by other code that doesn't require special knowledge of specific document
 * loader implementations.
 */
export abstract class DocumentLoaderManager {

  @Setter(onMethod_ = { @Synchronized })
   private static loader: ExternalDocumentLoader;

  // Private constructor to offset an implicit public constructor on a utility class
  private constructor() {
  }

  /**
   * Return an ExternalDocumentLoader that was previously set and stored statically
   * @return A valid ExternalDocumentLoader that was previously set
   * @throws ShapeTreeException
   */
  // @Synchronized
  public static getLoader(): ExternalDocumentLoader /* throws ShapeTreeException */ {
    if (loader === null) {
      throw new ShapeTreeException(500, "Must provide a valid ExternalDocumentLoader");
    }
    return DocumentLoaderManager.loader;
  }
}
