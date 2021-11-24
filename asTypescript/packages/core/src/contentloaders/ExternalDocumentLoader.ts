// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.core.contentloaders
import { DocumentResponse } from '../DocumentResponse';
import { ShapeTreeException } from '../exceptions/ShapeTreeException';

/**
 * Interface defining how a remote document can be loaded and its contents extracted.
 * Implementations can add capabilities like caching, retrieving resources from alternate
 * locations, etc.
 */
export interface ExternalDocumentLoader {

  /**
   * Describes the retrieval of a remote document
   * @param resourceUrl URL of resource to be retrieved
   * @return DocumentResponse representation which contains body and content type
   * @throws ShapeTreeException ShapeTreeException
   */
  loadExternalDocument(resourceUrl: URL): DocumentResponse /* throws ShapeTreeException */;
}
