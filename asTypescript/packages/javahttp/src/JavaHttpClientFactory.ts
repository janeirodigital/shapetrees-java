// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.javahttp
import { HttpClientFactory } from '@shapetrees/HttpClientFactory';
import { HttpRequest } from '@shapetrees/HttpRequest';
import { DocumentResponse } from '@shapetrees/DocumentResponse';
import { ExternalDocumentLoader } from '@shapetrees/contentloaders/ExternalDocumentLoader';
import { ShapeTreeException } from '@shapetrees/exceptions/ShapeTreeException';
import { JavaHttpClient } from './JavaHttpClient';

/**
 * The ShapeTree library uses a generic interface (`HttpClient`) to execute HTTP queries on the POD and for external documents.
 * The JavaHttpClient uses the java.net.http library to implement `HttpClient`.
 * This factory generates variations of java.net.http those clients depending on the need for SSL validation and ShapeTree validation.
 */
export class JavaHttpClientFactory implements HttpClientFactory, ExternalDocumentLoader {

   useSslValidation: boolean;

  /**
   * Construct a factory for JavaHttpClients
   *
   * @param useSslValidation
   */
  constructor(useSslValidation: boolean) {
    this.useSslValidation = useSslValidation;
  }

  /**
   * Create a new java.net.http HttpClient.
   * This fulfils the HttpClientFactory interface, so this factory can be use in
   *   HttpClientFactoryManager.setFactory(new JavaHttpClientFactory(...));
   *
   * @param useShapeTreeValidation
   * @return a new or existing java.net.http HttpClient
   * @throws ShapeTreeException if the JavaHttpClient constructor threw one
   */
  public get(useShapeTreeValidation: boolean): JavaHttpClient /* throws ShapeTreeException */ {
    try {
      return new JavaHttpClient(this.useSslValidation, useShapeTreeValidation);
    } catch (ex) {
 if (ex instanceof Exception) {
       throw new ShapeTreeException(500, ex.getMessage());
     }
  }

  /**
   * Load a non-POD document
   * This fulfils the ExternalDocumentLoader interface, so this factory can be use in
   *   DocumentLoaderManager.setLoader(new JavaHttpClientFactory(...));
   *
   * @param resourceUrl URL of resource to be retrieved
   * @return a DocumentResponse with the results of a successful GET
   * @throws ShapeTreeException if the GET was not successful
   */
  override public loadExternalDocument(resourceUrl: URL): DocumentResponse /* throws ShapeTreeException */ {
    let response: DocumentResponse = this.get(false).fetchShapeTreeResponse(new HttpRequest("GET", resourceUrl, null, null, null));
    if (response.getStatusCode() != 200) {
      throw new ShapeTreeException(500, "Failed to load contents of document: " + resourceUrl);
    }
    return response;
  }
}
