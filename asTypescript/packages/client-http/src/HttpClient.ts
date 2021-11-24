// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.client.http
import { DocumentResponse } from '@shapetrees/DocumentResponse';
import { ShapeTreeException } from '@shapetrees/exceptions/ShapeTreeException';
import { HttpRequest } from './HttpRequest';

/**
 * abstract base class for ShapeTree library network drivers
 */
export interface HttpClient {

   GET: string = "GET";

   PUT: string = "PUT";

   POST: string = "POST";

   PATCH: string = "PATCH";

   DELETE: string = "DELETE";

  /**
   * Execute an HTTP request to create a DocumentResponse object
   * Implements `HttpClient` interface
   * @param request an HTTP request with appropriate headers for ShapeTree interactions
   * @return new DocumentResponse with response headers and contents
   * @throws ShapeTreeException
   */
  fetchShapeTreeResponse(request: HttpRequest): DocumentResponse /* throws ShapeTreeException */;
}
