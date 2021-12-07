// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.core.contentloaders
import { DocumentResponse } from '../DocumentResponse';
import { ResourceAttributes } from '../ResourceAttributes';
import { ShapeTreeException } from '../exceptions/ShapeTreeException';
import * as URISyntaxException from 'java/net';
import * as HttpClient from 'java/net/http';
import * as HttpRequest from 'java/net/http';
import * as HttpResponse from 'java/net/http';
import { ExternalDocumentLoader } from './ExternalDocumentLoader';

/**
 * Simple HTTP implementation of ExternalDocumentLoader provided as an example
 * as well as for its utility in unit tests.
 */
export class HttpExternalDocumentLoader implements ExternalDocumentLoader {

   private readonly httpClient: HttpClient = HttpClient.newBuilder().followRedirects(HttpClient.Redirect.NEVER).build();

  override public loadExternalDocument(resourceUrl: URL): DocumentResponse /* throws ShapeTreeException */ {
    try {
      let request: HttpRequest = HttpRequest.newBuilder().GET().uri(resourceUrl.toURI()).build();
      let response: HttpResponse<string> = this.httpClient.send(request, HttpResponse.BodyHandlers.ofString());
      if (response.statusCode() != 200) {
        throw new IOException("Failed to load contents of document: " + resourceUrl);
      }
      let attributes: ResourceAttributes = new ResourceAttributes(response.headers().map());
      return new DocumentResponse(attributes, response.body(), response.statusCode());
    } catch (ex) {
 if (ex instanceof IOException) {
       throw new ShapeTreeException(500, "Error retrieving <" + resourceUrl + ">: " + ex.getMessage());
     } else if (ex instanceof InterruptedException) {
       Thread.currentThread().interrupt();
       throw new ShapeTreeException(500, "Error retrieving <" + resourceUrl + ">: " + ex.getMessage());
     } else if (ex instanceof URISyntaxException) {
       throw new ShapeTreeException(500, "Malformed URL <" + resourceUrl + ">: " + ex.getMessage());
     }
  }
}
