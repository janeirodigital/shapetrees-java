// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.javahttp
import { HttpResourceAccessor } from '@shapetrees/HttpResourceAccessor';
import { DocumentResponse } from '@shapetrees/DocumentResponse';
import { ResourceAttributes } from '@shapetrees/ResourceAttributes';
import { ResourceAccessor } from '@shapetrees/ResourceAccessor';
import { ShapeTreeRequest } from '@shapetrees/ShapeTreeRequest';
import { HttpHeaders } from '@shapetrees/enums/HttpHeaders';
import { ShapeTreeResourceType } from '@shapetrees/enums/ShapeTreeResourceType';
import { ShapeTreeException } from '@shapetrees/exceptions/ShapeTreeException';
import { ValidatingMethodHandler } from '@shapetrees/methodhandlers/ValidatingMethodHandler';
import { ValidatingDeleteMethodHandler } from '@shapetrees/methodhandlers/ValidatingDeleteMethodHandler';
import { ValidatingPutMethodHandler } from '@shapetrees/methodhandlers/ValidatingPutMethodHandler';
import { ValidatingPatchMethodHandler } from '@shapetrees/methodhandlers/ValidatingPatchMethodHandler';
import { ValidatingPostMethodHandler } from '@shapetrees/methodhandlers/ValidatingPostMethodHandler';
import * as NotNull from 'org/jetbrains/annotations';
import * as SSLSession from 'javax/net/ssl';
import * as MalformedURLException from 'java/net';
import * as URI from 'java/net';
import * as HttpResponse from 'java/net/http';
import * as Collections from 'java/util';
import * as TreeMap from 'java/util';

/**
 * Wrapper used for client-side validation
 */
export class JavaHttpValidatingShapeTreeInterceptor {

   private static readonly POST: string = "POST";

   private static readonly PUT: string = "PUT";

   private static readonly PATCH: string = "PATCH";

   private static readonly DELETE: string = "DELETE";

  // @NotNull
  public validatingWrap(clientRequest: java.net.http.HttpRequest, httpClient: java.net.http.HttpClient, body: string, contentType: string): java.net.http.HttpResponse /* throws IOException, InterruptedException */ {
    let shapeTreeRequest: ShapeTreeRequest = new JavaHttpShapeTreeRequest(clientRequest, body, contentType);
    let resourceAccessor: ResourceAccessor = new HttpResourceAccessor();
    // Get the handler
    let handler: ValidatingMethodHandler = getHandler(shapeTreeRequest.getMethod(), resourceAccessor);
    if (handler != null) {
      try {
        let shapeTreeResponse: DocumentResponse | null = handler.validateRequest(shapeTreeRequest);
        if (!shapeTreeResponse.isPresent()) {
          return JavaHttpClient.check(httpClient.send(clientRequest, java.net.http.HttpResponse.BodyHandlers.ofString()));
        } else {
          return createResponse(clientRequest, shapeTreeResponse.get());
        }
      } catch (ex) {
 if (ex instanceof ShapeTreeException) {
         log.error("Error processing shape tree request: ", ex);
         return createErrorResponse(ex, clientRequest);
       } else if (ex instanceof Exception) {
         log.error("Error processing shape tree request: ", ex);
         return createErrorResponse(new ShapeTreeException(500, ex.getMessage()), clientRequest);
       }
    } else {
      log.warn("No handler for method [{}] - passing through request", shapeTreeRequest.getMethod());
      return JavaHttpClient.check(httpClient.send(clientRequest, java.net.http.HttpResponse.BodyHandlers.ofString()));
    }
  }

  private getHandler(requestMethod: string, resourceAccessor: ResourceAccessor): ValidatingMethodHandler {
    switch(requestMethod) {
      case POST:
        return new ValidatingPostMethodHandler(resourceAccessor);
      case PUT:
        return new ValidatingPutMethodHandler(resourceAccessor);
      case PATCH:
        return new ValidatingPatchMethodHandler(resourceAccessor);
      case DELETE:
        return new ValidatingDeleteMethodHandler(resourceAccessor);
      default:
        return null;
    }
  }

  private createErrorResponse(exception: ShapeTreeException, nativeRequest: java.net.http.HttpRequest): java.net.http.HttpResponse {
    return new MyHttpResponse(exception.getStatusCode(), nativeRequest, java.net.http.HttpHeaders.of(Collections.emptyMap(), (a, v) -> true), exception.getMessage());
  }

  // @SneakyThrows
  private createResponse(nativeRequest: java.net.http.HttpRequest, response: DocumentResponse): java.net.http.HttpResponse {
    let headers: java.net.http.HttpHeaders = java.net.http.HttpHeaders.of(response.getResourceAttributes().toMultimap(), (a, v) -> true);
    return new MyHttpResponse(response.getStatusCode(), nativeRequest, headers, response.getBody());
  }

  private class JavaHttpShapeTreeRequest implements ShapeTreeRequest {

     private readonly request: java.net.http.HttpRequest;

     private resourceType: ShapeTreeResourceType;

     private readonly body: string;

     private readonly contentType: string;

     private readonly headers: ResourceAttributes;

    public constructor(request: java.net.http.HttpRequest, body: string, contentType: string) {
      this.request = request;
      this.body = body;
      this.contentType = contentType;
      let tm: TreeMap<string, Array<string>> = new TreeMap<>(String.CASE_INSENSITIVE_ORDER);
      let headerMap: Map<string, Array<string>> = this.request.headers().map();
      for (const entry of headerMap.entrySet()) {
        tm.put(entry.getKey(), entry.getValue());
      }
      this.headers = new ResourceAttributes(tm);
    }

    override public getMethod(): string {
      return this.request.method();
    }

    override public getUrl(): URL {
      try {
        return this.request.uri().toURL();
      } catch (ex) {
 if (ex instanceof MalformedURLException) {
         throw new IllegalStateException("request has a malformed URL <" + request.uri() + ">: " + ex.getMessage());
       }
    }

    override public getHeaders(): ResourceAttributes {
      return this.headers;
    }

    override public getLinkHeaders(): ResourceAttributes {
      return ResourceAttributes.parseLinkHeaders(this.getHeaderValues(HttpHeaders.LINK.getValue()));
    }

    override public getHeaderValues(header: string): Array<string> {
      return this.request.headers().allValues(header);
    }

    override public getHeaderValue(header: string): string {
      return this.request.headers().firstValue(header).orElse(null);
    }

    override public getContentType(): string {
      return this.getHeaders().firstValue(HttpHeaders.CONTENT_TYPE.getValue()).orElse(null);
    }

    override public getResourceType(): ShapeTreeResourceType {
      return this.resourceType;
    }

    override public setResourceType(resourceType: ShapeTreeResourceType): void {
      this.resourceType = resourceType;
    }

    override public getBody(): string {
      return this.body;
    }
  }

  private class MyHttpResponse implements java.net.http.HttpResponse {

     private statusCode: number;

     private request: java.net.http.HttpRequest;

     private headers: java.net.http.HttpHeaders;

     private body: string;

    override public statusCode(): number {
      return this.statusCode;
    }

    override public request(): java.net.http.HttpRequest {
      return this.request;
    }

    override public previousResponse(): HttpResponse<string> | null {
      return Optional.empty();
    }

    override public headers(): java.net.http.HttpHeaders {
      return this.headers;
    }

    override public body(): string {
      return this.body;
    }

    override public sslSession(): SSLSession | null {
      return Optional.empty();
    }

    override public uri(): URI {
      return null;
    }

    override public version(): java.net.http.HttpClient.Version {
      return null;
    }

    public constructor(statusCode: number, request: java.net.http.HttpRequest, headers: java.net.http.HttpHeaders, body: string) {
      this.statusCode = statusCode;
      this.request = request;
      this.headers = headers;
      this.body = body;
    }
  }
}
