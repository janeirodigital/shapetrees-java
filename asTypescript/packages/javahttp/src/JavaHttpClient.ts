// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.javahttp
import { HttpClient } from '@shapetrees/HttpClient';
import { HttpRequest } from '@shapetrees/HttpRequest';
import { DocumentResponse } from '@shapetrees/DocumentResponse';
import { ResourceAttributes } from '@shapetrees/ResourceAttributes';
import { ShapeTreeException } from '@shapetrees/exceptions/ShapeTreeException';
import * as TrustManager from 'javax/net/ssl';
import * as X509TrustManager from 'javax/net/ssl';
import * as SSLContext from 'javax/net/ssl';
import * as HttpsURLConnection from 'javax/net/ssl';
import * as HostnameVerifier from 'javax/net/ssl';
import * as SSLSession from 'javax/net/ssl';
import * as URISyntaxException from 'java/net';
import * as KeyManagementException from 'java/security';
import * as NoSuchAlgorithmException from 'java/security';
import * as CertificateException from 'java/security/cert';
import * as X509Certificate from 'java/security/cert';
import * as Objects from 'java/util';
import { JavaHttpValidatingShapeTreeInterceptor } from './JavaHttpValidatingShapeTreeInterceptor';

/**
 * java.net.http implementation of HttpClient
 */
export class JavaHttpClient implements HttpClient {

   private readonly httpClient: java.net.http.HttpClient;

   private validatingWrapper: JavaHttpValidatingShapeTreeInterceptor;

  /**
   * Execute an HTTP request to create a DocumentResponse object
   * Implements `HttpClient` interface
   * @param request an HTTP request with appropriate headers for ShapeTree interactions
   * @return new DocumentResponse with response headers and contents
   * @throws ShapeTreeException
   */
  override public fetchShapeTreeResponse(request: HttpRequest): DocumentResponse /* throws ShapeTreeException */ {
    let response: java.net.http.HttpResponse = fetch(request);
    let body: string = null;
    try {
      body = Objects.requireNonNull(response.body()).toString();
    } catch (ex) {
 if (ex instanceof NullPointerException) {
       log.error("Exception retrieving body string");
     }
    return new DocumentResponse(new ResourceAttributes(response.headers().map()), body, response.statusCode());
  }

  /**
   * Construct an JavaHttpClient with switches to enable or disable SSL and ShapeTree validation
   * @param useSslValidation
   * @param useShapeTreeValidation
   * @throws NoSuchAlgorithmException potentially thrown while disabling SSL validation
   * @throws KeyManagementException potentially thrown while disabling SSL validation
   */
  protected constructor(useSslValidation: boolean, useShapeTreeValidation: boolean) /* throws NoSuchAlgorithmException, KeyManagementException */ {
    let clientBuilder: java.net.http.HttpClient.Builder = java.net.http.HttpClient.newBuilder();
    this.validatingWrapper = null;
    if (Boolean.TRUE === useShapeTreeValidation) {
      this.validatingWrapper = new JavaHttpValidatingShapeTreeInterceptor();
    }
    if (Boolean.FALSE === useSslValidation) {
      let trustAllCerts: TrustManager[] = new TrustManager[] { new X509TrustManager() {

        public getAcceptedIssuers(): java.security.cert.X509Certificate[] {
          return null;
        }

        override public checkClientTrusted(arg0: X509Certificate[], arg1: string): void /* throws CertificateException */ {
        }

        override public checkServerTrusted(arg0: X509Certificate[], arg1: string): void /* throws CertificateException */ {
        }
      } };
      let sc: SSLContext = null;
      try {
        sc = SSLContext.getInstance("TLSv1.2");
      } catch (ex) {
 if (ex instanceof NoSuchAlgorithmException) {
         e.printStackTrace();
       }
      try {
        sc.init(null, trustAllCerts, new java.security.SecureRandom());
      } catch (ex) {
 if (ex instanceof KeyManagementException) {
         e.printStackTrace();
       }
      HttpsURLConnection.setDefaultSSLSocketFactory(sc.getSocketFactory());
      // Create all-trusting host name verifier
      let validHosts: HostnameVerifier = new HostnameVerifier() {

        override public verify(arg0: string, arg1: SSLSession): boolean {
          return true;
        }
      };
      // All hosts will be valid
      HttpsURLConnection.setDefaultHostnameVerifier(validHosts);
    }
    this.httpClient = clientBuilder.build();
  }

  /**
   * Internal function to execute HTTP request and return java.net.http response
   * @param request
   * @return
   * @throws ShapeTreeException
   */
  private fetch(request: HttpRequest): java.net.http.HttpResponse /* throws ShapeTreeException */ {
    if (request.body === null)
      request.body = "";
    try {
      let requestBuilder: java.net.http.HttpRequest.Builder = java.net.http.HttpRequest.newBuilder();
      requestBuilder.uri(request.resourceURL.toURI());
      if (request.headers != null) {
        let headerList: string[] = request.headers.toList("connection", "content-length", "date", "expect", "from", "host", "upgrade", "via", "warning");
        if (headerList.length > 0) {
          requestBuilder.headers(headerList);
        }
      }
      switch(request.method) {
        case HttpClient.GET:
        case HttpClient.DELETE:
          requestBuilder.method(request.method, java.net.http.HttpRequest.BodyPublishers.noBody());
          break;
        case HttpClient.PUT:
        case HttpClient.POST:
        case HttpClient.PATCH:
          requestBuilder.method(request.method, java.net.http.HttpRequest.BodyPublishers.ofString(request.body));
          requestBuilder.header("Content-Type", request.contentType);
          break;
        default:
          throw new ShapeTreeException(500, "Unsupported HTTP method for resource creation");
      }
      let nativeRequest: java.net.http.HttpRequest = requestBuilder.build();
      if (this.validatingWrapper === null) {
        return JavaHttpClient.check(this.httpClient.send(nativeRequest, java.net.http.HttpResponse.BodyHandlers.ofString()));
      } else {
        return this.validatingWrapper.validatingWrap(nativeRequest, this.httpClient, request.body, request.contentType);
      }
    } catch (ex) {
 if (ex instanceof IOException || ex instanceof InterruptedException) {
       throw new ShapeTreeException(500, ex.getMessage());
     } else if (ex instanceof URISyntaxException) {
       throw new ShapeTreeException(500, "Malformed URL <" + request.resourceURL + ">: " + ex.getMessage());
     }
  }

  protected static check(resp: java.net.http.HttpResponse): java.net.http.HttpResponse {
    if (resp.statusCode() > 599) {
      throw new Error("invalid HTTP response: " + resp + (resp.body() === null ? "" : "\n" + resp.body()));
    }
    return resp;
  }
}
