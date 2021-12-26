// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.client.http
import { ResourceAttributes } from '@shapetrees/ResourceAttributes';

export class HttpRequest {

   public method: string;

   public resourceURL: URL;

   public headers: ResourceAttributes;

   public body: string;

   public contentType: string;

  public constructor(method: string, resourceURL: URL, headers: ResourceAttributes, body: string, contentType: string) {
    this.method = method;
    this.resourceURL = resourceURL;
    this.headers = headers;
    this.body = body;
    this.contentType = contentType;
  }
}
