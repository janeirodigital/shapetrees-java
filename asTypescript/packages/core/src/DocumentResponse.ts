// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.core
import { HttpHeaders } from './enums/HttpHeaders';
import { ResourceAttributes } from './ResourceAttributes';

export class DocumentResponse {

   private readonly resourceAttributes: ResourceAttributes;

   private readonly body: string;

   private readonly statusCode: number;

  public getContentType(): string | null {
    return this.resourceAttributes.firstValue(HttpHeaders.CONTENT_TYPE.getValue());
  }

  // TODO: lots of choices re non-404, not >= 4xx, not 3xx. not 201 (meaning there's no body)
  public isExists(): boolean {
    return this.statusCode / 100 === 2;
  }

  public constructor(resourceAttributes: ResourceAttributes, body: string, statusCode: number) {
    this.resourceAttributes = resourceAttributes;
    this.body = body;
    this.statusCode = statusCode;
  }

  public getResourceAttributes(): ResourceAttributes {
    return this.resourceAttributes;
  }

  public getBody(): string {
    return this.body;
  }

  public getStatusCode(): number {
    return this.statusCode;
  }
}
