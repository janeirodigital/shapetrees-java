// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.core
import { ShapeTreeResourceType } from './enums/ShapeTreeResourceType';
import { ResourceAttributes } from './ResourceAttributes';

export interface ShapeTreeRequest {

  getMethod(): string;

  getUrl(): URL;

  getHeaders(): ResourceAttributes;

  getLinkHeaders(): ResourceAttributes;

  getHeaderValues(header: string): Array<string>;

  getHeaderValue(header: string): string;

  getBody(): string;

  getContentType(): string;

  getResourceType(): ShapeTreeResourceType;

  setResourceType(resourceType: ShapeTreeResourceType): void;
}
