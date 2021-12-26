// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.core
export class ShapeTreeContext {

   private authorizationHeaderValue: string;

  public constructor(authorizationHeaderValue: string) {
    this.authorizationHeaderValue = authorizationHeaderValue;
  }

  public getAuthorizationHeaderValue(): string {
    return this.authorizationHeaderValue;
  }
}
