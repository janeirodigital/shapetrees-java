// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.core.exceptions
export class ShapeTreeException extends Exception {

   private readonly statusCode: number;

   private readonly message: string;

  public constructor(statusCode: number, message: string) {
    super;
    this.statusCode = statusCode;
    this.message = message;
  }

  public getStatusCode(): number {
    return this.statusCode;
  }

  public getMessage(): string {
    return this.message;
  }
}
