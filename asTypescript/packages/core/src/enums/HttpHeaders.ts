// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.core.enums
public enum HttpHeaders {

  ACCEPT("Accept"),
  AUTHORIZATION("Authorization"),
  CONTENT_TYPE("Content-Type"),
  LINK("Link"),
  LOCATION("Location"),
  SLUG("Slug"),
  INTEROP_ORIGINATOR("InteropOrigin"),
  INTEROP_WEBID("InteropWebID");

  public getValue(): string {
    return this.value;
  }

   private readonly value: string;

  constructor(value: string) {
    this.value = value;
  }
}
