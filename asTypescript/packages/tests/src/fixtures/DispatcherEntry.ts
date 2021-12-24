// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.tests.fixtures
export class DispatcherEntry {

   private fixtureNames: Array<string>;

   private expectedMethod: string;

   private expectedPath: string;

   private expectedHeaders: Map<string, Array<string>>;

  public toString(): string {
    return "DispatcherEntry{" + fixtureNames + ":" + expectedMethod + '\'' + " " + expectedPath + '\'' + " " + expectedHeaders + "}";
  }

  public constructor(fixtureNames: Array<string>, expectedMethod: string, expectedPath: string, expectedHeaders: Map<string, Array<string>>) {
    this.fixtureNames = fixtureNames;
    this.expectedMethod = expectedMethod;
    this.expectedPath = expectedPath;
    this.expectedHeaders = expectedHeaders;
  }

  public getFixtureNames(): Array<string> {
    return this.fixtureNames;
  }

  public getExpectedMethod(): string {
    return this.expectedMethod;
  }

  public getExpectedPath(): string {
    return this.expectedPath;
  }

  public getExpectedHeaders(): Map<string, Array<string>> {
    return this.expectedHeaders;
  }
}
