// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.tests.fixtures
import * as MockResponse from 'okhttp3/mockwebserver';
import * as RecordedRequest from 'okhttp3/mockwebserver';
import * as StringSubstitutor from 'org/apache/commons/text';
import * as TimeUnit from 'java/util/concurrent';
import { YamlParser } from './YamlParser';
import { Parser } from './Parser';

/**
 * Originated from: https://github.com/orhanobut/mockwebserverplus (apache license)
 * Key changes:
 *  - Did not support non-JSON body response
 *  - Added minor token replacement for server base url in fixture contents
 *
 * A value container that holds all information about the fixture file.
 */
export class Fixture {

   public statusCode: number;

   public body: string;

   public headers: Array<string>;

   public delay: number;

  /**
   * Parse the given filename and returns the Fixture object.
   *
   * @param fileName filename should not contain extension or relative path. ie: login
   */
  public static parseFrom(fileName: string, request: RecordedRequest): Fixture {
    return parseFrom(fileName, new YamlParser(), request);
  }

  /**
   * Parse the given filename and returns the Fixture object.
   *
   * @param fileName filename should not contain extension or relative path. ie: login
   * @param parser   parser is required for parsing operation, it should not be null
   */
  public static parseFrom(fileName: string, parser: Parser, request: RecordedRequest): Fixture {
    if (fileName === null) {
      throw new NullPointerException("File name should not be null");
    }
    let path: string = "fixtures/" + fileName + ".yaml";
    let variables: Map<string, string> = new Map<>();
    variables.put("SERVER_BASE", getServerBaseFromRequest(request));
    let substitutor: StringSubstitutor = new StringSubstitutor(variables);
    try {
      return parser.parse(substitutor.replace(readPathIntoString(path)));
    } catch (ex) {
 if (ex instanceof IOException) {
      throw new IllegalStateException("Test Harness: Error reading from " + path + ": " + ex.getStackTrace());
    }
}
  }

  private static getServerBaseFromRequest(request: RecordedRequest): string {
    return request.getRequestUrl().scheme() + "://" + request.getRequestUrl().host() + ":" + request.getRequestUrl().port();
  }

  private static openPathAsStream(path: string): InputStream {
    let loader: ClassLoader = Thread.currentThread().getContextClassLoader();
    let inputStream: InputStream = loader.getResourceAsStream(path);
    if (inputStream === null) {
      throw new IllegalStateException("Test Harness: Invalid path: " + path);
    }
    return inputStream;
  }

  private static readPathIntoString(path: string): string /* throws IOException */ {
    let inputStream: InputStream = openPathAsStream(path);
    let reader: BufferedReader = new BufferedReader(new InputStreamReader(inputStream));
    let out: StringBuilder = new StringBuilder();
    let read: number;
    while ((read = reader.read()) != -1) {
      out.append((char) read);
    }
    reader.close();
    return out.toString();
  }

  public toMockResponse(): MockResponse {
    let mockResponse: MockResponse = new MockResponse();
    if (this.statusCode != 0) {
      mockResponse.setResponseCode(this.statusCode);
    }
    if (this.body != null) {
      mockResponse.setBody(this.body);
    }
    if (this.delay != 0) {
      mockResponse.setBodyDelay(this.delay, TimeUnit.MILLISECONDS);
    }
    if (this.headers != null) {
      for (const header of this.headers) {
        mockResponse.addHeader(header);
      }
    }
    return mockResponse;
  }
}
