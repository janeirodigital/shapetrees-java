// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.tests.fixtures
import * as Dispatcher from 'okhttp3/mockwebserver';
import * as MockResponse from 'okhttp3/mockwebserver';
import * as RecordedRequest from 'okhttp3/mockwebserver';
import * as NotNull from 'org/jetbrains/annotations';
import { DispatcherEntry } from './DispatcherEntry';

export class RequestMatchingFixtureDispatcher extends Dispatcher {

   configuredFixtures: Array<DispatcherEntry>;

   private readonly fixtureHitCounts: Map<DispatcherEntry, number> = new Map<>();

  public constructor(configuredFixtures: Array<DispatcherEntry>) {
    this.configuredFixtures = configuredFixtures;
  }

  // @NotNull
  override public dispatch(@NotNull recordedRequest: RecordedRequest): MockResponse {
    for (const entry of configuredFixtures) {
      if (matchesRequest(recordedRequest, entry)) {
        let fixtureName: string = getFixtureName(entry);
        try {
          let resp: MockResponse = Fixture.parseFrom(fixtureName, recordedRequest).toMockResponse();
          // status isn't a number, it's e.g. "HTTP/1.1 200 OK"
          if (resp.getStatus().contains("200") && recordedRequest.getMethod() === "POST") {
            const msg: string = "Mock: response to POST " + recordedRequest + " with " + entry + " returns " + resp.getStatus();
            log.error(msg);
            // This will show up in a stack trace,
            resp.setStatus("HTTP/1.1 999 " + msg);
            // but we can add it as a body as well.
            resp.setBody(msg);
          }
          return resp;
        } catch (ex) {
 if (ex instanceof Exception) {
          let msg: string = ex.getMessage();
          let resp: MockResponse = new MockResponse();
          // log.error(msg);
          ex.printStackTrace();
          // This will show up in a stack trace,
          resp.setStatus("HTTP/1.1 999 " + msg);
          // but we can add it as a body as well.
          resp.setBody(msg);
        }
}
      }
    }
    log.error("Mock: no response found for {} {}", recordedRequest.getMethod(), recordedRequest.getPath());
    return new MockResponse().setResponseCode(404);
  }

  public getFixtureByPath(expectedPath: string): DispatcherEntry {
    for (const entry of this.configuredFixtures) {
      if (entry.getExpectedPath() === expectedPath) {
        return entry;
      }
    }
    return null;
  }

  public removeFixtureByPath(expectedPath: string): void {
    let fixture: DispatcherEntry = getFixtureByPath(expectedPath);
    if (fixture != null) {
      this.configuredFixtures.remove(fixture);
    }
  }

  private getFixtureName(entry: DispatcherEntry): string {
    let hits: number;
    if (!fixtureHitCounts.containsKey(entry)) {
      fixtureHitCounts.put(entry, 1);
      hits = 1;
    } else {
      let existingHits: number = fixtureHitCounts.get(entry);
      existingHits++;
      fixtureHitCounts.replace(entry, existingHits);
      hits = existingHits;
    }
    if (entry.getFixtureNames().size() === 1) {
      return entry.getFixtureNames().get(0);
    } else if (entry.getFixtureNames().size() > 1) {
      let listIndex: number = hits - 1;
      if (listIndex >= entry.getFixtureNames().size()) {
        return entry.getFixtureNames().get(entry.getFixtureNames().size() - 1);
      }
      return entry.getFixtureNames().get(listIndex);
    } else if (entry.getFixtureNames().size() < 1) {
      return null;
    }
    return null;
  }

  private matchesRequest(recordedRequest: RecordedRequest, configuredFixture: DispatcherEntry): boolean {
    if (recordedRequest.getMethod() === null)
      return false;
    if (recordedRequest.getPath() === null)
      return false;
    if (!recordedRequest.getMethod() === configuredFixture.getExpectedMethod())
      return false;
    if (!recordedRequest.getPath() === configuredFixture.getExpectedPath())
      return false;
    if (configuredFixture.getExpectedHeaders() === null)
      return true;
    let recordedHeaders: Map<string, Array<string>> = recordedRequest.getHeaders().toMultimap();
    for (const expectedHeader of configuredFixture.getExpectedHeaders().entrySet()) {
      let expectedHeaderName: string = expectedHeader.getKey();
      if (!recordedHeaders.containsKey(expectedHeaderName))
        return false;
      if (expectedHeader.getValue() === null)
        return true;
      for (const expectedHeaderValue of expectedHeader.getValue()) {
        if (!recordedHeaders.get(expectedHeaderName).contains(expectedHeaderValue))
          return false;
      }
    }
    return true;
  }

  public getConfiguredFixtures(): Array<DispatcherEntry> {
    return this.configuredFixtures;
  }

  public getFixtureHitCounts(): Map<DispatcherEntry, number> {
    return this.fixtureHitCounts;
  }
}
