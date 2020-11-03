package com.janeirodigital.shapetrees.test.fixtures;

import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import okhttp3.mockwebserver.Dispatcher;
import okhttp3.mockwebserver.MockResponse;
import okhttp3.mockwebserver.RecordedRequest;
import org.jetbrains.annotations.NotNull;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Slf4j
public class RequestMatchingFixtureDispatcher extends Dispatcher {

    private final List<DispatcherEntry> configuredFixtures;
    private final Map<DispatcherEntry, Integer> fixtureHitCounts = new HashMap<>();

    public RequestMatchingFixtureDispatcher(List<DispatcherEntry> configuredFixtures) {
        this.configuredFixtures = configuredFixtures;
    }

    @SneakyThrows
    @NotNull
    @Override
    public MockResponse dispatch(@NotNull RecordedRequest recordedRequest) {
        for (DispatcherEntry entry : configuredFixtures) {
            if (matchesRequest(recordedRequest, entry)) {
                String fixtureName = getFixtureName(entry);
                return Fixture.parseFrom(fixtureName, recordedRequest).toMockResponse();
            }
        }
        return new MockResponse().setResponseCode(404);
    }

    private String getFixtureName(DispatcherEntry entry) {
        int hits;
        if (!fixtureHitCounts.containsKey(entry)) {
            fixtureHitCounts.put(entry, 1);
            hits = 1;
        } else {
            Integer existingHits = fixtureHitCounts.get(entry);
            existingHits++;
            fixtureHitCounts.replace(entry, existingHits);
            hits = existingHits;
        }

        if (entry.getFixtureNames().size() == 1) {
            return entry.getFixtureNames().get(0);
        } else if (entry.getFixtureNames().size() > 1) {
            int listIndex = hits - 1;
            if (listIndex >= entry.getFixtureNames().size()) {
                return entry.getFixtureNames().get(entry.getFixtureNames().size()-1);
            }
            return entry.getFixtureNames().get(listIndex);
        } else if (entry.getFixtureNames().size() < 1) {
            return null;
        }
        return null;
    }

    private boolean matchesRequest(RecordedRequest recordedRequest, DispatcherEntry configuredFixture) {
        if (recordedRequest.getMethod() == null) return false;
        if (recordedRequest.getPath() == null) return false;

        if (!recordedRequest.getMethod().equals(configuredFixture.getExpectedMethod())) return false;
        if (!recordedRequest.getPath().equals(configuredFixture.getExpectedPath())) return false;

        if (configuredFixture.getExpectedHeaders() == null) return true;

        Map<String, List<String>> recordedHeaders = recordedRequest.getHeaders().toMultimap();
        for (Map.Entry<String, List<String>> expectedHeader : configuredFixture.getExpectedHeaders().entrySet()) {
            String expectedHeaderName = expectedHeader.getKey();

            if (!recordedHeaders.containsKey(expectedHeaderName)) return false;
            if (expectedHeader.getValue() == null) return true;

            for (String expectedHeaderValue : expectedHeader.getValue()) {
                if (!recordedHeaders.get(expectedHeaderName).contains(expectedHeaderValue)) return false;
            }
        }
        return true;
    }
}
