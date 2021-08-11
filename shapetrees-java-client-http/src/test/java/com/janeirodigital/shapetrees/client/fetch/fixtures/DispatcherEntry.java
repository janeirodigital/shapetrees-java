package com.janeirodigital.shapetrees.client.fetch.fixtures;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

import java.util.List;
import java.util.Map;

@Getter @Setter @AllArgsConstructor
public class DispatcherEntry {
    private List<String> fixtureNames;
    private String expectedMethod;
    private String expectedPath;
    private Map<String, List<String>> expectedHeaders;
}
