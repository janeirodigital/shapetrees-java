package com.janeirodigital.shapetrees.core.helpers;

import lombok.extern.slf4j.Slf4j;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Convenience methods related to HTTP Headers
 */
@Slf4j
public class HttpHeaderHelper {

    private HttpHeaderHelper() {
    }

    private static final Pattern LINK_HEADER_PATTERN = Pattern.compile("^<(.*?)>\\s*;\\s*rel\\s*=\"(.*?)\"\\s*");

    /**
     * Parse link headers into a map that allows retrieval of one or more values for a given link relation
     * @param headerValues Header values for Link headers
     * @return Map of values parsed using the Link Relation as the key
     */
    public static Map<String, List<String>> parseLinkHeadersToMap(List<String> headerValues) {
        Map<String, List<String>> linkHeaderMap = new HashMap<>();
        if (headerValues == null) {
            // ericP: can be called with headerValues == null
            log.warn("No Link: header to parse");
        } else {
            for (String headerValue : headerValues) {
                Matcher matcher = LINK_HEADER_PATTERN.matcher(headerValue);
                if (matcher.matches() && matcher.groupCount() >= 2) {
                    String uri = matcher.group(1);
                    String rel = matcher.group(2);
                    if (!linkHeaderMap.containsKey(rel)) {
                        linkHeaderMap.put(rel, new ArrayList<>());
                    }
                    linkHeaderMap.get(rel).add(uri);
                } else {
                    log.warn("Unable to parse link header: [{}]", headerValue);
                }
            }
        }
        return linkHeaderMap;
    }
}
