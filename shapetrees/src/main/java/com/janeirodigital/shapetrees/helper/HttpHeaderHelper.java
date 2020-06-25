package com.janeirodigital.shapetrees.helper;

import lombok.extern.slf4j.Slf4j;
import org.apache.http.Header;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Slf4j
public class HttpHeaderHelper {

    private static final Pattern LINK_HEADER_PATTERN = Pattern.compile("^<(.*?)>\\s*;\\s*rel\\s*=\"(.*?)\"\\s*");

    public static Map<String, List<String>> parseHeadersToMap(Header[] headers) {
        Map<String, List<String>> headerMap = new HashMap<>();
        for (Header header : headers) {
            if (!headerMap.containsKey(header.getName())) {
                headerMap.put(header.getName(), new ArrayList<>());
            }
            headerMap.get(header.getName()).add(header.getValue());
        }
        return headerMap;
    }

    public static Map<String, List<String>> parseLinkHeadersToMap(Header[] linkHeaders) {
        Map<String, List<String>> linkHeaderMap = new HashMap<>();
        for (Header linkHeader : linkHeaders) {
            Matcher matcher = LINK_HEADER_PATTERN.matcher(linkHeader.getValue());
            if (matcher.matches() && matcher.groupCount() >= 2) {
                String uri = matcher.group(1);
                String rel = matcher.group(2);
                if (!linkHeaderMap.containsKey(rel)) {
                    linkHeaderMap.put(rel, new ArrayList<>());
                }
                linkHeaderMap.get(rel).add(uri);
            } else {
                log.warn("Unable to parse link header: [{}]", linkHeader.getValue());
            }
        }
        return linkHeaderMap;
    }

}
