package com.janeirodigital.shapetrees.core;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Slf4j
public class HttpClientHeaders {
    Map<String, List<String>> headers = new HashMap<>();

    public HttpClientHeaders() {
    }

    public HttpClientHeaders(String attr, String value) throws ShapeTreeException {
        if (attr == null || value == null) throw new ShapeTreeException(500, "Malforned header (" + attr + ") or value (" + value + ")");
        this.set(attr, value);
    }

    public HttpClientHeaders(Map<String, List<String>> headers) {
        this.headers = headers;
    }

    public HttpClientHeaders plus(String attr, String value) {
        HttpClientHeaders ret = new HttpClientHeaders();
        for (Map.Entry<String, List<String>> entry : headers.entrySet()) {
            ret.headers.put(entry.getKey(), new ArrayList<>(entry.getValue()));
        }
        set(attr, value);
        return ret;
    }

    private static final Pattern LINK_HEADER_PATTERN = Pattern.compile("^<(.*?)>\\s*;\\s*rel\\s*=\"(.*?)\"\\s*");

    /**
     * Parse link headers into a map that allows retrieval of one or more values for a given link relation
     * @param headerValues Header values for Link headers
     * @return Map of values parsed using the Link Relation as the key
     */
    public static HttpClientHeaders parseLinkHeaders(List<String> headerValues) {
        HttpClientHeaders linkHeaderMap = new HttpClientHeaders();
        for (String headerValue : headerValues) {
            Matcher matcher = LINK_HEADER_PATTERN.matcher(headerValue);
            if (matcher.matches() && matcher.groupCount() >= 2) {
                String uri = matcher.group(1);
                String rel = matcher.group(2);
                linkHeaderMap.headers.computeIfAbsent(rel, k -> new ArrayList<>());
                linkHeaderMap.headers.get(rel).add(uri);
            } else {
                log.warn("Unable to parse link header: [{}]", headerValue);
            }
        }
        return linkHeaderMap;
    }

    public void set(String attr, String value) {
        if (headers.containsKey(attr)) {
            headers.get(attr).add(value);
            } else {
            ArrayList<String> list = new ArrayList<String>();
            list.add(value);
            headers.put(attr, list);
        }
    }
    public void set(String attr, List<String> values) {
        headers.put(attr, values);
    }

    public Iterable<? extends Map.Entry<String, List<String>>> entrySet() {
        return headers.entrySet();
    }

    public boolean containsKey(String value) {
        return headers.containsKey(value);
    }

    public List<String> get(String value) {
        return headers.get(value);
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        for (Map.Entry<String, List<String>> entry : headers.entrySet()) {
            for (String value : entry.getValue()) {
                if (sb.length() != 0) {
                    sb.append(",");
                }
                sb.append(entry.getKey()).append("=").append(value);
            }
        }

        return sb.toString();
    }

    public List<String> computeIfAbsent(String header, Function<? super String, ? extends List<String>> f) { // Function <? super String, ? extends List<String>>
        return headers.computeIfAbsent(header, f);
    }
}

