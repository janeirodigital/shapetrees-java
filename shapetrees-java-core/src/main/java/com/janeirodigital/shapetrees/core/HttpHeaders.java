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

/**
 * The HttpClientHeaders object is a multi-map with some constructors and put-ers tailored to the
 * shapetrees-java libraries. The only behavior that's at all HTTP-specific is the
 * parseLinkHeaders(List<String> headerValues) factory which includes logic for HTTP Link headers.
 */
@Slf4j
public class HttpHeaders {
    Map<String, List<String>> myMapOfLists = new HashMap<>();

    /**
     * construct an empty HttpClientHeaders container
     */
    public HttpHeaders() {
    }

    /**
     * construct an HttpClientHeaders container and set attr to value if both are not null.
     * @param attr attribute (header) name to set
     * @param value String value to assign to attr
     */
    public HttpHeaders(String attr, String value) throws ShapeTreeException {
        this.maybeSet(attr, value);
    }

    /**
     * replace current map with passed map.
     * @param newMap replacement for myMapOfLists
     */
    public HttpHeaders(Map<String, List<String>> newMap) {
        this.myMapOfLists = newMap;
    }

    /**
     * Re-use HttpClientHeaders to capture link headers as a mapping from link relation to list of values
     * This is really a constructor but a named static function clarifies its intention.
     * @param headerValues Header values for Link headers
     * @return subset of this matching the pattern
     */
    public static HttpHeaders parseLinkHeaders(List<String> headerValues) {
        HttpHeaders linkHeaderMap = new HttpHeaders();
        for (String headerValue : headerValues) {
            Matcher matcher = LINK_HEADER_PATTERN.matcher(headerValue);
            if (matcher.matches() && matcher.groupCount() >= 2) {
                String uri = matcher.group(1);
                String rel = matcher.group(2);
                linkHeaderMap.myMapOfLists.computeIfAbsent(rel, k -> new ArrayList<>());
                linkHeaderMap.myMapOfLists.get(rel).add(uri);
            } else {
                log.warn("Unable to parse link header: [{}]", headerValue);
            }
        }
        return linkHeaderMap;
    }

    // copy constructor
    private HttpHeaders copy() {
        HttpHeaders ret = new HttpHeaders();
        for (Map.Entry<String, List<String>> entry : myMapOfLists.entrySet()) {
            ret.myMapOfLists.put(entry.getKey(), new ArrayList<>(entry.getValue()));
        }
        return ret;
    }

    /**
     * make a new HttpClientHeaders with the additional attr/value set.
     * @param attr attribute (header) name to set
     * @param value String value to assign to attr
     * @returns original HttpClientHeaders if no change is made; otherwise a new copy.
     */
    public HttpHeaders maybePlus(String attr, String value) {
        if (attr == null || value == null) {
            return this;
        }
        HttpHeaders ret = copy();
        ret.maybeSet(attr, value);
        return ret;
    }

    /**
     * set attr to value if both are not null.
     * @param attr attribute (header) name to set
     * @param value String value to assign to attr
     */
    public void maybeSet(String attr, String value) {
        if (attr == null || value == null) {
            return;
        }

        if (myMapOfLists.containsKey(attr)) {
            myMapOfLists.get(attr).add(value);
        } else {
            ArrayList<String> list = new ArrayList<String>();
            list.add(value);
            myMapOfLists.put(attr, list);
        }
    }

    /**
     * replaces the list of attrs (without regard to nulls)
     * @param attr attribute (header) name to set
     * @param values String values to assign to attr
     */
    public void setAll(String attr, List<String> values) {
        myMapOfLists.put(attr, values);
    }

    // Pass-through functions to headers - could be simpler if HttpClientHeaders extends a HashMap rather than contains a it.
    public Iterable<? extends Map.Entry<String, List<String>>> entrySet() {
        return myMapOfLists.entrySet();
    }

    public boolean containsKey(String value) {
        return myMapOfLists.containsKey(value);
    }

    public List<String> get(String value) {
        return myMapOfLists.get(value);
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        for (Map.Entry<String, List<String>> entry : myMapOfLists.entrySet()) {
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
        return myMapOfLists.computeIfAbsent(header, f);
    }

    private static final Pattern LINK_HEADER_PATTERN = Pattern.compile("^<(.*?)>\\s*;\\s*rel\\s*=\"(.*?)\"\\s*");
}

