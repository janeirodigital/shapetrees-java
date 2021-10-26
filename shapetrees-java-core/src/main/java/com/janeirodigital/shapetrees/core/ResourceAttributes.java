package com.janeirodigital.shapetrees.core;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static java.util.Objects.requireNonNull;

/**
 * The HttpClientHeaders object is a multi-map with some constructors and put-ers tailored to the
 * shapetrees-java libraries. The only behavior that's at all HTTP-specific is the
 * parseLinkHeaders(List<String> headerValues) factory which includes logic for HTTP Link headers.
 */
@Slf4j
public class ResourceAttributes {
    Map<String, List<String>> myMapOfLists;

    /**
     * construct a case-insensitive ResourceAttributes container
     */
    public ResourceAttributes() {
        this.myMapOfLists = new TreeMap<>(String.CASE_INSENSITIVE_ORDER);
    }

    /**
     * construct a case-insensitive ResourceAttributes container and set attr to value if both are not null.
     * @param attr attribute (header) name to set
     * @param value String value to assign to attr
     */
    public ResourceAttributes(String attr, String value) throws ShapeTreeException {
        this.myMapOfLists = new TreeMap<>(String.CASE_INSENSITIVE_ORDER);
        this.maybeSet(attr, value);
    }

    /**
     * Construct ResourceAttributes with passed map, which may be case-sensitive.
     * @param newMap replacement for myMapOfLists
     */
    public ResourceAttributes(Map<String, List<String>> newMap) {
        this.myMapOfLists = newMap;
    }

    // copy constructor
    private ResourceAttributes copy() {
        ResourceAttributes ret = new ResourceAttributes();
        for (Map.Entry<String, List<String>> entry : this.myMapOfLists.entrySet()) {
            ret.myMapOfLists.put(entry.getKey(), new ArrayList<>(entry.getValue()));
        }
        return ret;
    }

    /**
     * Re-use HttpClientHeaders to capture link headers as a mapping from link relation to list of values
     * This is really a constructor but a named static function clarifies its intention.
     * @param headerValues Header values for Link headers
     * @return subset of this matching the pattern
     */
    public static ResourceAttributes parseLinkHeaders(List<String> headerValues) {
        ResourceAttributes linkHeaderMap = new ResourceAttributes();
        for (String headerValue : headerValues) {
            Matcher matcher = LINK_HEADER_PATTERN.matcher(headerValue);
            // if (matcher.matches() && matcher.groupCount() >= 2) {
            if (matcher.matches()) {
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

    /**
     * make a new HttpClientHeaders with the additional attr/value set.
     * @param attr attribute (header) name to set
     * @param value String value to assign to attr
     * @returns original HttpClientHeaders if no change is made; otherwise a new copy.
     */
    public ResourceAttributes maybePlus(String attr, String value) {
        if (attr == null || value == null) {
            return this;
        }
        ResourceAttributes ret = copy();
        ret.maybeSet(attr, value);
        return ret;
    }

    /**
     * set attr to value if both are not null.
     * @param attr attribute (header) name to set
     * @param value String value to assign to attr
     */
    /*@SneakyThrows*/
    public void maybeSet(String attr, String value) {
        if (attr == null || value == null) {
            return;
        }

        if (this.myMapOfLists.containsKey(attr)) {
            List<String> existingValues = this.myMapOfLists.get(attr);
            boolean alreadySet = existingValues.stream().anyMatch(s -> s.equals(value));
            if (!alreadySet) {
                existingValues.add(value);
            }/* else {
                throw new Exception(attr + ": " + value + " already set.");
            }*/
        } else {
            ArrayList<String> list = new ArrayList<String>();
            list.add(value);
            this.myMapOfLists.put(attr, list);
        }
    }

    /**
     * replaces the list of attrs (without regard to nulls)
     * @param attr attribute (header) name to set
     * @param values String values to assign to attr
     */
    public void setAll(String attr, List<String> values) {
        this.myMapOfLists.put(attr, values);
    }

    /**
     * Returns a map of attributes to lists of values
     */
    public Map<String, List<String>> toMultimap() { return this.myMapOfLists; }

    /**
     * Returns an array with alternating attributes and values.
     * @param exclusions set of headers to exclude from returned array.
     *                   (This is useful for HttpRequest.Builder().)
     */
    public String[] toList(String... exclusions) {
        List<String> ret = new ArrayList<>();
        for (Map.Entry<String, List<String>> entry : this.myMapOfLists.entrySet()) {
            String attr = entry.getKey();
            if (!Arrays.stream(exclusions).anyMatch(s -> s.equals(attr))) {
                for (String value : entry.getValue()) {
                    ret.add(attr);
                    ret.add(value);
                }
            }
        }
        return ret.stream().toArray(String[]::new);
    }

    /**
     * Returns an {@link Optional} containing the first header string value of
     * the given named (and possibly multi-valued) header. If the header is not
     * present, then the returned {@code Optional} is empty.
     *
     * @param name the header name
     * @return an {@code Optional<String>} containing the first named header
     *         string value, if present
     */
    public Optional<String> firstValue(String name) {
        return allValues(name).stream().findFirst();
    }

    /**
     * Returns an unmodifiable List of all of the header string values of the
     * given named header. Always returns a List, which may be empty if the
     * header is not present.
     *
     * @param name the header name
     * @return a List of headers string values
     */
    public List<String> allValues(String name) {
        requireNonNull(name);
        List<String> values = toMultimap().get(name);
        // Making unmodifiable list out of empty in order to make a list which
        // throws UOE unconditionally
        return values != null ? values : List.of();
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        for (Map.Entry<String, List<String>> entry : this.myMapOfLists.entrySet()) {
            for (String value : entry.getValue()) {
                if (sb.length() != 0) {
                    sb.append(",");
                }
                sb.append(entry.getKey()).append("=").append(value);
            }
        }

        return sb.toString();
    }

    private static final Pattern LINK_HEADER_PATTERN = Pattern.compile("^<(.*?)>\\s*;\\s*rel\\s*=\"(.*?)\"\\s*");
}

