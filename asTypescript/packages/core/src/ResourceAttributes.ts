// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.core
import { ShapeTreeException } from './exceptions/ShapeTreeException';
import * as TreeMap from 'java/util';
import * as Arrays from 'java/util';
import * as Matcher from 'java/util/regex';
import * as Pattern from 'java/util/regex';
import * as requireNonNull from 'java/util/Objects';

/**
 * The HttpClientHeaders object is a multi-map with some constructors and put-ers tailored to the
 * shapetrees-java libraries. The only behavior that's at all HTTP-specific is the
 * parseLinkHeaders factory which includes logic for HTTP Link headers.
 */
export class ResourceAttributes {

   myMapOfLists: Map<string, Array<string>>;

  /**
   * construct a case-insensitive ResourceAttributes container
   */
  public constructor() {
    this.myMapOfLists = new TreeMap<>(String.CASE_INSENSITIVE_ORDER);
  }

  /**
   * construct a case-insensitive ResourceAttributes container and set attr to value if both are not null.
   * @param attr attribute (header) name to set
   * @param value String value to assign to attr
   */
  public constructor(attr: string, value: string) /* throws ShapeTreeException */ {
    this.myMapOfLists = new TreeMap<>(String.CASE_INSENSITIVE_ORDER);
    this.maybeSet(attr, value);
  }

  /**
   * Construct ResourceAttributes with passed map, which may be case-sensitive.
   * @param newMap replacement for myMapOfLists
   */
  public constructor(newMap: Map<string, Array<string>>) {
    this.myMapOfLists = newMap;
  }

  // copy constructor
  private copy(): ResourceAttributes {
    let ret: ResourceAttributes = new ResourceAttributes();
    for (const entry of this.myMapOfLists.entrySet()) {
      ret.myMapOfLists.put(entry.getKey(), new Array<>(entry.getValue()));
    }
    return ret;
  }

  /**
   * Re-use HttpClientHeaders to capture link headers as a mapping from link relation to list of values
   * This is really a constructor but a named static function clarifies its intention.
   * @param headerValues Header values for Link headers
   * @return subset of this matching the pattern
   */
  public static parseLinkHeaders(headerValues: Array<string>): ResourceAttributes {
    let linkHeaderMap: ResourceAttributes = new ResourceAttributes();
    for (const headerValue of headerValues) {
      let matcher: Matcher = LINK_HEADER_PATTERN.matcher(headerValue);
      // if (matcher.matches() && matcher.groupCount() >= 2) {
      if (matcher.matches()) {
        let uri: string = matcher.group(1);
        let rel: string = matcher.group(2);
        linkHeaderMap.myMapOfLists.computeIfAbsent(rel, k -> new Array<>());
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
   * @return original HttpClientHeaders if no change is made; otherwise a new copy.
   */
  public maybePlus(attr: string, value: string): ResourceAttributes {
    if (attr === null || value === null) {
      return this;
    }
    let ret: ResourceAttributes = copy();
    ret.maybeSet(attr, value);
    return ret;
  }

  /**
   * set attr to value if both are not null.
   * @param attr attribute (header) name to set
   * @param value String value to assign to attr
   */
  /*@SneakyThrows*/
  public maybeSet(attr: string, value: string): void {
    if (attr === null || value === null) {
      return;
    }
    if (this.myMapOfLists.containsKey(attr)) {
      let existingValues: Array<string> = this.myMapOfLists.get(attr);
      let alreadySet: boolean = existingValues.stream().anyMatch(s -> s === value);
      if (!alreadySet) {
        existingValues.add(value);
      }
      /* else {
                throw new Exception(attr + ": " + value + " already set.");
            }*/
    } else {
      let list: Array<string> = new Array<string>();
      list.add(value);
      this.myMapOfLists.put(attr, list);
    }
  }

  /**
   * replaces the list of attrs (without regard to nulls)
   * @param attr attribute (header) name to set
   * @param values String values to assign to attr
   */
  public setAll(attr: string, values: Array<string>): void {
    this.myMapOfLists.put(attr, values);
  }

  /**
   * Returns a map of attributes to lists of values
   */
  public toMultimap(): Map<string, Array<string>> {
    return this.myMapOfLists;
  }

  /**
   * Returns an array with alternating attributes and values.
   * @param exclusions set of headers to exclude from returned array.
   *                   (This is useful for HttpRequest.Builder().)
   */
  public toList(...exclusions: string): string[] {
    let ret: Array<string> = new Array<>();
    for (const entry of this.myMapOfLists.entrySet()) {
      let attr: string = entry.getKey();
      if (!Arrays.stream(exclusions).anyMatch(s -> s === attr)) {
        for (const value of entry.getValue()) {
          ret.add(attr);
          ret.add(value);
        }
      }
    }
    return ret.stream().toArray(string[]::new);
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
  public firstValue(name: string): string | null {
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
  public allValues(name: string): Array<string> {
    requireNonNull(name);
    let values: Array<string> = toMultimap().get(name);
    // Making unmodifiable list out of empty in order to make a list which
    // throws UOE unconditionally
    return values != null ? values : List.of();
  }

  public toString(): string {
    let sb: StringBuilder = new StringBuilder();
    for (const entry of this.myMapOfLists.entrySet()) {
      for (const value of entry.getValue()) {
        if (sb.length() != 0) {
          sb.append(",");
        }
        sb.append(entry.getKey()).append("=").append(value);
      }
    }
    return sb.toString();
  }

   private static readonly LINK_HEADER_PATTERN: Pattern = Pattern.compile("^<(.*?)>\\s*;\\s*rel\\s*=\"(.*?)\"\\s*");
}
