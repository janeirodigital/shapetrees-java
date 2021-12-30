// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.tests.fixtures
import * as Yaml from 'org/yaml/snakeyaml';
import { Fixture } from './Fixture';
import { Parser } from './Parser';

class YamlParser implements Parser {

  override public parse(string: string): Fixture {
    return new Yaml().loadAs(string, Fixture.class);
  }
}
