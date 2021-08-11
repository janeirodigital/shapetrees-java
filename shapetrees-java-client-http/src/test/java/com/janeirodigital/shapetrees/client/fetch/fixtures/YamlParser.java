package com.janeirodigital.shapetrees.client.fetch.fixtures;

import org.yaml.snakeyaml.Yaml;

class YamlParser implements Parser {
    @Override
    public Fixture parse(String string) {
        return new Yaml().loadAs(string, Fixture.class);
    }
}
