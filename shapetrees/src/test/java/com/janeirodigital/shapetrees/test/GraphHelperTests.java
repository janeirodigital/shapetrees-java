package com.janeirodigital.shapetrees.test;

import com.janeirodigital.shapetrees.helper.GraphHelper;
import lombok.SneakyThrows;
import org.apache.jena.riot.Lang;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class GraphHelperTests {
    @Test
    @DisplayName("Null content type")
    @SneakyThrows
    void handleNullContentType() {
        Lang lang = GraphHelper.getLangForContentType(null);
        assertEquals(lang, Lang.TURTLE);
    }

    @Test
    @DisplayName("Turtle content type")
    @SneakyThrows
    void handleTurtleContentType() {
        Lang lang = GraphHelper.getLangForContentType("text/turtle");
        assertEquals(lang, Lang.TURTLE);
    }

    @Test
    @DisplayName("Turtle content type default")
    @SneakyThrows
    void handleDefaultContentType() {
        Lang lang = GraphHelper.getLangForContentType("something/bogus");
        assertEquals(lang, Lang.TURTLE);
    }

    @Test
    @DisplayName("JSON LD content type")
    @SneakyThrows
    void handleJsonLD() {
        Lang lang = GraphHelper.getLangForContentType("application/ld+json");
        assertEquals(lang, Lang.JSONLD);
    }

}
