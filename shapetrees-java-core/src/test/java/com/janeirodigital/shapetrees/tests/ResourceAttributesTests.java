package com.janeirodigital.shapetrees.tests;

import com.janeirodigital.shapetrees.core.resources.ResourceAttributes;
import lombok.SneakyThrows;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.util.*;

class ResourceAttributesTests {

    @Test
    @DisplayName("Initialize empty ResourceAttributes")
    @SneakyThrows
    void initializeEmptyResourceAttributes() {
        ResourceAttributes resourceAttributes = new ResourceAttributes();
        Assertions.assertNotNull(resourceAttributes);
    }

    @Test
    @DisplayName("Initialize ResourceAttributes with value pair")
    @SneakyThrows
    void initializeResourceAttributesWithPair() {

        String attribute = "Attribute";
        String value = "Value";

        ResourceAttributes resourceAttributes = new ResourceAttributes(attribute, value);

        Assertions.assertNotNull(resourceAttributes);
        Assertions.assertTrue(resourceAttributes.toMultimap().containsKey(attribute));

    }

    @Test
    @DisplayName("Initialize ResourceAttributes with Map")
    @SneakyThrows
    void initializeResourceAttributesWithMap() {

        List<String> attributeStrings1 = new ArrayList<String>();
        List<String> attributeStrings2 = new ArrayList<String>();
        Map<String, List<String>> attributesMap = new HashMap<String, List<String>>();

        attributeStrings1.add("string1");
        attributeStrings1.add("string2");
        attributeStrings2.add("string3");
        attributeStrings2.add("string4");

        attributesMap.put("attribute1", attributeStrings1);
        attributesMap.put("attribute2", attributeStrings2);

        ResourceAttributes resourceAttributes = new ResourceAttributes(attributesMap);
        Assertions.assertNotNull(resourceAttributes);
        Assertions.assertEquals(resourceAttributes.toMultimap(), attributesMap);
    }

    @Test
    @DisplayName("Add pairs with MaybePlus")
    @SneakyThrows
    void addPairsWithMaybePlus() {

        ResourceAttributes resourceAttributes = new ResourceAttributes();
        Assertions.assertNotNull(resourceAttributes);

        ResourceAttributes resourceAttributes2 = resourceAttributes.maybePlus(null, "value");
        Assertions.assertTrue(resourceAttributes2.toMultimap().isEmpty());
        resourceAttributes2 = resourceAttributes.maybePlus("attribute", null);
        Assertions.assertTrue(resourceAttributes2.toMultimap().isEmpty());
        resourceAttributes2 = resourceAttributes.maybePlus(null, null);
        Assertions.assertTrue(resourceAttributes2.toMultimap().isEmpty());
        Assertions.assertEquals(resourceAttributes, resourceAttributes2);

        resourceAttributes2 = resourceAttributes.maybePlus("Attributes", "First Value");
        Assertions.assertNotEquals(resourceAttributes, resourceAttributes2);
        Assertions.assertFalse(resourceAttributes2.toMultimap().isEmpty());
        Assertions.assertTrue(resourceAttributes2.toMultimap().containsKey("Attributes"));

        ResourceAttributes resourceAttributes3 = resourceAttributes2.maybePlus("Attributes2", "Another First Value");
        Assertions.assertNotEquals(resourceAttributes2, resourceAttributes3);
        Assertions.assertFalse(resourceAttributes3.toMultimap().isEmpty());
        Assertions.assertTrue(resourceAttributes3.toMultimap().containsKey("Attributes"));
        Assertions.assertTrue(resourceAttributes3.toMultimap().containsKey("Attributes2"));

    }

    @Test
    @DisplayName("Add pairs with MaybeSet")
    @SneakyThrows
    void addPairsWithMaybeSet() {

        ResourceAttributes resourceAttributes = new ResourceAttributes();
        Assertions.assertNotNull(resourceAttributes);

        resourceAttributes.maybeSet(null, "value");
        Assertions.assertTrue(resourceAttributes.toMultimap().isEmpty());
        resourceAttributes.maybeSet("attribute", null);
        Assertions.assertTrue(resourceAttributes.toMultimap().isEmpty());
        resourceAttributes.maybeSet(null, null);
        Assertions.assertTrue(resourceAttributes.toMultimap().isEmpty());

        resourceAttributes.maybeSet("First Attribute", "First Attribute First Value");
        Assertions.assertEquals(resourceAttributes.firstValue("First Attribute"), Optional.of("First Attribute First Value"));

        // Try to reset with the same attribute and value (to no change)
        resourceAttributes.maybeSet("First Attribute", "First Attribute First Value");
        Assertions.assertEquals(1, resourceAttributes.toMultimap().size());
        Assertions.assertEquals(Optional.of("First Attribute First Value"), resourceAttributes.firstValue("First Attribute"));

        // Add to the same attribute with a different value, growing the list size
        resourceAttributes.maybeSet("First Attribute", "First Attribute Second Value");
        Assertions.assertEquals(1, resourceAttributes.toMultimap().size());
        Assertions.assertEquals(2, resourceAttributes.allValues("First Attribute").size());

    }

}
