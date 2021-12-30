// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.tests
import { ResourceAttributes } from '@shapetrees/core/src/ResourceAttributes';
import * as Assertions from 'org/junit/jupiter/api';
import * as DisplayName from 'org/junit/jupiter/api';
import * as Test from 'org/junit/jupiter/api';

class ResourceAttributesTests {

  // @Test, @DisplayName("Initialize empty ResourceAttributes"), @SneakyThrows
  initializeEmptyResourceAttributes(): void {
    let resourceAttributes: ResourceAttributes = new ResourceAttributes();
    Assertions.assertNotNull(resourceAttributes);
  }

  // @Test, @DisplayName("Initialize ResourceAttributes with value pair"), @SneakyThrows
  initializeResourceAttributesWithPair(): void {
    let attribute: string = "Attribute";
    let value: string = "Value";
    let resourceAttributes: ResourceAttributes = new ResourceAttributes(attribute, value);
    Assertions.assertNotNull(resourceAttributes);
    Assertions.assertTrue(resourceAttributes.toMultimap().containsKey(attribute));
  }

  // @Test, @DisplayName("Initialize ResourceAttributes with Map"), @SneakyThrows
  initializeResourceAttributesWithMap(): void {
    let attributeStrings1: List<string> = new Array<string>();
    let attributeStrings2: List<string> = new Array<string>();
    let attributesMap: Map<string, List<string>> = new Map<string, List<string>>();
    attributeStrings1.add("string1");
    attributeStrings1.add("string2");
    attributeStrings2.add("string3");
    attributeStrings2.add("string4");
    attributesMap.put("attribute1", attributeStrings1);
    attributesMap.put("attribute2", attributeStrings2);
    let resourceAttributes: ResourceAttributes = new ResourceAttributes(attributesMap);
    Assertions.assertNotNull(resourceAttributes);
    Assertions.assertEquals(resourceAttributes.toMultimap(), attributesMap);
  }

  // @Test, @DisplayName("Add pairs with MaybePlus"), @SneakyThrows
  addPairsWithMaybePlus(): void {
    let resourceAttributes: ResourceAttributes = new ResourceAttributes();
    Assertions.assertNotNull(resourceAttributes);
    let resourceAttributes2: ResourceAttributes = resourceAttributes.maybePlus(null, "value");
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
    let resourceAttributes3: ResourceAttributes = resourceAttributes2.maybePlus("Attributes2", "Another First Value");
    Assertions.assertNotEquals(resourceAttributes2, resourceAttributes3);
    Assertions.assertFalse(resourceAttributes3.toMultimap().isEmpty());
    Assertions.assertTrue(resourceAttributes3.toMultimap().containsKey("Attributes"));
    Assertions.assertTrue(resourceAttributes3.toMultimap().containsKey("Attributes2"));
  }

  // @Test, @DisplayName("Add pairs with MaybeSet"), @SneakyThrows
  addPairsWithMaybeSet(): void {
    let resourceAttributes: ResourceAttributes = new ResourceAttributes();
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
