// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.core.enums
import { ShapeTreeVocabulary } from '../vocabularies/ShapeTreeVocabulary';

public enum ShapeTreeResourceType {

  CONTAINER(ShapeTreeVocabulary.CONTAINER), RESOURCE(ShapeTreeVocabulary.RESOURCE), NON_RDF(ShapeTreeVocabulary.NON_RDF_RESOURCE);

   private readonly value: string;

  public getValue(): string {
    return this.value;
  }

  constructor(value: string) {
    this.value = value;
  }
}
