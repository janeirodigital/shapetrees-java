// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.core.enums
public enum LinkRelations {

  DESCRIBED_BY("describedby"), FOCUS_NODE("http://www.w3.org/ns/shapetrees#FocusNode"), MANAGED_BY("http://www.w3.org/ns/shapetrees#managedBy"), MANAGES("http://www.w3.org/ns/shapetrees#manages"), TARGET_SHAPETREE("http://www.w3.org/ns/shapetrees#TargetShapeTree"), TYPE("type"), ACL("acl");

   private readonly value: string;

  public getValue(): string {
    return this.value;
  }

  constructor(value: string) {
    this.value = value;
  }
}
