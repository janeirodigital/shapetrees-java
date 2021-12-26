// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.core.comparators
import { ShapeTree } from '../ShapeTree';
import { ShapeTreeFactory } from '../ShapeTreeFactory';
import * as Comparator from 'java/util';

export class ShapeTreeContainsPriority implements Comparator<URL>, Serializable {

  // Used for sorting shape trees in st:contains by most to least strict
  // @SneakyThrows
  override public compare(stUrl1: URL, stUrl2: URL): number {
    let st1: ShapeTree = ShapeTreeFactory.getShapeTree(stUrl1);
    let st2: ShapeTree = ShapeTreeFactory.getShapeTree(stUrl2);
    let st1Priority: number = 0;
    let st2Priority: number = 0;
    if (st1.getShape() != null) {
      st1Priority += 2;
    }
    if (st1.getLabel() != null) {
      st1Priority++;
    }
    // st:expectsType is required so it doesn't affect score priority
    if (st2.getShape() != null) {
      st2Priority += 2;
    }
    if (st2.getLabel() != null) {
      st2Priority++;
    }
    // Reversed to ensure ordering goes from most strict to least
    return Integer.compare(st2Priority, st1Priority);
  }
}
