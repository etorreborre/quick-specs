package org.specs.quick
import org.specs._
import org.specs.quick.expression._
import org.specs.quick.methods._

class QuickSpecsSpec extends SpecificationWithJUnit with QuickSpecs with Sugar with SampleLists {
  noDetailedDiffs()
  "creating quick specs" should {
    "return a list of equations for the most simple case" in {
      quick(Lists.accept("\\+\\+", "nil"), xs).split("\n") must have size(3)
    }
    "return a list of equations for a case with 2 variables" in {
      quick(Lists.accept("\\+\\+", "nil"), xs, ys).split("\n") must have size(7)
    }
    "return a list of equations containing the method name" in {
      quick(Lists, xs) must include("++")
    }
    "include only some methods" in {
      quick(Lists.accept("\\+\\+"), xs) must include("++") and not include("toString")
      quick(Lists.accept("toString"), xs) must include("toString") and not include("++")
    }
  }
}