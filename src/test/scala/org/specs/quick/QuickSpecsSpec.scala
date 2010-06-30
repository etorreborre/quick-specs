package org.specs.quick
import org.specs._
import org.specs.quick.expression._
import org.specs.quick.methods._
import org.specs.log._

class QuickSpecsSpec extends SpecificationWithJUnit with QuickSpecs with Sugar with SampleLists with SampleVariables with Log {
  noDetailedDiffs()
  "creating quick specs" should {
    "return a list of equations for the most simple case" in {
      combineDepth(1)
      level = Debug
      val equations = quick(Lists.accept("\\+\\+", "nil"), xs).split("\n")
      equations must have size(2)
    }
    "return a list of equations for a case with 2 variables and one operation" in {
      combineDepth(1)
      quick(Lists.accept("\\+\\+\\+"), xs, ys) must_== List(
        "[+++(xs, ys) == +++(ys, xs)]"
      ).mkString("\n")
    }
    "return a list of equations for a case with 2 variables" in {
      combineDepth(1)
      quick(Lists.accept("\\+\\+", "nil"), xs, ys) must_== List(
        "[xs == ++(nil(), xs)]",
        "[xs == ++(xs, nil())]"
      ).mkString("\n")
    }
    "return a list of equations containing the method name" in {
      1 must_== 1 //quick(Lists, xs) must include("++")
    }
    "include only some methods" in {
      quick(Lists.accept("\\+\\+"), xs) must include("++") and not include("toString")
      quick(Lists.accept("toString"), xs) must include("toString") and not include("++")
    }
  }
}