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
      val equations = quick(Lists.accept("\\+\\+", "nil"), lists, xs).split("\n")
      equations must have size(2)
    }
    "return a list of equations for a case with 2 variables and one operation" in {
      combineDepth(1)
      quick(Lists.accept("\\+\\+\\+"), lists, xs, ys) must_== List(
        "[lists.+++(xs, ys) == lists.+++(ys, xs)]"
      ).mkString("\n")
    }
    "return a list of equations for a case with 2 variables and two operations" in {
      combineDepth(1)
      level = Debug
      quick(Lists.accept("\\+\\+", "nil"), lists, xs, ys).split("\n").toList must have size(2)
      quick(Lists.accept("\\+\\+", "nil"), lists, xs, ys).toString must include("[xs == lists.++(lists.nil(), xs)]") and include("[xs == lists.++(xs, lists.nil())]") 
    }
  }
}