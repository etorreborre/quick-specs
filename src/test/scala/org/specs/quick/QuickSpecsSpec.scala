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
      val equations = quick(Lists.accept("\\+\\+", "nil"), xs).split("\n")
      equations must have size(2)
    }
    "return a list of equations for a case with 2 variables and one operation" in {
      combineDepth(1)
      quick(Lists.accept("\\+\\+\\+"), xs, ys) must_== List(
        "[+++(xs, ys) == +++(ys, xs)]"
      ).mkString("\n")
    }
    "return a list of equations for a case with 2 variables and two operations" in {
      combineDepth(1)
      quick(Lists.accept("\\+\\+", "nil"), xs, ys).toString must 
        (include("[xs == ++(nil(), xs)]") and include("[xs == ++(xs, nil())]"))
    }
  }
}