package org.specs.quick.expression
import org.specs._
import org.specs.quick.methods._
import org.specs.quick.SampleLists
import org.scalacheck.Gen
import org.scalacheck.util._

class CombineSpec extends Specification with ExpressionsCombiner with SampleLists {
  "the combine method" should {
    "combine an expression taking a 2 variables and 1 variable in 2 expressions: a, exp(a, a)" in {
      combine(List(plusPlus), List(xs)).expressions.toString must_== "List(++(xs, xs), xs)"
    }
    "combine an expression taking a 2 variables and 1 variables in 6 expressions: \n" + 
    "a, b, exp(a, b), exp(b, a), exp(a, a), exp(b, a)" in {
      combine(List(plusPlus), List(xs, ys)).expressions must have size(6)
    }
  }
}