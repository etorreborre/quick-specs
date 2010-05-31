package org.specs.quick.expression
import org.specs._

class CombineSpec extends Specification with ExpressionsCombiner {
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