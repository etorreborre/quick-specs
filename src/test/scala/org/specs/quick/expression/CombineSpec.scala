package org.specs.quick.expression
import org.specs._
import org.specs.quick.methods._
import org.specs.quick.SampleLists
import org.scalacheck.Gen
import org.scalacheck.util._

class CombineSpec extends Specification with ScalaMethodsFactory with ExpressionsCombiner with SampleLists {
  "the combine method" should {
    "combine an expression taking a 2 variables and 1 variable in 2 expressions: a, exp(a, a)" in {
      combine(plusPlus, List(xs)).expressions.map(_.show).toString must_== "List(++(xs, xs), xs)"
    }
    "combine an expression taking a 2 variables and 1 variables in 6 expressions: \n" + 
    "a, b, exp(a, b), exp(b, a), exp(a, a), exp(b, a)" in {
      combine(plusPlus, List(xs, ys)).expressions must have size(6)
    }
  }
  val exp1 = new ValuedExpression { 
    def getType = "scala.collection.immutable.List"
    override def toString = getType
    def value = Nil
  }
  "applicable parameters for a given method" should {
    "extract the same parameter types" in {
      MethodExpression(plusPlus).applicableParameters(List(exp1)) must_== List(List(exp1, exp1))
    }
  }
  "applying expressions to a given expression" should {
    "return a list of expressions" in {
      val plusPlusExpression = MethodExpression(plusPlus)
      plusPlusExpression.apply(List(exp1)) must_== List(ApplicationExpression(plusPlusExpression, List(exp1, exp1)))
    }
  }
}