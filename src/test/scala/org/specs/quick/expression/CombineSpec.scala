package org.specs.quick.expression
import org.specs._
import org.specs.quick.methods._
import org.specs.quick.SampleLists
import org.scalacheck.Gen
import org.scalacheck.util._

class CombineSpec extends Specification with ScalaMethodsFactory with ExpressionsCombiner with SampleLists {
  "the combine method" should {
    "combine an expression taking a 2 variables and 1 variable in 2 expressions: a, exp(a, a)" in {
      combine(plusPlus, List(xs)).expressions.sortBy(_.toString.size).map(_.show).toString must_== 
    	  "List(xs, ++(xs, xs), ++(++(xs, xs), ++(xs, xs)))"
    }
    "combine 2 expressions and one variable" in {
      combineDepth(1)
      noDetailedDiffs()
      val methods = combineMethodList(List(plusPlus, nil), List(xs)).expressions.map(_.show)
      methods.distinct.sortBy(_.length).mkString("\n") must include(List(
          "xs",
    	  "nil()",
    	  "++(xs, xs)",
    	  "++(nil(), xs)",
    	  "++(xs, nil())",
    	  "++(nil(), nil())").mkString("\n"))
    }
    "combine an expression taking a 2 variables and 1 variables in 6 expressions: \n" + 
    "a, b, exp(a, b), exp(b, a), exp(a, a), exp(b, a)" in {
      combineDepth(1)
      combine(plusPlus, List(xs, ys)).expressions must have size(6)
    }
  }
  case class ValuedExpressionReturning(t: String) extends ValuedExpression {
    def getType = t
    def value = ()
    def substitute(bindings: Map[Expression, ValuedExpression]) = this
    def variables: List[VariableExpression[_]] = Nil
  }
  val exp1 = new ValuedExpressionReturning("scala.collection.immutable.List")
  "applicable parameters for a given method" should {
    "extract the same parameter types" in {
      MethodExpression(plusPlus).applicableParameters(exp1) must_== List(List(exp1, exp1))
    }
  }
  "A variable can be applied to a method if" >> {
	"the variable type is subtype of the method parameter type" >> {
      val variable = ValuedExpressionReturning("java.io.BufferedInputStream")
      class T { def method(s: java.io.InputStream) = () }
      val method: ScalaMethod = (new T).accept("method").get("method")
	  MethodExpression(method).applicableParameters(variable) must_== List(List(variable))
	}
  }
  "A method with no parameters" >> {
	"have no applicable parameters" >> {
      val variable = ValuedExpressionReturning("any")
      class T { def method() = () }
      val method: ScalaMethod = (new T).accept("method").get("method")
	  MethodExpression(method).applicableParameters(variable) must_== List(Nil)
	}  
  }
  "applying expressions to a given expression" should {
    "return a list of expressions" in {
      val plusPlusExpression = MethodExpression(plusPlus)
      plusPlusExpression.apply(List(exp1)) must_== List(ApplicationExpression(plusPlusExpression, List(exp1, exp1)))
    }
  }
}