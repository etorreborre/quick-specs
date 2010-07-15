package org.specs.quick.expression
import org.specs._
import org.specs.quick.methods._
import org.specs.quick._
import org.scalacheck.Gen
import org.scalacheck.util._

class CombineSpec extends Specification with ScalaMethodsFactory with ExpressionsCombiner with SampleLists with SampleVariables {
  "the combine method" should {
    "combine an expression taking a 2 variables and 1 variable in 2 expressions: a, exp(a, a)" in {
      noDetailedDiffs()
      combine(List(plusPlus), List(lists, xs)).expressions.sortBy(_.toString.size).map(_.show).toString must_== 
    	  ("List(xs, lists, lists.++(xs, xs), lists.++(lists.++(xs, xs), xs), "+
    	   "lists.++(xs, lists.++(xs, xs)), lists.++(lists.++(xs, xs), lists.++(xs, xs)))")
    }
    "combine 1 method and 2 variables" in {
      combineDepth(1)
      noDetailedDiffs()
      val methods = combineMethodList(List(plusPlus), List(lists, xs, ys)).expressions.map(_.show)
      methods.distinct.sortBy(_.length).mkString("\n") must include(List(
          "xs",
    	  "ys",
    	  "lists",
    	  "lists.++(xs, xs)",
    	  "lists.++(xs, ys)",
    	  "lists.++(ys, xs)",
    	  "lists.++(ys, ys)").mkString("\n"))
    }
    "combine 2 expressions and one variable" in {
      combineDepth(1)
      noDetailedDiffs()
      val methods = combineMethodList(List(plusPlus, nilList), List(lists, xs)).expressions.map(_.show)
      methods.distinct.sortBy(_.length).mkString("\n") must include(List(
          "xs",
          "lists",
    	  "lists.nil()",
    	  "lists.++(xs, xs)").mkString("\n"))
    }
    "combine an expression taking a 2 variables and 1 variables in 6 expressions: \n" + 
    "a, b, instance, instance.exp(a, b), instance.exp(b, a), instance.exp(a, a), instance.exp(b, a)" in {
      combineDepth(1)
      combine(List(plusPlus), List(lists, xs, ys)).expressions must have size(7)
    }
    "combine an expression taking a 2 variables and 1 variables with depth 2" in {
      combineDepth(2)
      combine(List(plusPlus), List(lists, xs, ys)).expressions must have size(39)
    }
  }
  case class ValuedExpressionReturning(t: String) extends ValuedExpression {
    def getType = t
    def value = ()
    def substitute(bindings: Map[Expression, ValuedExpression]) = this
    def variables: List[VariableExpression[_]] = Nil
  }
  val exp1 = new ValuedExpressionReturning("scala.collection.immutable.List")
  val listsExp = VariableExpression(lists)
  "applicable parameters for a given method" should {
    "extract the same parameter types" in {
      FunctionExpression(plusPlus).applicableParameters(listsExp, exp1) must_== List(List(listsExp, exp1, exp1))
    }
  }
  "A variable can be applied to a method if" >> {
	"the variable type is subtype of the method parameter type" >> {
      val variable = ValuedExpressionReturning("java.io.BufferedInputStream")
      class T { def method(s: java.io.InputStream) = () }
      val method: ScalaMethod = (new T).accept("method").get("method")
      val instance = VariableExpression(constant(new T))
	  FunctionExpression(method).applicableParameters(instance, variable) must_== List(List(instance, variable))
	}
  }
  "A method with no parameters" >> {
	"have only its instance as applicable parameters" >> {
      val variable = ValuedExpressionReturning("any")
      class T { def method() = () }
      val method: ScalaMethod = (new T).accept("method").get("method")
      val instance = VariableExpression(constant(new T))
	  FunctionExpression(method).applicableParameters(instance, variable) must_== List(List(instance))
	}  
  }
  "applying expressions to a given expression" should {
    "return a list of expressions" in {
      val plusPlusExpression = FunctionExpression(plusPlus)
      plusPlusExpression.apply(listsExp, exp1) must_== List(ApplicationExpression(plusPlusExpression, List(listsExp, exp1, exp1)))
    }
  }
}