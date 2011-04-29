package org.specs2
package quick
package expression
import methods._
import quick.{SampleVariables, SampleLists}
import control.Property

class CombineSpec extends Specification { def is =

  "The combine method should"                                                                                           ^
    "combine an expression taking a 2 variables and 1 variable in 2 expressions: a, exp(a, a)"                          ! combine().e1^
    "combine 1 method and 2 variables"                                                                                  ! combine().e2^
    "combine 2 expressions and one variable"                                                                            ! combine().e3^
    "combine an expression taking a 2 variables and 1 variable in 6 expressions: \n" +
    "a, b, instance, instance.exp(a, b), instance.exp(b, a), instance.exp(a, a), instance.exp(b, a)"                    ! combine().e4^
    "combine an expression taking a 2 variables and 1 variables with depth 2"                                           ! combine().e5^
                                                                                                                        p^
  "Applicable parameters for a given method should"                                                                     ^
    "extract the same parameter types"                                                                                  ! app().e1^
                                                                                                                        p^
  "A variable can be applied to a method if"                                                                            ^
  	"the variable type is subtype of the method parameter type"                                                         ! app().e2^
                                                                                                                        p^
  "A method with no parameters"                                                                                         ^
	  "have only its instance as applicable parameters"                                                                   ! app().e3^
                                                                                                                        p^
  "applying expressions to a given expression should"                                                                   ^
    "return a list of expressions"                                                                                      ! app().e4^
                                                                                                                        end

  case class combine() extends ScalaMethodsFactory with ExpressionsCombiner with SampleLists with SampleVariables {
    def e1 = {
      combine(List(plusPlus), List(lists, xs)).expressions.sortBy(_.toString.size).map(_.show).toString must_== 
    	  ("List(xs, lists, lists.++(xs, xs), lists.++(lists.++(xs, xs), xs), "+
    	   "lists.++(xs, lists.++(xs, xs)), lists.++(lists.++(xs, xs), lists.++(xs, xs)))")
    }
    def e2 = {
      implicit val args = Args(combineDepth = 1)
      val methods = combineMethodList(List(plusPlus), List(lists, xs, ys)).expressions.map(_.show)
      methods.distinct.sortBy(_.length).mkString("\n") must contain(List(
        "xs",
    	  "ys",
    	  "lists",
    	  "lists.++(xs, xs)",
    	  "lists.++(xs, ys)",
    	  "lists.++(ys, xs)",
    	  "lists.++(ys, ys)").mkString("\n"))
    }
    def e3 = {
      implicit val args = Args(combineDepth = 1)
      val methods = combineMethodList(List(plusPlus, nilList), List(lists, xs)).expressions.map(_.show)
      methods.distinct.sortBy(_.length).mkString("\n") must contain(List(
          "xs",
          "lists",
    	  "lists.nil()",
    	  "lists.++(xs, xs)").mkString("\n"))
    }
    def e4 = {
      implicit val args = Args(combineDepth = 1)
      combine(List(plusPlus), List(lists, xs, ys)).expressions must have size(7)
    }
    def e5 = combine(List(plusPlus), List(lists, xs, ys)).expressions must have size(39)
  }

  case class app() extends ScalaMethodsFactory with ExpressionsCombiner with SampleLists with SampleVariables {
    val listsExp = VariableExpression(lists)

    def e1 = FunctionExpression(plusPlus).applicableParameters(listsExp, exp1) must_== List(List(listsExp, exp1, exp1))
    def e2 = {
      val variable = ValuedExpressionReturning("java.io.BufferedInputStream")
      class T { def method(s: java.io.InputStream) = () }
      val method: ScalaMethod = (new T).find("method").get
      val instance = VariableExpression(constant(new T))
	    FunctionExpression(method).applicableParameters(instance, variable) must_== List(List(instance, variable))
	  }
    def e3 = {
      val variable = ValuedExpressionReturning("any")
      class T { def method() = () }
      val method: ScalaMethod = (new T).find("method").get
      val instance = VariableExpression(constant(new T))
	    FunctionExpression(method).applicableParameters(instance, variable) must_== List(List(instance))
	  }  
    def e4 = {
      val plusPlusExpression = FunctionExpression(plusPlus)
      plusPlusExpression.apply(listsExp, exp1) must_== List(ApplicationExpression(plusPlusExpression, List(listsExp, exp1, exp1)))
    }
  }
  case class ValuedExpressionReturning(t: String) extends ValuedExpression {
    def getType = t
    def value = ()
    def substitute(bindings: Map[Expression, ValuedExpression]) = this
    def variables: List[VariableExpression[_]] = Nil
  }
  val exp1 = new ValuedExpressionReturning("scala.collection.immutable.List")

}