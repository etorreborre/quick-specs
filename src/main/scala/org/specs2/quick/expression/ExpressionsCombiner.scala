package org.specs2
package quick
package expression
import methods._
import classify._
import control.Property

/**
 * This trait takes methods and variables and combines them several times
 */
trait ExpressionsCombiner {
  val combine = (combineFunctions _).tupled
  val combineDepth = Property(2)

  protected def combineFunctions(functions: Seq[ScalaFunction], variables: Seq[Variable[_]]): CombinedExpressions = {
    println("functions to combine " + functions)
    val combined = combineMethodList(functions, variables)
    println("combined expressions are " + combined.expressions.mkString(", "))
    combined
  }
  protected def combineMethodList(functions: Seq[ScalaFunction], variables: Seq[Variable[_]]): CombinedExpressions = {
    combineExpressions(functions.map(FunctionExpression(_)), variables.map(VariableExpression(_)), variables)
  }
  private def combineExpressions(functions: Seq[ApplicableExpression], parameters: Seq[ValuedExpression], variables: Seq[Variable[_]]): CombinedExpressions = {
	  def applyParams(params: Seq[ValuedExpression]) = {
	    val distinctParams = params.distinct
	    val r = functions.flatMap(_.apply(distinctParams:_*))
	    println("trying to apply "+distinctParams+" to "+functions+" = "+r)
	    r
	  }
	  val zeroParamsMethods = applyParams(Nil)
	  val combined = (applyParams(zeroParamsMethods ++ parameters) /: (1 until combineDepth())) { (res, cur) =>
    	  res ++ applyParams(res ++ parameters)
	  }
    CombinedExpressions((combined ++ parameters).distinct, variables)
  }
}
case class CombinedExpressions(expressions: Seq[ValuedExpression], variables: Seq[Variable[_]])