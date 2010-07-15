package org.specs.quick.expression
import org.specs.quick.methods._
import org.specs.quick.classify._
import org.specs.util.Property
import org.specs.log._
/**
 * This trait takes methods and variables and combines them several times
 */
trait ExpressionsCombiner extends Log {
  val combine = (combineMethods _).tupled
  val combineDepth = Property(2)
  
  protected def combineMethods(methods: Seq[ScalaFunction], variables: Seq[Variable[_]]): CombinedExpressions = {
    debug("methods to combine " + methods)
    val combined = combineMethodList(methods, variables)
    debug("combined expressions are " + combined.expressions.mkString(", "))
    combined
  }
  protected def combineMethodList(methods: Seq[ScalaFunction], variables: Seq[Variable[_]]): CombinedExpressions = {
    combineExpressions(methods.map(FunctionExpression(_)), variables.map(VariableExpression(_)), variables)
  }
  private def combineExpressions(methods: Seq[ApplicableExpression], parameters: Seq[ValuedExpression], variables: Seq[Variable[_]]): CombinedExpressions = {
	def applyParams(params: Seq[ValuedExpression]) = {
	  val distinctParams = params.distinct
	  val r = methods.flatMap(_.apply(distinctParams:_*))
	  debug("trying to apply "+distinctParams+" to "+methods+" = "+r)
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