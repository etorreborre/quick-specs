package org.specs.quick.expression
import org.specs.quick.methods._
import org.specs.quick.classify._
import org.specs.util.Property
/**
 * This trait takes methods and variables and combines them several times
 */
trait ExpressionsCombiner {
  val combine = (combineMethods _).tupled
  val combineDepth = Property(2)
  protected def combineMethods(methods: ScalaMethods, variables: List[Variable[_]]): CombinedExpressions = {
    combineMethodList(methods.methods, variables)
  }
  protected def combineMethodList(methods: List[ScalaMethod], variables: List[Variable[_]]): CombinedExpressions = {
    combineExpressions(methods.map(MethodExpression(_)), variables.map(VariableExpression(_)), variables)
  }
  private def combineExpressions(methods: List[ApplicableExpression], parameters: List[ValuedExpression], variables: List[Variable[_]]): CombinedExpressions = {
	def applyParams(params: List[ValuedExpression]) = methods.flatMap(_.apply(params))
	val zeroParamsMethods = applyParams(Nil) 
	val combined = (methods.flatMap(_.apply(zeroParamsMethods ::: parameters)) /: (1 until combineDepth())) { (res, cur) =>
  	  res ::: applyParams(res)
	}
    CombinedExpressions(combined ::: parameters, variables)
  }
}
case class CombinedExpressions(expressions: List[ValuedExpression], variables: List[Variable[_]])