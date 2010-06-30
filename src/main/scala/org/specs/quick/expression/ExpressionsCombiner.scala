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
  protected def combineMethods(methods: ScalaMethods, variables: List[Variable[_]]): CombinedExpressions = {
    val combined = combineMethodList(methods.methods, variables)
    debug("combined " + combined)
    combined
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