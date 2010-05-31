package org.specs.quick.expression
import org.specs.quick.methods._
import org.specs.quick.classify._

/**
 * This trait takes methods and variables
 */
trait ExpressionsCombiner {
  val combine = (combineMethods _).tupled
  
  private def combineMethods(methods: ScalaMethods, variables: List[Variable[_]]): EquivalenceClass = {
    combineMethodList(methods.methods, variables)
  }
  private def combineMethodList(methods: List[ScalaMethod], variables: List[Variable[_]]): EquivalenceClass = {
    combineExpressions(methods.map(MethodExpression(_)), variables.map(VariableExpression(_)), variables)
  }
  private def combineExpressions(methods: List[ApplicableExpression], parameters: List[ValuedExpression], variables: List[Variable[_]]): EquivalenceClass = {
    new EquivalenceClass((methods flatMap (_.apply(parameters))) ::: parameters, variables)
  }
}
