package org.specs.quick.expression
import org.specs.quick.methods._
import org.specs.quick.collection.CartesianProduct._

/**
 * Expression for a ScalaMethod
 * 
 * It can be applied a list of ValuedExpressions and result in a list of ApplicationExpressions where all possible
 * applications of some parameter combinations with the method are created.
 * 
 * It can be evaluated by passing actual values that should correspond to the parameters expected types (@see ScalaMethod) 
 */
case class MethodExpression(method: ScalaMethod) extends ApplicableExpression with TypesMatcher {
  lazy val getType = method.returnType
  lazy val methodName = method.methodName
  override def show = method.methodName
  private[this] def signature = methodName + method.parameterTypes.mkString("(", ",", ")") + ": " + getType

  def applyValues(values: List[Any]) = method.apply(values)
  def apply(expressions: ValuedExpression*): List[ApplicationExpression] = apply(expressions:_*)
  def apply(expressions: List[ValuedExpression]): List[ApplicationExpression] = {
    applicableParameters(expressions:_*) map (params => ApplicationExpression(this, params))
  }

  /**
   * for each variable we keep a list of all the expressions that have the same type
   */
  private[expression] def applicableParameters(expressions: ValuedExpression*): List[List[ValuedExpression]] = {
	val applicable: List[List[ValuedExpression]] = method.parameterTypes.map {	t => 
	  expressions.toList.filter(e => typesMatch(e.getType, t))
    }
	cartesianProduct(applicable).distinct
  }
}
