package org.specs.quick.expression
import org.specs.quick.methods._
import CartesianProduct._

/**
 * Expression for a ScalaMethod
 * 
 * when evaluated
 */
case class MethodExpression(method: ScalaMethod) extends ApplicableExpression {
  lazy val getType = method.returnType
  lazy val methodName = method.methodName
  override def show = method.methodName
  private def signature = methodName + method.parameterTypes.mkString("(", ",", ")") + ": " + getType

  def applyValues(values: List[Any]) = method.apply(values)
  def apply(expressions: List[ValuedExpression]) = {
    applicableParameters(expressions) map (params => ApplicationExpression(this, params))
  }

  /**
   * for each variable we keep a list of all the expressions that have the same type
   */
  def applicableParameters(expressions: List[ValuedExpression]): List[List[ValuedExpression]] = {
    cartesianProduct(method.parameterTypes.map(t => expressions.filter(e => e.getType == t)))
  }
}
