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
  def apply(expressions: ValuedExpression*): List[ApplicationExpression] = apply(expressions:_*)
  def apply(expressions: List[ValuedExpression]): List[ApplicationExpression] = {
    applicableParameters(expressions:_*) map (params => ApplicationExpression(this, params))
  }

  /**
   * for each variable we keep a list of all the expressions that have the same type
   */
  def applicableParameters(expressions: ValuedExpression*): List[List[ValuedExpression]] = {
    cartesianProduct(method.parameterTypes.map(t => expressions.toList.filter(e => typesMatch(e.getType, t))))
  }
  /**
   * @return true if t2 is assignable with t1
   */
  private def typesMatch(t1: String, t2: String) = {
	val loader = getClass.getClassLoader
	loader.loadClass(t2).isAssignableFrom(loader.loadClass(t1))
  }
}
