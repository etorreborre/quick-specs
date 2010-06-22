package org.specs.quick.expression

/**
 * Application of a list of expressions to a Method
 * 
 * The type of an ApplicationExpression is the type of its Method
 * 
 * When it is evaluated, it may throw an exception, which is considered as its result
 */
case class ApplicationExpression(method: MethodExpression, parameters: List[ValuedExpression]) extends ValuedExpression {
  def getType = method.getType
  def value = {
    try {
      method.applyValues(parameters.map(_.value))
    } catch {
      case e => e
    }
  }
  override def show = method.show + parameters.map(_.show).mkString("(", ", ", ")")
  def substitute(bindings: Map[Expression, ValuedExpression]) = {
	 ApplicationExpression(method, parameters.map(_.substitute(bindings)))
  }
  def variables: List[VariableExpression[_]] = parameters.flatMap(_.variables)
}
