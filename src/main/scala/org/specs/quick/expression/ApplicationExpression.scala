package org.specs.quick.expression

/**
 * Application of a list of expressions to a Method
 * 
 * The type of an ApplicationExpression is the type of its Method
 * 
 * When it is evaluated, it may throw an exception, which is considered as its result
 */
case class ApplicationExpression(method: FunctionExpression, parameters: Seq[ValuedExpression]) extends ValuedExpression {
  def getType = method.getType
  def value = {
    try {
      method.applyValues(parameters.map(_.value))
    } catch {
      case e => e
    }
  }
  override def show = parameters(0)+"."+method.show + parameters.drop(1).map(_.show).mkString("(", ", ", ")")
  def substitute(bindings: Map[Expression, ValuedExpression]) = {
	 ApplicationExpression(method, parameters.map(_.substitute(bindings)))
  }
  def variables: Seq[VariableExpression[_]] = parameters.flatMap(_.variables)
}
