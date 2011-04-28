package org.specs2.quick
package expression

/**
 * Application of a list of expressions to a function
 * 
 * The type of an ApplicationExpression is the type of its function
 * 
 * When it is evaluated, it may throw an exception, which is considered as its result
 */
case class ApplicationExpression(function: FunctionExpression, parameters: Seq[ValuedExpression]) extends ValuedExpression {
  def getType = function.getType
  def value = {
    try {
      val r = function.applyValues(parameters.map(_.value))
      r
    } catch {
      case e => e
    }
  }
  override def show = function.show(parameters.map(_.show))
  override def toString = show
  def substitute(bindings: Map[Expression, ValuedExpression]) = {
	 ApplicationExpression(function, parameters.map(_.substitute(bindings)))
  }
  def variables: Seq[VariableExpression[_]] = parameters.flatMap(_.variables)
}
