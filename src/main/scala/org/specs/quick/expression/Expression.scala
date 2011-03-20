package org.specs.quick.expression

/**
 * An expression can be:
 * * a variable
 * * a method
 * * the application of variables to a method
 * * the application of expressions to a method
 * 
 * It has a getType method which allows to determine if this expression can be used as
 * a parameter to a MethodExpression
 * 
 * It can be displayed in a human readable form with the 'show' method
 *
 */
trait Expression {
  def getType: String
  def show: String = getType
  override def toString = show
}
/**
 * An applicable expression can be:
 * * a method
 * * an application
 * 
 * It can be applied a list of expressions to create an ApplicationExpression
 *
 */
trait ApplicableExpression extends Expression {
  def apply(expressions: ValuedExpression*): Seq[ValuedExpression]
}
/**
 * A valued expression is an expression with a value:
 * * a variable
 * * or an application
 * 
 */
trait ValuedExpression extends Expression {
  def value: Any
  def substitute(bindings: Map[Expression, ValuedExpression]): ValuedExpression
  def variables: Seq[VariableExpression[_]]
}

