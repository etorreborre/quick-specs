package org.specs.quick.expression
import org.specs.quick.methods._

/**
 * Expression representing a variable
 */
case class VariableExpression[A](variable: Variable[A]) extends ValuedExpression {
  lazy val getType = variable.getType
  def evaluate = variable.evaluate
  def value = variable.value
  override def show = variable.show
}