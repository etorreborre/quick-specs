package org.specs.quick.expression
import org.specs.quick.methods._

case class VariableExpression[A](variable: Variable[A]) extends Expression {
  def getType = variable.getType
  override def toString = variable.show
  override def value = variable.value
  def evaluate = variable.evaluate
}