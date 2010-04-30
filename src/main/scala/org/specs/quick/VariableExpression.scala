package org.specs.quick

case class VariableExpression[A](variable: Variable[A]) extends Expression {
  def getType = variable.getType
  override def toString = variable.show
  def evaluate = variable.value
}