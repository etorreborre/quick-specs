package org.specs.quick.expression
import org.specs.quick.methods._

/**
 * Expression representing a variable
 */
case class VariableExpression[A](variable: Variable[A]) extends ValuedExpression {
  lazy val getType = variable.getType
  def evaluate = variable.evaluate
  def value = variable.value
  def name = variable.name
  override def show = variable.show
  def substitute(bindings: Map[Expression, ValuedExpression]) = bindings.get(this) match {
	case Some(a) => a
	case None => this
  }
  def variables: List[VariableExpression[_]] = List(this)

}