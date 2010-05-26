package org.specs.quick

case class ComposedExpression(main: MethodExpression, other: List[Expression]) extends Expression {
  def getType = main.getType
  override def toString = main.toString + other.mkString("(", ", ", ")")
  def evaluate = {
    try {
      main.applyValues(other.map(_.value))
    } catch {
      case e => e
    }
  }
}
