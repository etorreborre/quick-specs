package org.specs.quick.classify
import org.specs.quick.expression._

class Equality[T](a: T, b: T) {
  def this(a: T) = this(a, a)
  override def toString = "[" + a.toString + " == " + b.toString + "]" 
}
case class ExpressionEquality(a: Expression, b: Expression) extends Equality[Expression](a, b) {
  def this(a: Expression) = this(a, a)
}
trait Curryfiable {
  def curryfy: this.type
}
