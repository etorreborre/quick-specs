package org.specs.quick.equality
import org.specs.quick.expression._

case class Equality[+T](a: T, b: T) {
  def this(a: T) = this(a, a)
  override def toString = "[" + a.toString + " == " + b.toString + "]"
  def map[V](f: T => V): Equality[V] = Equality(f(a), f(b))
  def isTautology = a == b
}

