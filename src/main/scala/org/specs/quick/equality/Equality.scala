package org.specs.quick.equality

case class Equality[T](a: T, b: T) {
  def this(a: T) = this(a, a)
  override def toString = "[" + a.toString + " == " + b.toString + "]"
  def map[V](f: T => V): Equality[V] = Equality(f(a), f(b))
}

