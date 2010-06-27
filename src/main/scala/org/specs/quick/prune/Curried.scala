package org.specs.quick.prune

private[prune] trait Curried {
  def show = getClass.getSimpleName+"("+value+")"
  def value: String
}
private[prune] case class Apply(a: Curried, b: Curried) extends Curried {
 override def toString = List(a, b).mkString(".(", ", ", ")")
 def value = a.value +", "+ b.value
}
private[prune] case class Curry(a: Any) extends Curried {
  override def hashCode = a.hashCode
  def value = a.toString
}
