package org.specs.quick.prune

private[prune] trait Curried {
  def show = getClass.getSimpleName+"("+value+")"
  protected def value: String
}
private[prune] case class Apply(a: Curried, b: Curried) extends Curried {
 override def toString = List(a, b).mkString(".(", ", ", ")")
 protected def value = a.show +", "+ b.show
}
private[prune] case class Curry(a: Any) extends Curried {
  override def toString = a.toString
  override def hashCode = a.hashCode
  protected def value = a.toString
}
