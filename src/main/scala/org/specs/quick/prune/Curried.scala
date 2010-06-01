package org.specs.quick.prune

private[prune] trait Curried
private[prune] case class Apply(a: Any, b: Curried) extends Curried {
 override def toString = List(a, b).mkString(".(", ", ", ")") 
}
private[prune] case class Curry(a: Any) extends Curried {
  override def toString = a.toString 
}
