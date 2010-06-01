package org.specs.quick.prune

trait Curried
case class Apply(a: Any, b: Curried) extends Curried {
 override def toString = List(a, b).mkString(".(", ", ", ")") 
}
case class Curry(a: Any) extends Curried {
  override def toString = a.toString 
}
