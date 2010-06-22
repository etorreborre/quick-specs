package org.specs.quick.collection

/**
 * helper trait to create the cartesian product of several lists
 */
trait CartesianProduct {
  def cartesianProduct[T](list: List[List[T]]): List[List[T]] = {
    list match {
      case Nil => List(List())
      case xs :: xss => for (y <- xs; ys <- cartesianProduct(xss)) yield y :: ys
    }
  }
}
object CartesianProduct extends CartesianProduct