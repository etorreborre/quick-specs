package org.specs.quick.collection

/**
 * helper trait to create the cartesian product of several Seqs
 */
trait CartesianProduct {
  def cartesianProduct[T](seq: Seq[Seq[T]]): Seq[Seq[T]] = {
    seq.toList match {
      case Nil => List(List())
      case xs :: xss => for (y <- xs; ys <- cartesianProduct(xss)) yield y +: ys  
    }
  }
}
object CartesianProduct extends CartesianProduct