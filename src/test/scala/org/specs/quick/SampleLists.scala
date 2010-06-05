package org.specs.quick
import org.specs.quick.methods._
import org.scalacheck._
import org.scalacheck.util._

object Lists {
  def ++[T](l: List[T], l2: List[T]) = l ::: l2
  def nil[T]: List[T] = Nil
  override def toString = "Lists (" + hashCode + ")" 
}
trait SampleLists extends ScalaMethodsFactory {
  val plusPlus: ScalaMethod = Lists.find("++").get
  val nil: ScalaMethod = Lists.find("nil").get
  implicit val params = Gen.Params(2,StdRand)
  implicit val smallLists = Arbitrary(Gen.sized(size => Gen.listOfN(size, Gen.oneOf(1,2,3))))
  val xs = Variable[List[Int]]("xs")
  val ys = Variable[List[Int]]("ys")
}
