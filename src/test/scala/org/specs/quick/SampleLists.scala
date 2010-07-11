package org.specs.quick
import org.specs.quick.methods._
import org.scalacheck._
import org.scalacheck.util._
import org.specs.specification.Tagged

object Lists {
  def +++[T](l: List[T], l2: List[T]) = {
	if (l isEmpty) l2
	else if (l2 isEmpty) l
	else (l ::: l2).sortBy(_.hashCode)
  }
  def ++[T](l: List[T], l2: List[T]) = l ::: l2
  def nil[T]: List[T] = Nil
  override def toString = "Lists (" + hashCode + ")" 
}
trait SampleLists extends ScalaMethodsFactory with Tagged {
  val listsName: ScalaMethod = Lists.find("toString").get
  val plusPlus: ScalaMethod = Lists.find("++").get
  val nilList: ScalaMethod = Lists.find("nil").get
  val plusPlusAndNil = Lists.accept("\\+\\+", "nil")
  val lists = constant("lists", Lists)
}
trait SampleVariables {
  implicit val parameters: Gen.Params = Gen.Params(2, StdRand)
  implicit val smallLists = Arbitrary(Gen.sized(size => Gen.listOfN(size, Gen.oneOf(1,2,3))))
  val xs = Variable[List[Int]]("xs")
  val ys = Variable[List[Int]]("ys")
}
