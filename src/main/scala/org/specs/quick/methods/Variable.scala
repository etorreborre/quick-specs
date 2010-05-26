package org.specs.quick
import org.scalacheck._
import scala.reflect.ClassManifest

trait Variable[A] {
  val name: String
  def getType: String
  def value: A
  def evaluate: A
  def show: String
  override def toString = show
}
object Variable {
  def apply[A](name: String)(implicit m: ClassManifest[A], arb: Arbitrary[A], params: Gen.Params) = new ArbitraryVariable(name)(m, arb, params)
}
class ArbitraryVariable[A](val name: String)(implicit m: ClassManifest[A], arb: Arbitrary[A], params: Gen.Params) extends Variable[A] {
  private var arbValue: A = evaluate
  def getType = m.erasure.getName
  def value: A = arbValue
  def evaluate = { arbValue = arb.arbitrary.apply(params).get; value }
  def show = name
}
object Constant {
  def apply[A](name: String)(v: A)(implicit m: ClassManifest[A]): Constant[A] = new Constant(name, v)(m)
}
class Constant[A](val name: String, v: A)(implicit m: ClassManifest[A]) extends Variable[A] {
  def getType = m.erasure.getName
  def value = evaluate
  def show = name
  def evaluate = v
  override def toString = name + " : " + v
}
