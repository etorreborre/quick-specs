package org.specs.quick
import org.scalacheck._

class Variable[A](name: String)(implicit m: scala.reflect.Manifest[A], arb: Arbitrary[A], params: Gen.Params) {
  def getType = m.erasure.getName
  override def toString = name
  def value = arb.arbitrary.apply(params)
}
