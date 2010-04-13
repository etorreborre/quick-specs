package org.specs.quick
import org.scalacheck._

case class Variable[A](name: String)(implicit m: scala.reflect.Manifest[A], arb: Arbitrary[A], params: Gen.Params) {
  def getType = m.erasure.getName
  def value = arb.arbitrary.apply(params)
  def show = name
}
